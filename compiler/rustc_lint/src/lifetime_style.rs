use std::slice;

use rustc_data_structures::fx::FxIndexMap;
use rustc_hir::intravisit::{self, Visitor};
use rustc_hir::{self as hir, LifetimeSource, LifetimeSyntax};
use rustc_session::lint::Lint;
use rustc_session::{declare_lint, declare_lint_pass, impl_lint_pass};
use rustc_span::Span;
use tracing::instrument;

use crate::{LateContext, LateLintPass, LintContext, lints};

declare_lint! {
    /// The `mismatched_lifetime_syntaxes` lint detects when an
    /// elided lifetime uses different syntax between function
    /// arguments and return values.
    ///
    /// The three kinds of syntax are:
    /// 1. Explicitly bound lifetimes, such as `'static` or `'a`.
    /// 2. The explicitly anonymous lifetime `'_`.
    /// 3. Implicit lifetimes, such as `&u8` or `ThisHasALifetimeGeneric`.
    ///
    /// As an exception, this lint allows references with implicit or
    /// explicitly anonymous lifetimes to be paired with paths using
    /// explicitly anonymous lifetimes.
    ///
    /// ### Example
    ///
    /// ```rust,compile_fail
    /// #![deny(mismatched_lifetime_syntaxes)]
    ///
    /// pub fn mixing_explicit_bound_with_implicit(v: &'static u8) -> &u8 {
    ///     v
    /// }
    ///
    /// struct Person<'a> {
    ///     name: &'a str,
    /// }
    ///
    /// pub fn mixing_implicit_with_explicit_anonymous(v: Person) -> Person<'_> {
    ///     v
    /// }
    ///
    /// struct Foo;
    ///
    /// impl Foo {
    ///     // Lifetime elision results in the output lifetime becoming
    ///     // `'static`, which is not what was intended.
    ///     pub fn get_mut(&'static self, x: &mut u8) -> &mut u8 {
    ///         unsafe { &mut *(x as *mut _) }
    ///     }
    /// }
    /// ```
    ///
    /// {{produces}}
    ///
    /// ### Explanation
    ///
    /// Lifetime elision is useful because it frees you from having to
    /// give each lifetime its own name and show the relation of input
    /// and output lifetimes for common cases. However, a lifetime
    /// that uses inconsistent syntax between related arguments and
    /// return values is more confusing.
    ///
    /// In certain `unsafe` code, lifetime elision combined with
    /// inconsistent lifetime syntax may result in unsound code.
    pub MISMATCHED_LIFETIME_SYNTAXES,
    Warn,
    "detects when an elided lifetime uses different syntax between arguments and return values"
}

declare_lint! {
    /// The `hidden_lifetimes_in_input_paths` lint detects the use of
    /// hidden lifetime parameters in types occurring as a function
    /// argument.
    ///
    /// ### Example
    ///
    /// ```rust,compile_fail
    /// struct ContainsLifetime<'a>(&'a i32);
    ///
    /// #[deny(hidden_lifetimes_in_input_paths)]
    /// fn foo(x: ContainsLifetime) {}
    /// ```
    ///
    /// {{produces}}
    ///
    /// ### Explanation
    ///
    /// Hidden lifetime parameters can make it difficult to see at a
    /// glance that borrowing is occurring.
    ///
    /// This lint ensures that lifetime parameters are always
    /// explicitly stated, even if it is the `'_` [placeholder
    /// lifetime].
    ///
    /// This lint is "allow" by default as function arguments by
    /// themselves do not usually cause much confusion.
    ///
    /// [placeholder lifetime]: https://doc.rust-lang.org/reference/lifetime-elision.html#lifetime-elision-in-functions
    pub HIDDEN_LIFETIMES_IN_INPUT_PATHS,
    Allow,
    "hidden lifetime parameters in types in function arguments may be confusing"
}

declare_lint! {
    /// The `hidden_lifetimes_in_output_paths` lint detects the use
    /// of hidden lifetime parameters in types occurring as a function
    /// return value.
    ///
    /// ### Example
    ///
    /// ```rust,compile_fail
    /// struct ContainsLifetime<'a>(&'a i32);
    ///
    /// #[deny(hidden_lifetimes_in_output_paths)]
    /// fn foo(x: &i32) -> ContainsLifetime {
    ///     ContainsLifetime(x)
    /// }
    /// ```
    ///
    /// {{produces}}
    ///
    /// ### Explanation
    ///
    /// Hidden lifetime parameters can make it difficult to see at a
    /// glance that borrowing is occurring. This is especially true
    /// when a type is used as a function's return value: lifetime
    /// elision will link the return value's lifetime to an argument's
    /// lifetime, but no syntax in the function signature indicates
    /// that.
    ///
    /// This lint ensures that lifetime parameters are always
    /// explicitly stated, even if it is the `'_` [placeholder
    /// lifetime].
    ///
    /// [placeholder lifetime]: https://doc.rust-lang.org/reference/lifetime-elision.html#lifetime-elision-in-functions
    pub HIDDEN_LIFETIMES_IN_OUTPUT_PATHS,
    Warn,
    "hidden lifetime parameters in types in function return values are deprecated"
}

declare_lint_pass!(LifetimeStyle => [
    MISMATCHED_LIFETIME_SYNTAXES,
    HIDDEN_LIFETIMES_IN_INPUT_PATHS,
    HIDDEN_LIFETIMES_IN_OUTPUT_PATHS,
]);

impl<'tcx> LateLintPass<'tcx> for LifetimeStyle {
    #[instrument(skip_all)]
    fn check_fn(
        &mut self,
        cx: &LateContext<'tcx>,
        _: hir::intravisit::FnKind<'tcx>,
        fd: &'tcx hir::FnDecl<'tcx>,
        _: &'tcx hir::Body<'tcx>,
        _: rustc_span::Span,
        _: rustc_span::def_id::LocalDefId,
    ) {
        let mut input_map = Default::default();
        let mut output_map = Default::default();

        for input in fd.inputs {
            LifetimeInfoCollector::collect(input, &mut input_map);
        }

        if let hir::FnRetTy::Return(output) = fd.output {
            LifetimeInfoCollector::collect(output, &mut output_map);
        }

        report_mismatches(cx, &input_map, &output_map);
        report_hidden_in_paths(cx, &input_map, HIDDEN_LIFETIMES_IN_INPUT_PATHS);
        report_hidden_in_paths(cx, &output_map, HIDDEN_LIFETIMES_IN_OUTPUT_PATHS);
    }
}

#[instrument(skip_all)]
fn report_mismatches<'tcx>(
    cx: &LateContext<'tcx>,
    inputs: &LifetimeInfoMap<'tcx>,
    outputs: &LifetimeInfoMap<'tcx>,
) {
    for (resolved_lifetime, output_info) in outputs {
        if let Some(input_info) = inputs.get(resolved_lifetime) {
            let relevant_lifetimes = input_info.iter().chain(output_info);

            // There can only ever be zero or one bound lifetime
            // for a given lifetime resolution.
            let mut bound_lifetime = None;

            // Categorize lifetimes into source/syntax buckets
            let mut n_hidden = 0;
            let mut n_elided = 0;
            let mut n_named = 0;

            // These are the lifetime instances that are allowed to
            // change to a given alternate style.
            let mut suggest_change_to_implicit = Vec::new();
            let mut suggest_change_to_mixed_implicit = Vec::new();
            let mut suggest_change_to_mixed_explicit_anonymous = Vec::new();
            let mut suggest_change_to_explicit_anonymous = Vec::new();
            let mut suggest_change_to_explicit_bound = Vec::new();

            // Some styles prevent using implicit syntax at all.
            let mut allow_suggesting_implicit = true;

            for info in relevant_lifetimes {
                use LifetimeSource::*;
                //                use LifetimeSyntax::*;
                use X::*;

                enum X { Implicit, ExplicitBound, ExplicitAnonymous }

                let x = match info.lifetime.syntax {
                    LifetimeSyntax::Implicit => Implicit,
                    _ => if info.lifetime.is_anonymous() { ExplicitAnonymous } else { ExplicitBound }
                };

                let syntax_source = (x, info.lifetime.source);

                if let (ExplicitBound, _) = syntax_source {
                    bound_lifetime = Some(info);
                }

                match syntax_source {
                    // Ignore any other kind of lifetime
                    (_, Other) => continue,

                    // e.g. `&T`
                    (Implicit, Reference | OutlivesBound | PreciseCapturing) |
                    // e.g. `&'_ T`
                    (ExplicitAnonymous, Reference | OutlivesBound | PreciseCapturing) |
                    // e.g. `ContainsLifetime<'_>`
                    (ExplicitAnonymous, Path { .. }) => n_elided += 1,

                    // e.g. `ContainsLifetime`
                    (Implicit, Path { .. }) => n_hidden += 1,

                    // e.g. `&'a T`
                    (ExplicitBound, Reference | OutlivesBound | PreciseCapturing) |
                    // e.g. `ContainsLifetime<'a>`
                    (ExplicitBound, Path { .. }) => n_named += 1,
                };

                match syntax_source {
                    // e.g. `&T`
                    (Implicit, Reference) => {
                        suggest_change_to_explicit_anonymous.push(info);
                        suggest_change_to_explicit_bound.push(info);
                    }

                    // e.g. `&'_ T`
                    (ExplicitAnonymous, Reference) => {
                        suggest_change_to_implicit.push(info);
                        suggest_change_to_mixed_implicit.push(info);
                        suggest_change_to_explicit_bound.push(info);
                    }

                    // e.g. `ContainsLifetime`
                    (Implicit, Path { .. }) => {
                        suggest_change_to_mixed_explicit_anonymous.push(info);
                        suggest_change_to_explicit_anonymous.push(info);
                        suggest_change_to_explicit_bound.push(info);
                    }

                    // e.g. `ContainsLifetime<'_>`
                    (ExplicitAnonymous, Path { .. }) => {
                        suggest_change_to_explicit_bound.push(info);
                    }

                    // e.g. `&'a T`
                    (ExplicitBound, Reference) => {
                        suggest_change_to_implicit.push(info);
                        suggest_change_to_mixed_implicit.push(info);
                        suggest_change_to_explicit_anonymous.push(info);
                    }

                    // e.g. `ContainsLifetime<'a>`
                    (ExplicitBound, Path { .. }) => {
                        suggest_change_to_mixed_explicit_anonymous.push(info);
                        suggest_change_to_explicit_anonymous.push(info);
                    }

                    (Implicit, OutlivesBound | PreciseCapturing) => {
                        panic!("This syntax / source combination is not possible");
                    }

                    // e.g. `+ '_`, `+ use<'_>`
                    (ExplicitAnonymous, OutlivesBound | PreciseCapturing) => {
                        suggest_change_to_explicit_bound.push(info);
                    }

                    // e.g. `+ 'a`, `+ use<'a>`
                    (ExplicitBound, OutlivesBound | PreciseCapturing) => {
                        suggest_change_to_mixed_explicit_anonymous.push(info);
                        suggest_change_to_explicit_anonymous.push(info);
                    }

                    (_, Other) => {
                        panic!("This syntax / source combination has already been skipped");
                    }
                }

                if matches!(syntax_source, (_, Path { .. } | OutlivesBound | PreciseCapturing)) {
                    allow_suggesting_implicit = false;
                }
            }

            // Check if syntaxes are consistent
            let syntax_counts = (n_hidden, n_elided, n_named);
            tracing::debug!(?syntax_counts);

            if let (_, 0, 0) | (0, _, 0) | (0, 0, _) = syntax_counts {
                // The syntax is consistent, nothing to report.
                continue;
            }

            let make_implicit_suggestions =
                |infos: &[&Info<'_>]| infos.iter().map(|i| i.hiding_span()).collect::<Vec<_>>();

            let inputs = input_info.iter().map(|info| info.reporting_span()).collect();
            let outputs = output_info.iter().map(|info| info.reporting_span()).collect();

            let explicit_bound_suggestion = bound_lifetime.and_then(|info| {
                let name = info.lifetime_name()?;
                Some(build_mismatch_suggestion(name, &suggest_change_to_explicit_bound))
            });

            let is_bound_static = bound_lifetime.is_some_and(|info| info.is_static());

            tracing::debug!(?bound_lifetime, ?explicit_bound_suggestion, ?is_bound_static);

            let should_suggest_mixed =
                // Is there anything to change?
                !suggest_change_to_mixed_implicit.is_empty() &&
                !suggest_change_to_mixed_explicit_anonymous.is_empty() &&
                // If we have `'static`, we don't want to remove it
                !is_bound_static;

            let mixed_suggestion = should_suggest_mixed.then(|| {
                let implicit_suggestions =
                    make_implicit_suggestions(&suggest_change_to_mixed_implicit);

                let explicit_anonymous_suggestions = suggest_change_to_mixed_explicit_anonymous
                    .iter()
                    .map(|info| info.suggestion("'_"))
                    .collect();

                lints::MismatchedLifetimeSyntaxesSuggestion::Mixed {
                    implicit_suggestions,
                    explicit_anonymous_suggestions,
                    tool_only: false,
                }
            });

            tracing::debug!(
                ?suggest_change_to_mixed_implicit,
                ?suggest_change_to_mixed_explicit_anonymous,
                ?mixed_suggestion,
            );

            let should_suggest_implicit =
                // Is there anything to change?
                !suggest_change_to_implicit.is_empty() &&
                // We never want to hide the lifetime in a path (or similar)
                allow_suggesting_implicit &&
                // If we have `'static`, we don't want to remove it
                !is_bound_static;

            let implicit_suggestion = should_suggest_implicit.then(|| {
                let suggestions = make_implicit_suggestions(&suggest_change_to_implicit);

                lints::MismatchedLifetimeSyntaxesSuggestion::Implicit {
                    suggestions,
                    tool_only: false,
                }
            });

            tracing::debug!(
                ?should_suggest_implicit,
                ?suggest_change_to_implicit,
                allow_suggesting_implicit,
                ?implicit_suggestion,
            );

            let should_suggest_explicit_anonymous =
                // Is there anything to change?
                !suggest_change_to_explicit_anonymous.is_empty() &&
                // If we have `'static`, we don't want to remove it
                !is_bound_static;

            let explicit_anonymous_suggestion = should_suggest_explicit_anonymous
                .then(|| build_mismatch_suggestion("'_", &suggest_change_to_explicit_anonymous));

            tracing::debug!(
                ?should_suggest_explicit_anonymous,
                ?suggest_change_to_explicit_anonymous,
                ?explicit_anonymous_suggestion,
            );

            let lifetime_name =
                bound_lifetime.and_then(|info| info.lifetime_name()).unwrap_or("'_").to_owned();

            // We can produce a number of suggestions which may overwhelm
            // the user. Instead, we order the suggestions based on Rust
            // idioms. The "best" choice is shown to the user and the
            // remaining choices are shown to tools only.
            let mut suggestions = Vec::new();
            suggestions.extend(explicit_bound_suggestion);
            suggestions.extend(mixed_suggestion);
            suggestions.extend(implicit_suggestion);
            suggestions.extend(explicit_anonymous_suggestion);

            cx.emit_span_lint(
                MISMATCHED_LIFETIME_SYNTAXES,
                Vec::clone(&inputs),
                lints::MismatchedLifetimeSyntaxes { lifetime_name, inputs, outputs, suggestions },
            );
        }
    }
}

fn build_mismatch_suggestion(
    lifetime_name: &str,
    infos: &[&Info<'_>],
) -> lints::MismatchedLifetimeSyntaxesSuggestion {
    let lifetime_name = lifetime_name.to_owned();

    let suggestions = infos.iter().map(|info| info.suggestion(&lifetime_name)).collect();

    lints::MismatchedLifetimeSyntaxesSuggestion::Explicit {
        lifetime_name,
        suggestions,
        tool_only: false,
    }
}

fn report_hidden_in_paths<'tcx>(
    cx: &LateContext<'tcx>,
    info_map: &LifetimeInfoMap<'tcx>,
    lint: &'static Lint,
) {
    let relevant_lifetimes = info_map
        .iter()
        .filter(|&(&&res, _)| reportable_lifetime_resolution(res))
        .flat_map(|(_, info)| info)
        .filter(|info| {
            matches!(info.lifetime.source, LifetimeSource::Path { .. })
                && info.lifetime.is_implicit()
        });

    let mut reporting_spans = Vec::new();
    let mut suggestions = Vec::new();

    for info in relevant_lifetimes {
        reporting_spans.push(info.reporting_span());
        suggestions.push(info.suggestion("'_"));
    }

    if reporting_spans.is_empty() {
        return;
    }

    cx.emit_span_lint(
        lint,
        reporting_spans,
        lints::HiddenLifetimeInPath {
            suggestions: lints::HiddenLifetimeInPathSuggestion { suggestions },
        },
    );
}

/// We don't care about errors, nor do we care about the lifetime
/// inside of a trait object.
fn reportable_lifetime_resolution(kind: hir::LifetimeKind) -> bool {
    matches!(
        kind,
        hir::LifetimeKind::Param(..) | hir::LifetimeKind::Infer | hir::LifetimeKind::Static
    )
}

#[derive(Debug)]
struct Info<'tcx> {
    type_span: Span,
    referenced_type_span: Option<Span>,
    lifetime: &'tcx hir::Lifetime,
}

impl<'tcx> Info<'tcx> {
    fn lifetime_name(&self) -> Option<&str> {
        match &self.lifetime.syntax {
            LifetimeSyntax::Implicit => None,
            LifetimeSyntax::Explicit(e) => Some(e.as_str())
        }
    }

    fn is_static(&self) -> bool {
        self.lifetime.is_static()
    }

    /// When reporting a lifetime that is implicit, we expand the span
    /// to include the type. Otherwise we end up pointing at nothing,
    /// which is a bit confusing.
    fn reporting_span(&self) -> Span {
        if self.lifetime.is_implicit() { self.type_span } else { self.lifetime.span() }
    }

    /// When hiding a lifetime in a reference, we want to remove the
    /// whitespace after the lifetime.
    ///
    /// ```rust
    /// fn x(a: &'_ u8) {}
    /// ```
    ///
    /// Should become
    ///
    /// ```rust
    /// fn x(a: &u8) {}
    /// ```
    // FIXME: Ideally, we'd also remove the lifetime declaration.
    fn hiding_span(&self) -> Span {
        let mut span = self.suggestion("'dummy").0;

        if let Some(referenced_type_span) = self.referenced_type_span {
            span = span.until(referenced_type_span);
        }

        span
    }

    fn suggestion(&self, lifetime_name: &str) -> (Span, String) {
        self.lifetime.suggestion(lifetime_name)
    }
}

type LifetimeInfoMap<'tcx> = FxIndexMap<&'tcx hir::LifetimeKind, Vec<Info<'tcx>>>;

struct LifetimeInfoCollector<'a, 'tcx> {
    type_span: Span,
    referenced_type_span: Option<Span>,
    map: &'a mut LifetimeInfoMap<'tcx>,
}

impl<'a, 'tcx> LifetimeInfoCollector<'a, 'tcx> {
    fn collect(ty: &'tcx hir::Ty<'tcx>, map: &'a mut LifetimeInfoMap<'tcx>) {
        let mut this = Self { type_span: ty.span, referenced_type_span: None, map };

        intravisit::walk_unambig_ty(&mut this, ty);
    }
}

impl<'a, 'tcx> Visitor<'tcx> for LifetimeInfoCollector<'a, 'tcx> {
    #[instrument(skip(self))]
    fn visit_lifetime(&mut self, lifetime: &'tcx hir::Lifetime) {
        let type_span = self.type_span;
        let referenced_type_span = self.referenced_type_span;

        let info = Info { type_span, referenced_type_span, lifetime };

        self.map.entry(&lifetime.kind).or_default().push(info);
    }

    #[instrument(skip(self))]
    fn visit_ty(&mut self, ty: &'tcx hir::Ty<'tcx, hir::AmbigArg>) -> Self::Result {
        let old_type_span = self.type_span;
        let old_referenced_type_span = self.referenced_type_span;

        self.type_span = ty.span;
        if let hir::TyKind::Ref(_, ty) = ty.kind {
            self.referenced_type_span = Some(ty.ty.span);
        }

        intravisit::walk_ty(self, ty);

        self.type_span = old_type_span;
        self.referenced_type_span = old_referenced_type_span;
    }
}

declare_lint! {
    /// The `hidden_lifetimes_in_type_paths` lint detects the use of
    /// hidden lifetime parameters in types not part of a function's
    /// arguments or return values.
    ///
    /// ### Example
    ///
    /// ```rust,compile_fail
    /// struct ContainsLifetime<'a>(&'a i32);
    ///
    /// #[deny(hidden_lifetimes_in_type_paths)]
    /// static FOO: ContainsLifetime = ContainsLifetime(&42);
    /// ```
    ///
    /// {{produces}}
    ///
    /// ### Explanation
    ///
    /// Hidden lifetime parameters can make it difficult to see at a
    /// glance that borrowing is occurring.
    ///
    /// This lint ensures that lifetime parameters are always
    /// explicitly stated, even if it is the `'_` [placeholder
    /// lifetime].
    ///
    /// [placeholder lifetime]: https://doc.rust-lang.org/reference/lifetime-elision.html#lifetime-elision-in-functions
    pub HIDDEN_LIFETIMES_IN_TYPE_PATHS,
    Allow,
    "hidden lifetime parameters in types outside function signatures are discouraged"
}

#[derive(Default)]
pub(crate) struct HiddenLifetimesInTypePaths {
    inside_fn_signature: bool,
}

impl_lint_pass!(HiddenLifetimesInTypePaths => [HIDDEN_LIFETIMES_IN_TYPE_PATHS]);

impl<'tcx> LateLintPass<'tcx> for HiddenLifetimesInTypePaths {
    #[instrument(skip(self, cx))]
    fn check_ty(&mut self, cx: &LateContext<'tcx>, ty: &'tcx hir::Ty<'tcx, hir::AmbigArg>) {
        if self.inside_fn_signature {
            return;
        }

        // Do not lint about usages like `ContainsLifetime::method` or
        // `ContainsLifetimeAndType::<SomeType>::method`.
        if ty.source == hir::TySource::ImplicitSelf {
            return;
        }

        let hir::TyKind::Path(path) = ty.kind else { return };

        let path_segments = match path {
            hir::QPath::Resolved(_ty, path) => path.segments,

            hir::QPath::TypeRelative(_ty, path_segment) => slice::from_ref(path_segment),

            hir::QPath::LangItem(..) => &[],
        };

        let mut suggestions = Vec::new();

        for path_segment in path_segments {
            for arg in path_segment.args().args {
                if let hir::GenericArg::Lifetime(lifetime) = arg
                    && lifetime.is_implicit()
                    && reportable_lifetime_resolution(lifetime.kind)
                {
                    suggestions.push(lifetime.suggestion("'_"))
                }
            }
        }

        if suggestions.is_empty() {
            return;
        }

        cx.emit_span_lint(
            HIDDEN_LIFETIMES_IN_TYPE_PATHS,
            ty.span,
            lints::HiddenLifetimeInPath {
                suggestions: lints::HiddenLifetimeInPathSuggestion { suggestions },
            },
        );
    }

    #[instrument(skip_all)]
    fn check_fn(
        &mut self,
        _: &LateContext<'tcx>,
        _: hir::intravisit::FnKind<'tcx>,
        _: &'tcx hir::FnDecl<'tcx>,
        _: &'tcx hir::Body<'tcx>,
        _: rustc_span::Span,
        _: rustc_span::def_id::LocalDefId,
    ) {
        // We make the assumption that we will visit the function
        // declaration first, before visiting the body.
        self.inside_fn_signature = true;
    }

    // This may be a function's body, which would indicate that we are
    // no longer in the signature. Even if it's not, a body cannot
    // occur inside a function signature.
    #[instrument(skip_all)]
    fn check_body(&mut self, _: &LateContext<'tcx>, _: &hir::Body<'tcx>) {
        self.inside_fn_signature = false;
    }
}
