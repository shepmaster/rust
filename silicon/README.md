Clone libc into `silicon/lib`, checkout the silicon branch

# Build a compiler capable of cross-compiling to the DTK

The current Rust compiler doesn't know how to target the DTK. We need
to build an enhanced version that can.

This section can be removed when an appropriate target specification
is added to the standard Rust bootstrapping compiler.

1. `cd silicon/bootstrap`

1. Build our bootstrapping compiler
  ```
  CFLAGS_aarch64_apple_darwin='-arch arm64' \
  SDKROOT=/Users/shep/Downloads/Xcode-beta.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX11.0.sdk \
  MACOSX_DEPLOYMENT_TARGET=11.5 \
  ../../x.py build -i --stage 1 --warnings warn src/libstd
  ```

  N.B. these environment variables *probably* aren't required, but
  they are for consistency with the next step.

# Cross-compile the compiler for the DTK

Now that we have a compiler that knows about the DTK target, we can
build a compiler native to the DTK.

1. Modify the absolute paths in `silicon/cross/config.toml` in the
   `cargo`, `rustc`, and `rustfmt` keys to point to your checkout.

1. `cd silicon/cross`

1. Cross-compile the compiler

  ```
  CFLAGS_aarch64_apple_darwin='-arch arm64' \
  SDKROOT=/Users/shep/Downloads/Xcode-beta.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX11.0.sdk \
  MACOSX_DEPLOYMENT_TARGET=11.5 \
  ../../x.py build -i --stage 1 --warnings warn src/libstd
  ```

# Copy the cross-compiler to the DTK

`rsync` is a good choice, as you'll probably need to iterate a few times!

```
rsync -avz silicon/cross/build/aarch64-apple-darwin/stage1 dtk:
```

One strange thing is that the the standard libary files aren't in the
right place yet. You can copy them from the sibling x86_64 build
though:

```
rsync -avz build/x86_64-apple-darwin/stage1/lib/rustlib/aarch64-apple-darwin/ dtk:stage1/lib/rustlib/aarch64-apple-darwin/
```

# Build a native binary

On the DTK, you can now use native Rust:

```
% echo 'fn main() { println!("Hello, DTK!") }' > hello.rs
% ./stage1/bin/rustc hello.rs
% ./hello
Hello, DTK!
```

# Miscellaneous notes

- You'll need to have installed the Xcode beta (or newer, perhaps) in
  order to have the appropriate libraries and toolchains to
  cross-compile.

- `CFLAGS_aarch64_apple_darwin=` - used to inform the `cc` crate how
  to cross-compile native objects like libbacktrace and
  compiler-builtins

- `SDKROOT` - used to indicate to `rustc` what SDK whould be
  used. Point this at the `MacOSX11` target inside your `Xcode.app` /
  `Xcode-beta.app`

- `MACOSX_DEPLOYMENT_TARGET` - used to indicate to `rustc` what
  version of macOS to target.

- We build LLVM *3 times* and use incremental builds for faster
  iteration times. This can easily take up **30 GiB** of
  space. Building can take several hours on an 2.9 GHz 6-Core Intel
  Core i9.
