// Forbid assignment into a dynamically sized type.

//@ dont-require-annotations: NOTE

struct Fat<T: ?Sized> {
    f1: isize,
    f2: &'static str,
    ptr: T
}

#[derive(PartialEq,Eq)]
struct Bar;

#[derive(PartialEq,Eq)]
struct Bar1 {
    f: isize
}

trait ToBar {
    fn to_bar(&self) -> Bar;
    fn to_val(&self) -> isize;
}

impl ToBar for Bar1 {
    fn to_bar(&self) -> Bar {
        Bar
    }
    fn to_val(&self) -> isize {
        self.f
    }
}

pub fn main() {
    // Assignment.
    let f5: &mut Fat<dyn ToBar> = &mut Fat { f1: 5, f2: "some str", ptr: Bar1 {f :42} };
    let z: Box<dyn ToBar> = Box::new(Bar1 {f: 36});
    f5.ptr = Bar1 {f: 36};
    //~^ ERROR mismatched types
    //~| NOTE expected `dyn ToBar`, found `Bar1`
    //~| NOTE expected trait object `dyn ToBar`
    //~| NOTE found struct `Bar1`
    //~| ERROR the size for values of type
}
