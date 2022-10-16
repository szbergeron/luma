struct Foo {
    var bar: i32
}

struct Foo {
}

impl Bar for Foo {
    fn a() -> () {
        print("original");
    }
}

trait Bar {
    fn a() -> () {
        print("from trait");
    }
}

fn main() -> () {
}
