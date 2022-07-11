struct Foo {
    //bar: i32
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

sdfjkl

fn main() {
    let f = Foo { 1i32 };

    f <- Bar {
        fn a() -> () {
            print("Changed at runtime!");
        }
    };

    f.a(); // > "Changed at runtime!"

    let handle = thread::init(f.a);

    f <- Bar.a() -> () {
        print("Changed another time, this time just the one function")
    }

    f.a(); // will print "Changed another time..."

    handle.start(); // *may print* "Changed at runtime"
}
