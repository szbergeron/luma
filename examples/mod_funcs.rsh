use m1::m2::*;
use m1::m2;

mod m1 {
    mod m2 {
        use super::m3 as m7;
    }

    mod m3 {
        mod m5 {
            use global::m4::*;
        }
    }
}

mod flat {
    fn add(a: i32, b: i32) -> i32 {
        a + b
    }

    fn add_intrinsic(a: i32, b: i32) -> i32
    #builtin
    #bind a => a
    #bind b => b
    #var c
    llvm hello llvm
    //{{c}} = add i32 {{a}}, 

    struct a {
        b: T,
        c: U,
    }
}

mod m4 {
    mod m6 {
        fn add(a: i32, b: i32) -> i32 {
            a + b
        }
    }
}
