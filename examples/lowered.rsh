struct Foo {
    field bar i32;
}

trait Bar {
    function f input Unit output Unit {
        bind t0 "hello world";
        bind t1 construct Tuple<str> { 0: t0 };
        call print with t1 into t2;
    }
}
