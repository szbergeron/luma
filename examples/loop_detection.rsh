fn a(p1, p2) {
    if rand() {
        a(p1, p2)
    } else if rand() {
        "bar"
    } else {
        c(p1)
    }
}

fn b(p3) {
    thread a(p3, "foo")
}

fn c(p4) {
    if rand() {
        a(p4, "foo")
    } else {
        b("foo", "foo")
    }
}:

{
    b: Any
    c: default Bar

    VAR_FROM_WRAPPING_CONTEXT = g();

    b <- impl Bar {
        f() { return VAR_FROM_WRAPPING_CONTEXT }
    }

    d = [b, c]

    for val in d {
        val.f()
    }
}
