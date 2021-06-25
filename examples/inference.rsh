pub struct S<A, B> {
    // e_a XNOR e_b must be parameterized, can't only have one or other
    e_a: A<i32>,
    e_b: B,
    e_c: A<B>,
}

pub struct T<C, D> {
    m: S<C, i32>,
    d: D,
}

pub struct U {
    m: T<String, i64>,
}

pub fn main() {
    let a = S();

    a.e_a = Vec<i32>,
    a.e_b = //
}

// just as notes and thinking to myself,

if we have a call chain:

    let a = b.foo(c).bar(d, e).baz()

this desugars to

    B::foo(B, C) -> F
    F::bar(F, D, E) -> G
    G::baz(G) -> H

    let a: H = G::baz(F::bar(B::foo(b, c), d, e))

So method searches are actually not done on the type itself, they aren't *members* as far as the type system is concerned,
and if their type is known at runtime (not dyn types), then they are actually completely devirtualized.


