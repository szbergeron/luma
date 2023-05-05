# Luma
is designed to provide the prototyping flexibility of Python and JavaScript, while being statically compiled to efficient, performant, code.

Luma makes use of a hybrid two-phase type inference engine to allow users to write code without thinking about what shape their types have.

Does a type not have a field you need? Go ahead and use the field anyway, if the inference engine can figure out what type the field is
then you never have to specify what type it is in the struct/class definition.

```rust

struct Body {
    var x: std::f64;
    var y: std::f64;
};

fn generate_body(x: std::f64, y: std::f64, mass: std::f64) -> Body {
    let b = struct Body {};

    b.x = x;
    b.y = y;
    b.mass = mass; // mass is an f64, so the emitted struct
                   // for `Body` will have a `mass: f64` member!

    b.v_x = x.cos();
    b.v_y = y.cos();

    b
};
```

If you're looking to hack on the "new bits" that come with Luma, or just want to see how the core of the inferencing engine works,
take a look at `src/mir/quark.rs`, `src/mir/instance.rs`, and `src/mir/transponster.rs`.

Good entry points for the compiler "as a whole" are `src/compile/stager.rs` for the "biggest picture" and setup orchestration, and `src/compile/per_module.rs`
for most coordination of the compilation process after the parse and setup stage. Luma doesn't follow the typical pass segmentation structure you
see in many toolchains, and instead relies on coordinated phase changes for the "big picture" passes. Name resolution is done/can be done at any phase
after parsing, and at some point monomorphization/codegen and type inference will happen during overlapping time spans.

Async-await is used pervasively through the post-parse stages of compilation, and forms the core of the inner computation engine where
resolutions (of symbols, types, unifications, etc) are kept around as futures, and can be waited on by their dependents.
