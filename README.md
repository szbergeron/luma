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
    b.mass = mass; // mass is an f64, so the emitted struct for `Body` will have a `mass: f64` member!

    b.v_x = x.cos();
    b.v_y = y.cos();

    b
};
```
