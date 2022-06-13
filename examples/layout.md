consider:

```
struct Foo {
    a: i32,
    b: &Bar,
}

impl Foo {
    func c(i: i32) -> str { "blah" }
}

spec Bar {
    func d() -> ();
}
```

Static vtables will be

A variable of type `&Foo` will have this structure:


