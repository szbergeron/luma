This language is designed to enable similar patterns to those used in dynamic languages
such as Python and Javascript while allowing for runtime safety similar to languages such as Rust or Java.

Conceptually, the type system is very similar to that of Rust.
Objects may implement positive or negative traits, and there is no inheritence.
Traits may provide default implementations, and may be included in the prototypes for any basic type.
All objects that are marked as 'dyn' allow for mutable polymorphism, but can be conceptually treated
much like owning a java object as one of the interfaces it implements, as there are methods to down-
or cross- cast to the other traits that the object itself implements.

To gain an intuition for how the semantics work, it may be helpful to look at how objects themselves are laid out:

```
+--------------+                       +-------------+           
|primary handle|-----------------------| vtable list |---+         
+--------------+                       +-------------+   |          
                                                         |          
+----------------------+                                 |          
|secondary trait handle|-----------+         +-----------+----+    
+----------------------+           |         |                |    
                                   |         |                |    
                                   |     +--------+      +--------+
                                   |     |trait A |      |trait B |
                                   +-----|        |      |        |
                                         |field a |      |field c |
                                         |method b|      |method d|
                                         +--------+      +--------+
```

Each `dyn` object instance has its *own* set of vtables, *multiple* vtables.
Although each object that `impl trait A` has its own `trait A` vtable,
each `trait A` vtable is *binary comptible* with every other `trait A`
vtable. This works very similarly to how Java interfaces work, except for
the fact that each of these vtables can also store *fields*. Each vtable
acts almost like it's own object, except for the fact
that for an object that does a runtime mixin of A and B, the implementation
of B for that object can refer to methods and fields from trait A on that object,
or vice versa.

This is where the idea of "mutable types" come into play:
At any point, if you recieve a `non-const + dyn` object (or create one),
it is possible to modify the type of the object by adding or removing a trait to/from that instance.

The following pseudo-syntax shows how this may be accomplished:

```
trait Foo {
    int bar,
    func baz(self, float) -> float,

    int quux = 10,
}

func f() -> Box<dyn Foo> {
    qux = Box::new(Object::new()); // basic Object trait constructor, no fields or methods

    // "where qux impl Object" is implied, but you can specify other traits here to forward-acknowledge
    // that you'll be using fields of some other traits. This allows for circular field/method
    // references between `open` blocks
    foo_for_qux = open Foo for qux where qux impl Object {
        bar = 50,
        baz = (self, input) {
            // within this context, any field/method from
            // the opened trait and the specified `where ... impl`
            // are visible and accessible.

            (self.bar + self.quux) as float * input
        }
    }

    // This statement verifies that all paths within the current block fully define `Foo` for `qux`.
    // This means that foo_for_qux must be the result of an expression that evaluates to
    // the type `impl Foo for qux` and that defines every non-default-defined field and method in Foo

    // This also modifies the type of the variable qux to be `Object + Foo`, and adds a `trait Foo` vtable to the
    // object behind qux that is initialized in accordance with the `open` expression from before
    qux is Foo by foo_for_qux

    // at this point we can use methods guaranteed by `trait Foo` on `qux`:
    print(qux.baz(4.0)) // 240.0

    // since qux is Foo+Object, Foo is a supertype of the type of qux so we can return qux:
    qux
}
```
