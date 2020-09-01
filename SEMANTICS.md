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
    bar: int,
    baz: fn(self, float) -> float,

    quux: int = 10,
}

func f() -> Box<dyn Foo> {
    qux = Box::new(Object::new()); // basic Object trait constructor, no fields or methods

    // "where qux impl Object" is implied, but you can specify other traits here to forward-acknowledge
    // that you'll be using fields of some other traits. This allows for circular field/method
    // references between `open` blocks
    foo_for_qux = impl Foo for qux where qux impl Object {
        bar = 50,
        baz = (self, input) {
            // within this context, any field/method from
            // the opened trait and the specified `where ... impl`
            // are visible and accessible.

            (self.bar + self.quux) as float * input
        },
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

So what's that whole `secondary trait handle` thing doing there?
Well, indirection (following pointers) is often rather expensive on modern machines,
that's why linked lists are so often frowned upon. Here, say you recieve an object
as a parameter to a function and you specify that it must `impl trait Foo + trait Bar`.
Since we already know it has to implement those two things, it doesn't make sense
to dereference the primary handle and traverse the vtable list, we can instead just
pass around the `trait Foo` and `trait Bar` handles and only have to do a single
dereference for virtual dispatch! This is a significant part of why
it's good to enforce as many type constraints as you can at compile time,
because trait (re)-discovery can get rather expensive (on top of being
hard for the programmer themself to reason about.)

So what do we do when a single trait isn't granular enough for what we want to do?
If you want to add single fields over time, you can just pass around the vtable itself
as a variable! When you want to add it to an object, it *will* be a runtime error
if you don't initialize every required element. What does this look like? Well,
first you create the empty vtable:
```
let mut impl_foo_for_qux = impl Foo for qux where qux impl Object {};
```
This creates a vtable specific to qux (with a pointer back to that variable for `self` refs)
which you can then build out. If you want to define a field/method, you can simply do this:
```
open impl_foo_for_qux {
    bar = 50,
    baz = (self, _) {
        42
    }
}
```
or, if a symbol is unambiguous (you aren't doing method overloading) then
you can treat it like a struct, and simply use the dot operator:
```
impl_foo_for_qux.bar = Some(50)
```
Say you pass around the vtable beyond some opaque barrier
(like a function that can't get inlined, or you pass it into a virtual dispatch method)
and the state of the vtable when you want to add it to an object isn't clear, what then?
So in the simple case where you define the entire trait in one block, the compiler can
follow the lifetime of the vtable and know that all the fields have been defined
(or even warn you if it can prove you didn't specify all the fields/methods!)
In the case where you have an opaque barrier though, there will be a runtime check
that all fields and methods have been specified, and that all traits that were
stated to be defined (in the `where` clause, for instance) are indeed present on
the object. If a runtime error here occurs, it is currently treated as a fatal error.
The thread that is trying to add that trait will panic, and so will any threads
that are unconditionally joining to that thread or that attempt to. This behavior
may change later, and there may be various control flow mechanisms added to 
make this more ergonomic.

So what's going on with that whole `Some(50)` thing in that second example?
When you're actually passing around a vtable as a variable, it's best
to think of it as a struct that contains a bunch of `Option<T>`. If a field or method
is defined, then the attribute that corresponds to it is `Some`, otherwise that attribute
is `None`. This lets you do some manual checking to see if stuff is defined, and if not
then you can define it yourself. `open` and `impl` blocks automatically wrap any attributes
in `Some` for convenience, since it's incredibly rare to want to *remove* the provided
definition for some attribute when you can just overwrite it otherwise, and this behavior
closely matches default initialization syntax for many other languages.
