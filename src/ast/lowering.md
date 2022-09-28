There are a few states here that we need to take into account

First, we have the basic AST which is desugared and
contains symbolic information on types and variables, but
is still in tree form

Next, we have symbolic lowered form
Type checking and inference happens on the Symbolic lowered form.
In Symbolic, we can no no monomorphization
and we only inspect the contracts that we are given by types
Symbolic lowered form is "linear" and still deals with allocations
and the like

Concrete lowered form is our actual monomorphizations.
Concrete lowered form has no generics, but even still the full
runtime type of some variables is unknowable because of modification
and polymorphism. We know the size of types held by value, though,
which allows us to actually take this form once it's resolved and
output LLVM



At the symbolic point, a type that is fully resolved
may still have generics



struct S<U: New> {
    v: *U,
}

func a<T>(self, b: T) where T: Printable -> U {
    self.v = U::new();
    println(b)

    self.v
}

Here, first we have the AST for a:

Block {
    statements: [
        assign {
            field {
                self
                .v
            }

            call {
                new
                ()

                base {
                    generic {
                        U
                    }
                }
            }
        }

        call {
            println
            (b)

            base {
                unscoped
            }
        }

        field {
            self
            .v
        }
    ]
}




Notes:

I think I need to actually pull apart value and virtual types

Even though they would be nice to unify for generics and such there's this weird divide in how we treat them

I think the difference can be how we store values...sorta

An *implementation* is an instance of an interface, while for some reason a valuetype and its instance
are implicitly thought of as the same thing when that isn't really the case


Actually...what if `<-` syntax allowed anything? What if it allowed any value, not just an Implementation

What if you could just like `obj <- i32 { 5 }`?

I'm going to allow using types as tags for putting in arbitrary values into objects

possible things:


a <- Printable {
    fn print(self) {
        ...
    }
}

a <- i32 { 5 }

a <- "adds" {
    fn add(a: i32, b: i32) -> i32 { a + b }
}

// up here, a is just whatever type it was before

ensure a is "adds" {
    a.add(1, 2)
}



a = Foo {
    f: i32 = 3,
    g: str = "hello"
}

schema Bar {
    h: i32
}

a <- Bar { h: 3 }

if rand() {
    a <- "users" {
        users: [1, 2, 3]
    }
} else {
    a <- "cheaters" {
        users: ["phil", "bob"]
    }
}

static-assert(a: Foo)
static-assert-not(a: Foo + "users")

print(a.f) // infallible

ensure "users" in a {
    static-assert(a: Foo + "users")

    for elem in a.users {
        print(elem)
    } // "1, 2, 3"
}

ensure "users" in a { a.users[0] } else { -1 }

a[["users"]] { a.users[0] } else { -1 }

// or for panic on nonexistence:
a[["users"]].users[0]

a[[i32]] + 3


a[["users"]] = { users: [1, 2, 3] }

// for stringly tagged members, we use implicit context
// as our descriminating property
// by having it use the held type that it is applied to
// as part of the narrowing. If we have an i32 that we [["a"]] on and an i64 that we [["a"]] on
// they do not have to be compatible member types, and will occupy different slots within
// intersection types

// By looking at the intersection of these values we can issue warnings for potentially
// ambiguous or *mentally* similar/ambiguous slots. We can declare certain interfaces/slots as
// not ambiguity hinting, so for instance two objects that overlap by the "Printable" interface
// don't need to hint that they have an ambiguous dynamic tag

each tag a {
    "users" => {
        print("runs if rand() true")
    }
    Foo => {
        print("also runs")
    }
}

//a.map\_tag("users", |a| a.users[0])

a.users // ill defined

fname = "blah"

ensure "blah": { foo: i32, bar: [string] } in b {
} else ensure "blah" { bar: [string], .. } in b {
}


struct Bar {
    field: string = "field"
}

ensure "blah" in b where {
    { foo: i32, bar: [string] } => {
        // things
    }

    { bar: [string], .. } => {
        // other things
    }
}


struct Bar(i32);

b.0 : i32

let Bar(c) = b // c is i32
