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
