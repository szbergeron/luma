mod Primary {
    trait A {
        pub fn foo(self) -> ();

        pub fn bar(self) -> ();

        pub fn baz(self) -> () {
            self.foo()
        }
    }

    trait B {
        pub fn bar(self, p) -> ();
    }
}
