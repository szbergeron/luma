# example 1
class Foo(Object):
    pass

def qux():
    a = Foo()
    b = Foo()
    a.bar = 42
    b.bar = "baz"

    s = [a, b]

    c = s[rand() % 2]

    d = c.bar


# example 2
class Foo(Object):
    pass

def baz(a: Foo):
    d = a.bar

def qux():
    b = Foo()
    b.bar = 42

    baz(b)

qux()

# example 3
class A():
    pass

class B():
    pass

def c(v: A):
    d = B()
    d.baz = v.bar

    e(d)

def e(v: B):
    v.baz % 3 # this operation is (int, int) -> int

def f():
    g = A()
    g.bar = 42

    c(g)
