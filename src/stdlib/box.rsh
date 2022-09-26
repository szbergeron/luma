struct Box<T> {
    value: *T
}

impl<T> Box<T> {
    pub func as_ref(&self) -> &T ll_vars { in <self>, out <r> } llvm{
        ; we have self in this context under the label %var_self, which is
        ; a &&Self within this context (labels/variables are by reference within llvm)
        %r = load {type_self} %{self}
    }llvm
}
