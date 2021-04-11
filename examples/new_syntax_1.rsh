struct StructName {
    field_1: i32,
}

fn func(param_a: &StructName, param_b: i32) -> i32 {
    param_a.field_1 + param_b // should eval whether deref is automatic for field access on ref types, potentially only require for opaque ref types?
}

fn main() {
    let inst: StructName = StructName { field_1: 5 };

    print(stringify(func(&inst, 10)));
}

__builtin fn add(v1: i32, v2: i32) -> i32
__ll_vars { var1: v1, var2: v2, out } // put the value of v1 into the temp var1, and same for v2. `out` is left uninitialized
__ll_result { out } // expect the output single tuple element of the function to reside in the temp `out`
llvm{
    ; add impl for i32 type
    {out} = add i32 {v1}, {v2}
}llvm

__builtin fn assign(lhs: &i32, rhs: i32) -> &i32
__ll_vars { var1: lhs, var2: rhs, out } // put the value of v1 into the temp var1, and same for v2. `out` is left uninitialized
__ll_result { out } // expect the output single tuple element of the function to reside in the temp `out`
llvm{
    ; insert into lhs (stor) from value of rhs
}llvm
