/// This is a start-from-scratch expression type solution engine, just like my other 6
/// start-from-scratch attempts at making a type solution engine except this one
/// will *definitely* work

enum Expression {
    BinaryOperator(),
    Identifier(),
    Literal(),
    DynamicAccess(),
    DynamicLiteral(),
    FieldAccess(),
    ArrayAccess(),
    Assignment(),
}

struct Assignment {
    left: ExpressionID,
    right: ExpressionID,
}

struct FieldAccess {
}

struct DynamicAccess {
    base: ExpressionID,

    tag: Tag,
}

struct DynamicLiteral {
}
