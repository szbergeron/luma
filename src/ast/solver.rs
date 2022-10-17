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

    /// Yes, a type reference is in fact an expression
    ///
    /// A type reference can be used as an ID for matching
    /// tags or can have the variadic Construct operation applied to it
    ///
    /// This all allows type aliases within variables, but only at compile time
    ///
    /// If variables of type Type cannot be completely resolved then an error
    /// should be emitted, as every compositionally unique
    /// Type expression has a distinct type, in other words
    /// (TypeA,) and (TypeB,) even though both are Type expressions
    /// in arity-1 tuples, have different types.
    ///
    /// All Type expressions are (and must be) const
    Type(),
}

struct Assignment {
    left: ExpressionID,
    right: ExpressionID,
}

struct FieldAccess {
}

struct DynamicAccess {
    base: ExpressionID,

    tag: ExpressionID,
}

struct DynamicLiteral {
}

/// the type of a given expression is itself
/// a value, but more specifically is a fully
/// const value
///
/// The type of `3`, say, is both `i32` and `3`, but
/// only `3` within const context for, say, tags and selectors
///
/// We can therefore have a function that
/// has a signature `func f(a: i32, b: 10)` where
/// the function can be called with `f(3, 10)` but not `f(3, 9)`,
/// because there `9` does not have type `10`
///
/// This neat trick means we can verify some really
/// cool generic requirements later on, say with
/// list concatenation and such
struct Type {
}

/// Model typeof("hello") as
/// Tag(str) + Size(sizeof(str)) + Value(['h', 'e', 'l', 'l', 'o'])
enum Constraint {
    Tag(TagConstraint),
    Size(SizeConstraint),
    Value(ValueConstraint),
    Operation(OperationConstraint),
    Type(TypeConstraint),
}

impl Type {
    pub fn as_const(&self) -> Vec<ID> {
    }

    pub fn 
}

struct Expressionable {
    operation: Expression,

    etype: Future<Type>,
}

struct NodeInstance {
    dynmem: HashMap<ConstValue, NodeInstanceID>,

    members: HashMap<IStr, Member>,
}

struct Type {
    chain: Vec<NodeInstanceID>,
}

struct Value {
    origin: ExpressionID,

    value: Option<ConstValue>,

    is_a: Future<Type>,
}
