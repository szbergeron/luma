use crate::helper::interner::{IStr, Internable};

use super::{
    //quark::{Linear, NoOperation, Operation},
    types::InstanceConstraint,
};

pub enum AnyExpression {
    Assign(Assign),
    Compare(Compare),
    Convert(Convert),
    While(While),
    Branch(Branch),
    Binding(Binding),
}

impl AnyExpression {
    /// lower returns the
    /// node ref for the "entry" node, is given the
    /// node ref for the "next" node
    pub fn lower(&self, entry: usize) -> usize {
        match self {
            AnyExpression::While(a) => a.lower(todo!()),
            AnyExpression::Compare(a) => a.lower(todo!()),
            AnyExpression::Convert(a) => a.lower(todo!()),
            AnyExpression::Assign(a) => a.lower(todo!()),
            AnyExpression::Branch(a) => a.lower(todo!()),
            AnyExpression::Binding(a) => a.lower(todo!()),
        }
    }
}

/// Describes the type of the expression
/// as well as the type that it needs to eventually
/// be
pub struct TypeInfo {
    source_type: InstanceConstraint,
    target_type: Option<Box<InstanceConstraint>>,
}

pub struct MetaData {
    type_info: TypeInfo,
}

pub struct Assign {
    meta: MetaData,

    rhs: Box<AnyExpression>,
    lhs: Box<AnyExpression>,
}

pub struct Compare {
    meta: MetaData,

    rhs: Box<AnyExpression>,
    lhs: Box<AnyExpression>,
}

pub struct Convert {
    meta: MetaData,

    source: Box<AnyExpression>,
    //target: TypeConstraint,
    /// Indicates that this is an inverred conversion,
    /// which can allow things like deref conversions
    /// and the like, but not more involved conversions
    implicit: bool,
}

pub struct While {
    meta: MetaData,

    condition: Box<AnyExpression>,

    body: Box<AnyExpression>,
}

impl While {
    pub fn lower(&self, backto: usize) {
        /*let begin = Operation::new(
            super::quark::OperationInner::Noop(NoOperation {}),
            "loop begin".intern(),
        );*/

        todo!()
    }
}

pub struct Branch {
    meta: MetaData,

    /// A sequence, in order, of conditions to be matched (against
    /// the given expression)
    /// If a condition evaluates to true, the associated expression
    /// is evaluated and the result becomes the
    /// result of the Branch. If branches are not exhaustive,
    /// the program will be made to panic
    elements: Vec<BranchArm>,
}

pub struct BranchArm {
    pattern: Pattern,
    guard: Option<AnyExpression>,
    body: AnyExpression,
}

impl Branch {
    pub fn lower(&self, entry: usize) -> usize {
        todo!()
    }
}

pub enum Pattern {
    Binding(Binding),
    Literal(Literal),
    Tuple(Vec<Pattern>),
}

pub struct Binding {
    name: IStr,

    has_type: InstanceConstraint,
}

struct Literal {
    has_type: InstanceConstraint,
}

struct VariableReference {
    name: IStr,
}

struct Return {
    meta: MetaData,

    value: Box<AnyExpression>,
}

struct Block {
    statements: Vec<AnyExpression>,
}

struct Break {
    meta: MetaData,

    value: AnyExpression,
}

/// Corresponds to unary `-` operator
struct Negate {}

/// Corresponds to unary `&` operator
struct Ref {}

/// Corresponds to unary `*` operator
struct Deref {}

/// Corresponds to unary `!` operator
struct Invert {}

struct Guard {
    check: AnyExpression,
    has: InstanceConstraint,

    then: AnyExpression,
    otherwise: AnyExpression,
}

/// Corresponds to binary `<-` operator
struct Implement {
    target: Box<AnyExpression>,

    implementation: !,

    checked: bool,
}

pub struct Let {
    into: Pattern,
}
