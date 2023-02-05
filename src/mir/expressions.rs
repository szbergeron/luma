use std::collections::HashMap;

use crate::{
    avec::AtomicVec,
    cst::{self, ExpressionWrapper, LetComponentIdentifier, TypeReference},
    helper::interner::{IStr, Internable},
};

//use super::{ty, tree::NodeReference};

pub type ExpressionID = crate::avec::AtomicVecIndex;

#[derive(Debug)]
pub struct ExpressionContext {
    expressions: AtomicVec<AnyExpression>,
}

impl ExpressionContext {
    pub fn add(&mut self, expr: AnyExpression) -> (ExpressionID, &AnyExpression) {
        self.expressions.push(expr)
    }
}

#[derive(Clone, Debug)]
pub struct VarID(usize);

#[derive(Clone, Debug)]
pub enum AnyExpression {
    /// A series of expressions that are evaluated in-order
    /// and their result is currently discarded
    ///
    /// this could be changed later to aggregate their results
    /// and structure that into a type if we want `let` to have a return
    ///
    /// Currently result type is just () for this, it's used to introduce bindings
    /// at the current scope
    Block(StringBlock),

    Assign(Assign),

    Convert(Convert),
    While(Iterate),
    Branch(Branch),

    Expire(VarID),

    ///
    Binding(Binding),

    /// If the target is FQ we know exactly what to call, so we can
    /// use it directly for inference
    InvokeKnown(InvokeKnown),

    /// If this is a method call or otherwise not fully qualified
    /// then we just know the name, and a list of args, and
    /// can't yet *really* use it for inference
    ///
    /// I may remove this in favor of a `Call(Access(_)) nest pattern that is
    /// just matched for within the inference engine
    //CombinedInvokeVirtual(InvokeVirtual),

    /// A field/method access through the "static" syntax approach,
    /// where the target field must have been specified in the
    /// type definition for the type of the base variable
    StaticAccess(StaticAccess),

    /// A field/method access through the "dynamic" syntax approach,
    /// this poses a barrier obstacle to the inference engine,
    /// as the target type here requires whole-program inference
    DynamicAccess(DynamicAccess),

    /// A variable, looks up a value and has type equal to the binding
    Variable(VarID),

    /// Subject to some level of type inference, represents
    /// only a regular literal such as a str, i#, u#, f#, or ()
    ///
    /// Struct literals get their own variant in Composite()
    Literal(Literal),

    /// A composite is pretty much reserved for struct literals,
    /// though may be used in other situations eventually.
    ///
    /// It allows for easier, deeper, inference based on known field types
    Composite(Composite),
}

impl AnyExpression {
    pub fn from_ast(within: &mut ExpressionContext, ast_node: ExpressionWrapper) -> ExpressionID {
        match ast_node {
            ExpressionWrapper::Assignment(a) => {
                let cst::AssignmentExpression {
                    node_info,
                    lhs,
                    rhs,
                } = a;

                let rhs_id = Self::from_ast(within, *rhs);
                let lhs_id = Self::from_ast(within, *lhs);

                let self_node = AnyExpression::Assign(Assign {
                    rhs: rhs_id,
                    lhs: lhs_id,
                });

                let (self_id, _self_ref) = within.expressions.push(self_node);

                self_id
            }
            ExpressionWrapper::BinaryOperation(bo) => {
                let cst::BinaryOperationExpression {
                    node_info: _,
                    box lhs,
                    box rhs,
                    operation,
                } = bo;

                let rhs_id = Self::from_ast(within, rhs);
                let lhs_id = Self::from_ast(within, lhs);

                let opstr = match operation {
                    cst::BinaryOperation::Multiply => "operation *",
                    cst::BinaryOperation::Divide => "operation /",
                    cst::BinaryOperation::Add => "operation +",
                    cst::BinaryOperation::Subtract => "operation -",
                }
                .intern();

                // for now just have binary expressions be
                // called on LHS regardless of actual associativity
                let self_id = AnyExpression::make_call(
                    within,
                    lhs_id,
                    opstr,
                    vec![lhs_id, rhs_id],
                    vec![],
                );

                //let (self_id, _self_ref) = within.add(self_node);

                self_id
            }
            ExpressionWrapper::UnaryOperation(uo) => {
                let cst::UnaryOperationExpression {
                    node_info: _,
                    operation,
                    box subexpr,
                } = uo;

                let on_id = Self::from_ast(within, subexpr);

                let opstr = match operation {
                    cst::UnaryOperation::Negate => "operation u-",
                    cst::UnaryOperation::Invert => "operation u!",
                    cst::UnaryOperation::Dereference => "operation u*",
                    cst::UnaryOperation::Reference => "operation u&",
                }
                .intern();

                let self_id = AnyExpression::make_call(within, on_id, opstr, vec![on_id], vec![]);

                //let (self_id, _self_ref) = within.add(self_node);

                self_id
            }
            ExpressionWrapper::Comparison(c) => {
                let cst::ComparisonOperationExpression {
                    node_info: _,
                    operation,
                    lhs,
                    rhs,
                } = c;

                let rhs_id = Self::from_ast(within, *rhs);
                let lhs_id = Self::from_ast(within, *lhs);

                let opstr = match operation {
                    cst::ComparisonOperation::Equal => "operation ==",
                    cst::ComparisonOperation::GreaterThan => "operation >",
                    cst::ComparisonOperation::LessThan => "operation <",
                    cst::ComparisonOperation::GreaterThanOrEqual => "operation >=",
                    cst::ComparisonOperation::LessThanOrEqual => "operation <=",
                    cst::ComparisonOperation::NotEqual => "operation !@=",
                }
                .intern();

                let self_id = AnyExpression::make_call(
                    within,
                    lhs_id,
                    opstr,
                    vec![lhs_id, rhs_id],
                    vec![],
                );

                //let (self_id, _self_ref) = within.add(self_node);

                self_id
            }
            ExpressionWrapper::LetExpression(le) => {
                let cst::LetExpression {
                    node_info,
                    box primary_component,
                    box expression,
                } = le;

                let from_exp_id = Self::from_ast(within, expression);

                fn let_recursive(
                    primary_component: cst::LetComponent,
                    within: &mut ExpressionContext,
                ) -> ExpressionID {
                    let binding = match primary_component {
                        cst::LetComponent::Identifier(LetComponentIdentifier {
                            node_info,
                            identifier_string,
                            type_specifier,
                        }) => {
                            let b = AnyExpression::Binding(Binding {
                                name: identifier_string,
                                has_type: type_specifier,
                            });

                            b
                        }

                        cst::LetComponent::Tuple(t) => {
                            //
                            // do recursive bind
                            todo!()
                        }

                        // these we can revisit once we eval adding patterns and such
                        cst::LetComponent::ScopedDestructure(_) => todo!(),
                        cst::LetComponent::Discard(_) => todo!(),
                    };

                    let (id, _) = within.add(binding);

                    id
                }

                let self_id = let_recursive(primary_component, within);

                self_id
            }
            ExpressionWrapper::Cast(_) => todo!(),
            ExpressionWrapper::Literal(_) => todo!(),
            ExpressionWrapper::MemberAccess(_) => todo!(),
            ExpressionWrapper::Statement(_) => todo!(),
            ExpressionWrapper::Block(_) => todo!(),
            ExpressionWrapper::IfThenElse(_) => todo!(),
            ExpressionWrapper::While(_) => todo!(),
            ExpressionWrapper::Tuple(_) => todo!(),
            ExpressionWrapper::Return(_) => todo!(),
            ExpressionWrapper::Wildcard(_) => todo!(),
            ExpressionWrapper::LLVMLiteral(_) => todo!(),
            ExpressionWrapper::Identifier(_) => todo!(),
            ExpressionWrapper::FunctionCall(_) => todo!(),
            ExpressionWrapper::ImplementationModification(_) => todo!(),
            ExpressionWrapper::DynamicMember(_) => todo!(),
        }
    }

    pub fn make_call(
        within: &mut ExpressionContext,
        on: ExpressionID,
        to: IStr,
        args: Vec<ExpressionID>,
        generics: Vec<()>,
    ) -> ExpressionID {
        todo!()
    }
}

/*impl AnyExpression {
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
            AnyExpression::Call(a)
        }
    }
}*/

/*/// Describes the type of the expression
/// as well as the type that it needs to eventually
/// be
pub struct TypeInfo {
    source_type: InstanceConstraint,
    target_type: Option<Box<InstanceConstraint>>,
}

pub struct MetaData {
    type_info: TypeInfo,
}*/
#[derive(Clone, Debug)]
pub struct StringBlock {
    expressions: Vec<ExpressionID>,
}

#[derive(Clone, Debug)]
pub struct Scope {
    contents: ExpressionID,
}

#[derive(Clone, Debug)]
pub struct Assign {
    //meta: MetaData,
    rhs: ExpressionID,
    lhs: ExpressionID,
}

#[derive(Clone, Debug)]
pub struct Compare {
    //meta: MetaData,
    rhs: ExpressionID,
    lhs: ExpressionID,
}

#[derive(Clone, Debug)]
pub struct Convert {
    //meta: MetaData,
    source: ExpressionID,
    //target: TypeConstraint,
    /// Indicates that this is an inverred conversion,
    /// which can allow things like deref conversions
    /// and the like, but not more involved conversions
    implicit: bool,
}

#[derive(Clone, Debug)]
pub struct Iterate {
    //meta: MetaData,
    iterator: ExpressionID,

    body: ExpressionID,
}

#[derive(Clone, Debug)]
pub struct InvokeVirtual {
    pub args: Vec<ExpressionID>,
    pub on: ExpressionID,

    pub named: IStr,

    pub generics: Vec<TypeReference>,
}

#[derive(Clone, Debug)]
pub struct InvokeKnown {
    pub args: Vec<ExpressionID>,

    pub named: IStr,

    pub generics: Vec<TypeReference>,
}

impl Iterate {
    pub fn lower(&self, backto: usize) {
        /*let begin = Operation::new(
            super::quark::OperationInner::Noop(NoOperation {}),
            "loop begin".intern(),
        );*/

        todo!()
    }
}

#[derive(Clone, Debug)]
pub struct Branch {
    //meta: MetaData,
    /// A sequence, in order, of conditions to be matched (against
    /// the given expression)
    /// If a condition evaluates to true, the associated expression
    /// is evaluated and the result becomes the
    /// result of the Branch. If branches are not exhaustive,
    /// the program will be made to panic
    elements: Vec<BranchArm>,
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum Pattern {
    Binding(Binding),
    Literal(Literal),
    Tuple(Vec<Pattern>),
}

#[derive(Clone, Debug)]
pub struct Binding {
    name: IStr,

    has_type: Option<Box<TypeReference>>,
}

#[derive(Clone, Debug)]
pub struct Literal {
    has_type: TypeReference,
}

#[derive(Clone, Debug)]
pub struct StaticAccess {
    field: IStr,
    on: ExpressionID,
}

#[derive(Clone, Debug)]
pub struct DynamicAccess {
    tag: ExpressionID,
    on: ExpressionID,
}

#[derive(Clone, Debug)]
pub struct Composite {
    base_type: TypeReference,

    generics: Vec<TypeReference>,

    fields: HashMap<IStr, ExpressionID>,
}

#[derive(Clone, Debug)]
struct Return {
    //meta: MetaData,
    value: ExpressionID,
}

#[derive(Clone, Debug)]
struct Break {
    //meta: MetaData,
    value: ExpressionID,
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
    has: TypeReference,

    then: AnyExpression,
    otherwise: AnyExpression,
}

/// Corresponds to binary `<-` operator
struct Implement {
    target: ExpressionID,

    implementation: !,

    checked: bool,
}

pub struct Let {
    into: Pattern,
}
