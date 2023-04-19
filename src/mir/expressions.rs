use std::collections::HashMap;

use itertools::Itertools;
use tracing::info;

use crate::{
    avec::AtomicVec,
    cst::{
        self, CastExpression, ExpressionWrapper, FunctionCall, IdentifierExpression,
        IfThenElseExpression, LetComponentIdentifier, LiteralExpression, MemberAccessExpression,
        NodeInfo, ScopedName, StatementExpression, StructLiteralExpression,
        SyntacticTypeReferenceRef,
    },
    helper::interner::{IStr, Internable}, ast::tree::CtxID,
};

pub type ExpressionID = crate::avec::AtomicVecIndex;

#[derive(Debug)]
pub struct ExpressionContext {
    pub expressions: AtomicVec<AnyExpression>,

    pub var_id_gen: usize,
}

impl ExpressionContext {
    pub fn add(&mut self, expr: AnyExpression) -> (ExpressionID, &AnyExpression) {
        self.expressions.push(expr)
    }

    pub fn get(&self, id: ExpressionID) -> &AnyExpression {
        self.expressions.get(id)
    }

    pub fn new_empty() -> Self {
        Self {
            expressions: AtomicVec::new(),
            var_id_gen: 0,
        }
    }

    pub fn next_var(&mut self) -> VarID {
        self.var_id_gen += 1;
        VarID(self.var_id_gen)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct VarID(pub usize);

/// If we have two operands but want to know which direction
/// any coercions should be going, this specifies which
/// operand is the source and which is the dest
#[derive(Clone, Debug)]
pub enum AssignmentDirection {
    SrcThenDest,
    DestThenSrc,
}

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
    For(For),
    Branch(Branch),

    If(If),

    //Expire(VarID),
    Binding(Binding),

    Invoke(Invoke),

    /// If the target is FQ we know exactly what to call, so we can
    /// use it directly for inference
    //InvokeKnown(InvokeKnown),

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
    Variable(VarID, NodeInfo),

    OuterReference(ScopedName, NodeInfo),

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

    /// These following variants are used during codegen when (non-compiler-bug)
    /// errors should not occur, and all regular analysis has been done
    FusedMethodInvoke(FusedMethodInvoke),
    Deref(ExpressionID),
}

#[derive(Clone, Debug)]
pub struct If {
    pub condition: ExpressionID,
    pub then_do: ExpressionID,
    pub else_do: Option<ExpressionID>,
}

#[derive(Clone, Debug)]
pub struct For {
    pub body: ExpressionID,
    pub pre: ExpressionID,
    pub post: ExpressionID,
    pub condition: ExpressionID,
}

#[derive(Clone, Debug)]
pub struct FusedMethodInvoke {
    calls: CtxID,
    with: Vec<ExpressionID>, // first one is self
}

pub struct Bindings<'prior> {
    cur: smallvec::SmallVec<[(IStr, VarID); 5]>,

    prior: Option<&'prior Bindings<'prior>>,
}

impl<'prior> Bindings<'prior> {
    pub fn child_scoped<'s>(&'s self) -> Bindings<'s>
    where
        's: 'prior,
    {
        Self {
            cur: smallvec::SmallVec::new(),
            prior: Some(self),
        }
    }

    pub fn add_binding(&mut self, name: IStr, id: VarID) {
        tracing::info!("adding binding for '{name}' to {id:?}");
        self.cur.push((name, id))
    }

    pub fn binding_for(&self, name: IStr) -> Option<VarID> {
        tracing::info!("getting what '{name}' is bound to");

        if let Some(v) = self.cur.iter().find(|(istr, vid)| *istr == name) {
            Some(v.1)
        } else {
            self.prior.map(|p| p.binding_for(name)).flatten()
        }
    }
}

impl Bindings<'static> {
    pub fn fresh() -> Self {
        Self {
            cur: smallvec::SmallVec::new(),
            prior: None,
        }
    }
}

impl AnyExpression {
    pub fn from_ast<'bindings>(
        within: &mut ExpressionContext,
        ast_node: &ExpressionWrapper,
        bindings: &mut Bindings<'bindings>,
    ) -> ExpressionID {
        match ast_node {
            ExpressionWrapper::Assignment(a) => {
                let cst::AssignmentExpression {
                    node_info,
                    lhs,
                    rhs,
                } = a;

                let rhs_id = Self::from_ast(within, rhs, bindings);
                let lhs_id = Self::from_ast(within, lhs, bindings);

                let self_node = AnyExpression::Assign(Assign {
                    info: *node_info,
                    rhs: rhs_id,
                    lhs: lhs_id,
                });

                let (self_id, _self_ref) = within.expressions.push(self_node);

                self_id
            }
            ExpressionWrapper::BinaryOperation(bo) => {
                let cst::BinaryOperationExpression {
                    node_info,
                    box lhs,
                    box rhs,
                    operation,
                } = bo;

                let rhs_id = Self::from_ast(within, rhs, bindings);
                let lhs_id = Self::from_ast(within, lhs, bindings);

                let opstr = match operation {
                    cst::BinaryOperation::Multiply => "operator[_*_]",
                    cst::BinaryOperation::Divide => "operator[_/_]",
                    cst::BinaryOperation::Add => "operator[_+_]",
                    cst::BinaryOperation::Subtract => "operator[_-_]",
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
                    *node_info,
                );

                //let (self_id, _self_ref) = within.add(self_node);

                self_id
            }
            ExpressionWrapper::UnaryOperation(uo) => {
                let cst::UnaryOperationExpression {
                    node_info,
                    operation,
                    box subexpr,
                } = uo;

                let on_id = Self::from_ast(within, subexpr, &mut bindings.child_scoped());

                let opstr = match operation {
                    cst::UnaryOperation::Negate => "operator[-_]",
                    cst::UnaryOperation::Invert => "operator[!_]",
                    cst::UnaryOperation::Dereference => "operator[*_]",
                    cst::UnaryOperation::Reference => "operator[&_]",
                }
                .intern();

                let self_id =
                    AnyExpression::make_call(within, on_id, opstr, vec![on_id], vec![], *node_info);

                //let (self_id, _self_ref) = within.add(self_node);

                self_id
            }
            ExpressionWrapper::Comparison(c) => {
                let cst::ComparisonOperationExpression {
                    node_info,
                    operation,
                    lhs,
                    rhs,
                } = c;

                let rhs_id = Self::from_ast(within, &rhs, &mut bindings.child_scoped());
                let lhs_id = Self::from_ast(within, &lhs, &mut bindings.child_scoped());

                let opstr = match operation {
                    cst::ComparisonOperation::Equal => "operator[_==_]",
                    cst::ComparisonOperation::GreaterThan => "operator[_>_]",
                    cst::ComparisonOperation::LessThan => "operator[_<_]",
                    cst::ComparisonOperation::GreaterThanOrEqual => "operator[_>=_]",
                    cst::ComparisonOperation::LessThanOrEqual => "operator[_<=_]",
                    cst::ComparisonOperation::NotEqual => "operator[_!=_]",
                }
                .intern();

                let self_id = AnyExpression::make_call(
                    within,
                    lhs_id,
                    opstr,
                    vec![lhs_id, rhs_id],
                    vec![],
                    *node_info,
                );

                //let (self_id, _self_ref) = within.add(self_node);

                self_id
            }
            ExpressionWrapper::LetExpression(le) => {
                let cst::LetExpression {
                    node_info,
                    box primary_component,
                    box expression,
                    constrained_to,
                } = le;

                //let mut cs = bindings.child_scoped();

                // a let expression operates on the parent scope
                //bindings.add_binding(todo!("break down primary_component"), within.next_var());
                /*let ident = match primary_component {
                    LetComponent::ScopedDestructure(_) => todo!(),
                    LetComponent::Tuple(_) => todo!(),
                    LetComponent::Identifier(id) => id.identifier_string,
                    LetComponent::Discard(_) => todo!(),
                };*/

                //bindings.add_binding(ident, within.next_var());

                let from_exp_id = Self::from_ast(within, expression, &mut bindings.child_scoped());

                fn let_recursive(
                    primary_component: cst::LetComponent,
                    within: &mut ExpressionContext,
                    from_exp_id: crate::avec::AtomicVecIndex,
                    bindings: &mut Bindings,
                ) -> ExpressionID {
                    let binding = match primary_component {
                        cst::LetComponent::Identifier(LetComponentIdentifier {
                            node_info,
                            identifier_string,
                            type_specifier,
                        }) => {
                            let var_id = within.next_var();
                            let b = AnyExpression::Binding(Binding {
                                info: node_info,
                                introduced_as: var_id,
                                name: identifier_string,
                                has_type: type_specifier.map(|box v| v),
                                from_source: from_exp_id, // for now, do destructuring later
                            });

                            //

                            bindings.add_binding(identifier_string, var_id);

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

                let self_id = let_recursive(primary_component.clone(), within, from_exp_id, bindings);

                self_id
            }
            ExpressionWrapper::IfThenElse(ite) => {
                let IfThenElseExpression {
                    box if_exp,
                    box then_exp,
                    box else_exp,
                    ..
                } = ite;

                /*let branch_on =
                    AnyExpression::from_ast(within, if_exp, &mut bindings.child_scoped());

                let branch_yes = BranchArm {
                    pattern: Pattern::Literal(Literal::lit_true()),
                    guard: None,
                    body: AnyExpression::from_ast(within, then_exp, &mut bindings.child_scoped()),
                };

                let branch_no = BranchArm {
                    pattern: Pattern::Literal(Literal::lit_false()),
                    guard: None,
                    body: AnyExpression::from_ast(within, else_exp, &mut bindings.child_scoped()),
                };

                let branch = Branch {
                    branch_on,
                    elements: vec![branch_yes, branch_no],
                };

                within.add(AnyExpression::Branch(branch)).0*/

                let condition = AnyExpression::from_ast(within, if_exp, bindings);
                let then_do = AnyExpression::from_ast(within, then_exp, bindings);
                let else_do = AnyExpression::from_ast(within, else_exp, bindings);

                let if_e = If { condition, then_do, else_do: Some(else_do) };

                within.add(AnyExpression::If(if_e)).0
            }
            ExpressionWrapper::Block(b) => {
                let mut cs = bindings.child_scoped(); // every bind within this operates within the
                                                      // same binding scope

                let statements = b
                    .statements
                    .clone()
                    .into_iter()
                    .map(|box e| AnyExpression::from_ast(within, &e, &mut cs))
                    .collect_vec();

                let final_expr = b.final_expr.clone().map(|box e| AnyExpression::from_ast(within, &e, &mut cs));

                let ae = AnyExpression::Block(StringBlock {
                    statements,
                    final_expr,
                    info: b.node_info,
                });

                within.add(ae).0
            }
            ExpressionWrapper::Cast(c) => {
                let CastExpression {
                    box subexpr,
                    box typeref,
                    node_info,
                } = c;
                within
                    .add(AnyExpression::Convert(Convert {
                        info: *node_info,
                        source: AnyExpression::from_ast(
                            within,
                            subexpr,
                            &mut bindings.child_scoped(),
                        ),
                        target: typeref.clone(),
                        implicit: todo!(),
                    }))
                    .0
            }
            ExpressionWrapper::MemberAccess(ma) => {
                let MemberAccessExpression {
                    node_info,
                    box on,
                    name,
                } = ma;

                let sa = StaticAccess {
                    info: *node_info,
                    field: *name,
                    on: AnyExpression::from_ast(within, on, &mut bindings.child_scoped()),
                };

                within.add(AnyExpression::StaticAccess(sa)).0
            }
            ExpressionWrapper::Literal(li) => {
                let LiteralExpression {
                    node_info,
                    contents,
                } = li;

                let value_type = match contents {
                    cst::Literal::UnitLiteral() => {
                        SyntacticTypeReferenceRef::from_std("std::Unit")
                    }
                    cst::Literal::StringLiteral(sl) => {
                        SyntacticTypeReferenceRef::from_std("std::String")
                    },
                    cst::Literal::f32Literal(_) => todo!(),
                    cst::Literal::f64Literal(_) => todo!(),
                    cst::Literal::u128Literal(_) => todo!(),
                    cst::Literal::u64Literal(_) => todo!(),
                    cst::Literal::u32Literal(_) => todo!(),
                    cst::Literal::u16Literal(_) => todo!(),
                    cst::Literal::u8Literal(_) => todo!(),
                    cst::Literal::i128Literal(_) => todo!(),
                    cst::Literal::i64Literal(_) => todo!(),
                    cst::Literal::i32Literal(v) => {
                        todo!()
                    },
                    cst::Literal::i16Literal(_) => todo!(),
                    cst::Literal::i8Literal(_) => todo!(),
                    cst::Literal::UnknownIntegerLiteral(v) => {
                        tracing::warn!("for now just casting unknown ints down to i64");
                        //Literal::lit_i32(*v as i32)
                        SyntacticTypeReferenceRef::from_std("std::i64")
                    }
                    cst::Literal::Boolean(b) => {
                        SyntacticTypeReferenceRef::from_std("std::bool")
                    },
                };

                let li = Literal {
                    info: *node_info,
                    has_type: value_type,
                    value: li.clone(),
                };

                within.add(AnyExpression::Literal(li)).0
            }
            ExpressionWrapper::StructLiteral(sl) => {
                let StructLiteralExpression {
                    info,
                    bind_from,
                    struct_base: struct_tr,
                    generics,
                } = sl;

                let fields = bind_from
                    .into_iter()
                    .map(|(name, ew)| {
                        let eid = Self::from_ast(within, ew, bindings);

                        (*name, eid)
                    })
                    .collect();

                within
                    .add(AnyExpression::Composite(Composite {
                        info: *info,
                        fields,
                        base_type: struct_tr.clone(),
                        generics: generics.clone(),
                    }))
                    .0
            }
            ExpressionWrapper::Statement(s) => {
                let StatementExpression { node_info, subexpr } = s;

                let e = Self::from_ast(within, &subexpr, bindings);

                e
            }
            ExpressionWrapper::For(f) => {
                let cst::expressions::ForExpression { node_info, box pre, box check, box post, box body } = f;
                let mut pre_scope = bindings.child_scoped();

                let pre = AnyExpression::from_ast(within, pre, &mut pre_scope);

                let mut check_scope = pre_scope.child_scoped();

                let condition = AnyExpression::from_ast(within, check, &mut check_scope);

                let mut post_scope = pre_scope.child_scoped();

                let post = AnyExpression::from_ast(within, post, &mut post_scope);

                let mut body_scope = pre_scope.child_scoped();

                let body = AnyExpression::from_ast(within, body, &mut body_scope);

                let for_e = For { body, pre, post, condition };

                within.add(AnyExpression::For(for_e)).0
            }
            ExpressionWrapper::While(_) => todo!(),
            ExpressionWrapper::Tuple(_) => todo!(),
            ExpressionWrapper::Return(_) => todo!(),
            ExpressionWrapper::Wildcard(_) => todo!(),
            ExpressionWrapper::LLVMLiteral(_) => todo!(),
            ExpressionWrapper::Identifier(id) => {
                let IdentifierExpression { node_info, ident } = id;

                if let [one] = ident.scope.as_slice() {
                    println!("Variable is: {:?}", ident);

                    let vid = bindings
                        .binding_for(*one)
                        .expect("variable was not in scope");

                    within.add(AnyExpression::Variable(vid, *node_info)).0
                } else {
                    let ae = AnyExpression::OuterReference(ident.clone(), *node_info);

                    within.add(ae).0
                    //todo!("scoped ident?")
                }

            }
            ExpressionWrapper::FunctionCall(fc) => {
                let FunctionCall {
                    node_info,
                    function,
                    args,
                } = fc;

                let dt = if let box ExpressionWrapper::Tuple(t) = args {
                    t.expressions.clone()
                } else {
                    unreachable!("dumbness")
                };

                let args = dt
                    .into_iter()
                    .map(|box e| Self::from_ast(within, &e, bindings))
                    .collect_vec();

                let target_fn = Self::from_ast(within, &function, bindings);

                let invocation = Invoke {
                    info: *node_info,
                    target_fn,
                    args,
                };

                within.add(AnyExpression::Invoke(invocation)).0
            }
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
        info: NodeInfo,
    ) -> ExpressionID {
        info!("making a call to {to}");

        let field = AnyExpression::StaticAccess(StaticAccess {
            field: to,
            on,
            info,
        });
        let field_eid = within.add(field);

        let invoke = AnyExpression::Invoke(Invoke {
            target_fn: field_eid.0,
            args,
            info,
        });

        let invoke_eid = within.add(invoke);

        invoke_eid.0
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
    pub info: NodeInfo,

    pub statements: Vec<ExpressionID>,
    pub final_expr: Option<ExpressionID>,
}

#[derive(Clone, Debug)]
pub struct Scope {
    pub contents: ExpressionID,
}

#[derive(Clone, Debug)]
pub struct Assign {
    pub info: NodeInfo,

    //meta: MetaData,
    pub rhs: ExpressionID,
    pub lhs: ExpressionID,
}

#[derive(Clone, Debug)]
pub struct Compare {
    pub info: NodeInfo,

    //meta: MetaData,
    pub rhs: ExpressionID,
    pub lhs: ExpressionID,
}

#[derive(Clone, Debug)]
pub struct Convert {
    pub info: NodeInfo,

    //meta: MetaData,
    pub source: ExpressionID,
    pub target: SyntacticTypeReferenceRef,
    /// Indicates that this is an inverred conversion,
    /// which can allow things like deref conversions
    /// and the like, but not more involved conversions
    implicit: bool,
}

#[derive(Clone, Debug)]
pub struct Iterate {
    pub info: NodeInfo,

    //meta: MetaData,
    pub iterator: ExpressionID,

    pub body: ExpressionID,
}

#[derive(Clone, Debug)]
pub struct Invoke {
    pub info: NodeInfo,

    pub target_fn: ExpressionID,
    pub args: Vec<ExpressionID>,
}

/*#[derive(Clone, Debug)]
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
}*/

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
    branch_on: ExpressionID,
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
    guard: Option<ExpressionID>,
    body: ExpressionID,
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
    pub info: NodeInfo,

    pub name: IStr,

    pub introduced_as: VarID,

    pub has_type: Option<SyntacticTypeReferenceRef>,

    pub from_source: ExpressionID,
}

#[derive(Clone, Debug)]
pub struct Literal {
    pub info: NodeInfo,

    pub has_type: SyntacticTypeReferenceRef,

    pub value: LiteralExpression,
}

impl Literal {
    pub fn lit_true() -> Self {
        Self::lit_bool(true)
    }

    pub fn lit_false() -> Self {
        Self::lit_bool(false)
    }

    pub fn lit_bool(v: bool) -> Self {
        Literal {
            info: NodeInfo::Builtin,
            has_type: SyntacticTypeReferenceRef::from_std("std::bool"),
            value: LiteralExpression {
                node_info: cst::NodeInfo::Builtin,
                contents: cst::Literal::Boolean(v),
            },
        }
    }

    pub fn lit_i32(val: i32) -> Self {
        Literal {
            info: NodeInfo::Builtin,
            has_type: SyntacticTypeReferenceRef::from_std("std::i32"),
            value: LiteralExpression {
                node_info: cst::NodeInfo::Builtin,
                contents: cst::Literal::i32Literal(val),
            },
        }
    }
}

#[derive(Clone, Debug)]
pub struct StaticAccess {
    pub info: NodeInfo,
    pub field: IStr,
    pub on: ExpressionID,
}

#[derive(Clone, Debug)]
pub struct DynamicAccess {
    tag: ExpressionID,
    on: ExpressionID,
}

#[derive(Clone, Debug)]
pub struct Composite {
    pub info: NodeInfo,

    pub base_type: ScopedName,

    pub generics: Vec<SyntacticTypeReferenceRef>,

    pub fields: HashMap<IStr, ExpressionID>,
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
    has: SyntacticTypeReferenceRef,

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
