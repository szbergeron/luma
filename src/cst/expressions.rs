use std::fmt::Debug;

use super::cst_traits::*;


/*use super::base::*;
use super::outer::*;
use super::ImplementationBody;
use crate::ast::TypeReference;*/
use crate::helper::interner::*;

use crate::lex::TokenWrapper;
//use super::types;

use crate::lex::Token;

pub trait Expression: CstNode {
    //fn expr_type(&self) -> Box<dyn types::StaticType>; // TODO
}

#[derive(Clone)]
pub enum ExpressionWrapper {
    Assignment(AssignmentExpression),
    BinaryOperation(BinaryOperationExpression),
    UnaryOperation(UnaryOperationExpression),
    Comparison(ComparisonOperationExpression),
    Cast(CastExpression),
    Literal(LiteralExpression),
    //MethodCall(MethodCall),
    //Access(AccessExpression),
    MemberAccess(MemberAccessExpression),
    Statement(StatementExpression),
    Block(BlockExpression),
    IfThenElse(IfThenElseExpression),
    While(WhileExpression),
    LetExpression(LetExpression),
    Tuple(Tuple),
    Return(ReturnExpression),
    Wildcard(WildcardExpression),
    LLVMLiteral(LLVMLiteralExpression),
    Identifier(IdentifierExpression),
    FunctionCall(FunctionCall),
    ImplementationModification(ImplementationModificationExpression),
}

impl Debug for ExpressionWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_node().fmt(f)
    }
}

impl ExpressionWrapper {
    //pub fn int_literal(input: &'a str) -> ExpressionWrapper {
    pub fn literal_expression(input: TokenWrapper) -> Box<ExpressionWrapper> {
        let inner = LiteralExpression::new_expr(input);

        inner
    }

    pub fn wildcard(input: TokenWrapper) -> Box<ExpressionWrapper> {
        let node_info = NodeInfo::from_token(&input);

        WildcardExpression::new_expr(node_info)
    }
}

impl IntoCstNode for ExpressionWrapper {
    fn as_node(&self) -> &dyn CstNode {
        match self {
            Self::Assignment(e) => e,
            Self::BinaryOperation(e) => e,
            Self::UnaryOperation(e) => e,
            Self::Comparison(e) => e,
            Self::Literal(e) => e,
            Self::Cast(e) => e,
            Self::Statement(e) => e,
            Self::Block(e) => e,
            Self::IfThenElse(e) => e,
            Self::LetExpression(e) => e,
            Self::Tuple(e) => e,
            Self::Wildcard(e) => e,
            Self::While(e) => e,
            Self::Return(e) => e,
            Self::LLVMLiteral(e) => e,
            Self::MemberAccess(e) => e,
            Self::FunctionCall(e) => e,
            Self::Identifier(e) => e,
            Self::ImplementationModification(e) => e,
        }
    }
}

#[derive(Debug, Clone)]
pub struct WildcardExpression {
    node_info: NodeInfo,
}

impl WildcardExpression {
    pub fn new_expr(node_info: NodeInfo) -> Box<ExpressionWrapper> {
        Box::new(ExpressionWrapper::Wildcard(WildcardExpression {
            node_info,
        }))
    }
}

impl CstNode for WildcardExpression {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn pretty(&self, f: &mut dyn std::fmt::Write, _depth: usize) {
        let _ = writeln!(f, "_");
    }
}

#[derive(Debug, Clone)]
pub struct StatementExpression {
    node_info: NodeInfo,

    subexpr: Box<ExpressionWrapper>,
}

impl StatementExpression {
    pub fn new_expr(
        node_info: NodeInfo,
        subexpr: Box<ExpressionWrapper>,
    ) -> Box<ExpressionWrapper> {
        Box::new(ExpressionWrapper::Statement(StatementExpression {
            node_info,
            subexpr,
        }))
    }
}

impl CstNode for StatementExpression {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        self.subexpr.as_node().pretty(f, depth);
    }
}

#[derive(Clone)]
pub struct Tuple {
    pub node_info: NodeInfo,

    //on: &'a str,
    pub expressions: Vec<Box<ExpressionWrapper>>,
}

impl Debug for Tuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Tuple")
            .field("expressions", &self.expressions)
            .finish()
    }
}

impl Tuple {
    pub fn new_expr(
        node_info: NodeInfo,
        expressions: Vec<Box<ExpressionWrapper>>,
    ) -> Box<ExpressionWrapper> {
        Box::new(ExpressionWrapper::Tuple(Tuple {
            node_info,
            expressions,
        }))
    }
}

impl IntoCstNode for Tuple {
    fn as_node(&self) -> &dyn CstNode {
        self
    }
}

impl CstNode for Tuple {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        let _ = write!(f, "(");

        for exp in self.expressions.iter() {
            exp.as_node().pretty(f, depth);
            let _ = write!(f, ",");
        }

        let _ = write!(f, ")");
    }
}

#[derive(Clone)]
pub struct WhileExpression {
    node_info: NodeInfo,

    if_exp: Box<ExpressionWrapper>,
    then_exp: Box<ExpressionWrapper>,
}

impl WhileExpression {
    pub fn new_expr(
        node_info: NodeInfo,
        if_exp: Box<ExpressionWrapper>,
        then_exp: Box<ExpressionWrapper>,
    ) -> Box<ExpressionWrapper> {
        Box::new(ExpressionWrapper::While(WhileExpression {
            node_info,
            if_exp,
            then_exp,
        }))
    }
}

impl Debug for WhileExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("WhileExpression")
            .field("while", &self.if_exp)
            .field("do", &self.then_exp)
            .finish()
    }
}

impl CstNode for WhileExpression {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        let _ = write!(f, "while(");
        self.if_exp.as_node().pretty(f, depth);
        let _ = writeln!(f, ") {{");
        let _ = write!(f, "{}", indent(depth + 1));
        self.then_exp.as_node().pretty(f, depth + 1);
        let _ = writeln!(f, "\n}}");
    }
}

#[derive(Clone)]
pub struct IfThenElseExpression {
    node_info: NodeInfo,

    if_exp: Box<ExpressionWrapper>,
    then_exp: Box<ExpressionWrapper>,
    else_exp: Box<ExpressionWrapper>,
}

impl IfThenElseExpression {
    pub fn new_expr(
        node_info: NodeInfo,
        if_exp: Box<ExpressionWrapper>,
        then_exp: Box<ExpressionWrapper>,
        else_exp: Box<ExpressionWrapper>,
    ) -> Box<ExpressionWrapper> {
        Box::new(ExpressionWrapper::IfThenElse(IfThenElseExpression {
            node_info,
            if_exp,
            then_exp,
            else_exp,
        }))
    }
}

impl Debug for IfThenElseExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IfThenElseExpression")
            .field("if", &self.if_exp)
            .field("then", &self.then_exp)
            .field("else", &self.else_exp)
            .finish()
    }
}

impl CstNode for IfThenElseExpression {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        let _ = write!(f, "if(");
        self.if_exp.as_node().pretty(f, depth);
        let _ = writeln!(f, ") then {{");
        let _ = write!(f, "{}", indent(depth + 1));
        self.then_exp.as_node().pretty(f, depth + 1);
        let _ = writeln!(f, "\n}} else {{");

        let _ = write!(f, "{}", indent(depth + 1));
        self.else_exp.as_node().pretty(f, depth + 1);
        let _ = writeln!(f, "\n}}");
    }
}

#[derive(Debug, Clone)]
pub struct BlockExpression {
    pub node_info: NodeInfo,

    pub contents: Vec<Box<ExpressionWrapper>>,
}

impl CstNode for BlockExpression {
    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        let _ = writeln!(f, "{{");
        for c in self.contents.iter() {
            let _ = write!(f, "{}", indent(depth + 1));
            c.as_node().pretty(f, depth + 1);
            let _ = writeln!(f, "");
        }

        let _ = write!(f, "{}}}", indent(depth));
    }
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

impl BlockExpression {
    pub fn new_expr(
        node_info: NodeInfo,
        contents: Vec<Box<ExpressionWrapper>>,
    ) -> Box<ExpressionWrapper> {
        Box::new(ExpressionWrapper::Block(BlockExpression {
            node_info,
            contents,
        }))
    }
}

/*#[derive(Debug)]
pub struct LetExpression {
    node_info: NodeInfo,

    pub into: Box<ExpressionWrapper>,
}

impl LetExpression {
    pub fn new_expr(
        node_info: NodeInfo,
        into: Box<ExpressionWrapper>,
    ) -> Box<ExpressionWrapper> {
        Box::new(ExpressionWrapper::LetExpression(LetExpression {
            node_info,
            into,
        }))
    }
}*/

#[derive(Debug, Clone)]
pub struct LetExpression {
    pub node_info: NodeInfo,

    pub primary_component: Box<LetComponent>,

    pub expression: Box<ExpressionWrapper>,
}

#[derive(Debug, Clone)]
pub struct LetComponentScopedDestructure {
    // TODO: need to pub more work into this, revisit when relevant for
// product types down the line
}

#[derive(Debug, Clone)]
pub struct LetComponentTuple {
    pub node_info: NodeInfo,

    // pub arity: usize, // implicit in length of component vec
    pub elements: Vec<LetComponent>,

    pub type_specifier: Option<Box<super::TypeReference>>,
}

#[derive(Debug, Clone)]
pub struct LetComponentIdentifier {
    pub node_info: NodeInfo,

    pub identifier_string: IStr,

    pub type_specifier: Option<Box<super::TypeReference>>,
}

#[derive(Debug, Clone)]
pub enum LetComponent {
    ScopedDestructure(LetComponentScopedDestructure),
    Tuple(LetComponentTuple),
    Identifier(LetComponentIdentifier),
    Discard(NodeInfo),
}


/*impl CstNode for LetExpression {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}*/

impl IntoCstNode for LetComponent {
    /*fn as_node_mut(&mut self) -> Option<&mut dyn AstNode> {
        Some(self)
    }*/

    fn as_node(&self) -> &dyn CstNode {
        self
    }
}

impl CstNode for LetComponent {
    fn node_info(&self) -> NodeInfo {
        match self {
            Self::ScopedDestructure(_lcsd) => {
                todo!("LetComponentScopedDestructure not implemented for fmt")
            }
            Self::Discard(ni) => *ni,
            Self::Tuple(lct) => lct.node_info,
            Self::Identifier(lci) => lci.node_info,
        }
    }

    /*fn format(&self) -> RcDoc {
        match self {
            Self::ScopedDestructure(_lcsd) => {
                todo!("LetComponentScopedDestructure not implemented for fmt")
            }
            Self::Discard(_) => RcDoc::text("_"),
            Self::Identifier(lci) => RcDoc::text(lci.identifier_string.resolve())
                .append(": ")
                .append(
                    lci.type_specifier
                        .as_ref()
                        .map(|tr| tr.as_node().format())
                        .unwrap_or(RcDoc::text("{unknown}")),
                )
                .group(),
            Self::Tuple(lct) => {
                let base = RcDoc::text("(")
                    .append(
                        RcDoc::intersperse(lct.elements.iter().map(|e| e.format()), comma_break())
                            .nest(1),
                    )
                    .append(")");

                let tspec = lct
                    .type_specifier
                    .as_ref()
                    .map(|tr| tr.as_node().format())
                    .unwrap_or(RcDoc::text("{unknown}"));

                base.append(": ").append(tspec)
                /*let mut inner = String::new();
                for idx in 0..lct.elements.len() {
                    inner.push_str(lct.elements[idx].format().as_str());
                    //lct.elements[idx].display(f, depth);
                    if idx < lct.elements.len() - 1 {
                        inner.push_str(", ");
                        //write!(f, ", ").unwrap();
                    }
                }
                /*for cmp in lct.elements.iter() {
                    cmp.display(f, depth);
                    write!(f, ", ").unwrap();
                }*/*/

                /*let tspec = lct
                    .type_specifier
                    .as_ref()
                    .map(|tr| tr.as_node().format())
                    .unwrap_or("{unknown}".to_owned());
                format!("({inner}): {tspec}")*/
            }
        }
    }*/
}

impl CstNode for LetExpression {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Clone)]
pub struct AssignmentExpression {
    node_info: NodeInfo,
    //lhs: Box<dyn Expression>,
    //rhs: Box<dyn Expression>,
    //pub is_let_expression: bool,
    pub lhs: Box<ExpressionWrapper>,
    pub rhs: Box<ExpressionWrapper>,
    //pub span: Span,
}

impl Debug for AssignmentExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AssignmentExpression")
            .field("operation", &"=")
            .field("lhs", &self.lhs)
            .field("rhs", &self.rhs)
            .finish()
    }
}

impl AssignmentExpression {
    pub fn new_expr(
        node_info: NodeInfo,
        lhs: Box<ExpressionWrapper>,
        rhs: Box<ExpressionWrapper>,
        //is_let_expression: bool,
    ) -> Box<ExpressionWrapper> {
        Box::new(ExpressionWrapper::Assignment(AssignmentExpression {
            node_info,
            lhs,
            rhs, //is_let_expression,
        }))
    }
}

impl CstNode for AssignmentExpression {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Clone)]
pub struct BinaryOperationExpression {
    node_info: NodeInfo,

    pub operation: BinaryOperation,
    pub lhs: Box<ExpressionWrapper>,
    pub rhs: Box<ExpressionWrapper>,
    //lhs: Box<dyn Expression>,
    //rhs: Box<dyn Expression>,
    //pub span: Span,
}

impl Debug for BinaryOperationExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BinaryOperationExpression")
            .field("operation", &self.operation)
            .field("lhs", &self.lhs)
            .field("rhs", &self.rhs)
            .finish()
    }
}

impl BinaryOperationExpression {
    pub fn new_expr(
        node_info: NodeInfo,
        operation: Token,
        lhs: Box<ExpressionWrapper>,
        rhs: Box<ExpressionWrapper>,
    ) -> Box<ExpressionWrapper> {
        let operation = BinaryOperation::from_token(operation)
            .expect("tried to build binop from bad operator token");
        Box::new(ExpressionWrapper::BinaryOperation(
            BinaryOperationExpression {
                node_info,
                lhs,
                rhs,
                operation,
            },
        ))
    }
}

impl CstNode for BinaryOperationExpression {
    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        self.lhs.as_node().pretty(f, depth);
        let _ = write!(f, " {} ", self.operation.fmt());
        self.rhs.as_node().pretty(f, depth);
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Clone)]
pub struct ComparisonOperationExpression {
    node_info: NodeInfo,

    pub operation: ComparisonOperation,
    //lhs: Box<dyn Expression>,
    //rhs: Box<dyn Expression>,
    pub lhs: Box<ExpressionWrapper>,
    pub rhs: Box<ExpressionWrapper>,
    //pub span: Span,
}

impl Debug for ComparisonOperationExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ComparisonOperationExpression")
            .field("operation", &self.operation)
            .field("lhs", &self.lhs)
            .field("rhs", &self.rhs)
            .finish()
    }
}

impl ComparisonOperationExpression {
    pub fn new_expr(
        node_info: NodeInfo,
        operation: Token,
        lhs: Box<ExpressionWrapper>,
        rhs: Box<ExpressionWrapper>,
    ) -> Box<ExpressionWrapper> {
        let operation = ComparisonOperation::from_token(operation)
            .expect("tried to build binop from bad operator token");
        Box::new(ExpressionWrapper::Comparison(
            ComparisonOperationExpression {
                node_info,
                lhs,
                rhs,
                operation,
            },
        ))
    }
}

impl CstNode for ComparisonOperationExpression {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Debug, Clone)]
pub enum ComparisonOperation {
    Equal,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    NotEqual,
}

impl ComparisonOperation {
    pub fn from_token(t: Token) -> Option<ComparisonOperation> {
        match t {
            Token::CmpEqual => Some(Self::Equal),
            Token::CmpGreaterThan => Some(Self::GreaterThan),
            Token::CmpLessThan => Some(Self::LessThan),
            Token::CmpGreaterThanOrEqual => Some(Self::GreaterThanOrEqual),
            Token::CmpLessThanOrEqual => Some(Self::LessThanOrEqual),
            Token::CmpNotEqual => Some(Self::NotEqual),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOperation {
    Multiply,
    Divide,
    Add,
    Subtract,
}

impl BinaryOperation {
    pub fn fmt(&self) -> &'static str {
        match self {
            Self::Multiply => "*",
            Self::Divide => "/",
            Self::Add => "+",
            Self::Subtract => "-",
        }
    }
    pub fn from_token(t: Token) -> Option<BinaryOperation> {
        match t {
            Token::Asterisk => Some(Self::Multiply),
            Token::FSlash => Some(Self::Divide),
            Token::Plus => Some(Self::Add),
            Token::Dash => Some(Self::Subtract),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOperation {
    Negate,
    Invert,
    Dereference,
    Reference,
}

impl UnaryOperation {
    pub fn from_token(t: Token) -> Option<UnaryOperation> {
        match t {
            Token::Asterisk => Some(Self::Dereference),
            Token::Bang => Some(Self::Invert),
            Token::Dash => Some(Self::Negate),
            Token::Ampersand => Some(Self::Reference),
            _ => None,
        }
    }
}

#[derive(Clone)]
pub struct MemberAccessExpression {
    pub node_info: NodeInfo,

    pub on: Box<ExpressionWrapper>,
    pub name: IStr,
    //pub span: Span,
    //pub field: &'a str,
    //pub scope: Box<ScopedNameReference>,
    //pub pattern: Option<Tuple>,
}

impl MemberAccessExpression {
    pub fn new_expr(
        node_info: NodeInfo,
        on: Box<ExpressionWrapper>,
        name: IStr,
    ) -> Box<ExpressionWrapper> {
        Box::new(ExpressionWrapper::MemberAccess(MemberAccessExpression {
            node_info,
            on,
            name,
        }))
    }
}

impl std::fmt::Debug for MemberAccessExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.debug_struct("MemberAccessExpression")
            .field("on", &self.on)
            .field("name", &self.name)
            .finish()
    }
}

impl CstNode for MemberAccessExpression {
    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        //self.scope.as_node().pretty(f, depth);
        //self.on.iter().for_each(|on| on.as_node().pretty(f, depth));
        self.on.as_node().pretty(f, depth);
        write!(f, ".{}", self.name.resolve());
        //self.pattern.iter().for_each(|pattern| pattern.as_node().pretty(f, depth));
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Clone)]
pub struct FunctionCall {
    pub node_info: NodeInfo,
    pub function: Box<ExpressionWrapper>,
    pub args: Box<Tuple>,
}

impl CstNode for FunctionCall {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

impl std::fmt::Debug for FunctionCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.debug_struct("FunctionCall")
            .field("expression", &self.function)
            .field("args", &self.args)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct MethodCall {
    node_info: NodeInfo,

    pub on: Box<ExpressionWrapper>,
    //pub span: Span,
    pub method: IStr,
    pub arguments: Vec<Box<ExpressionWrapper>>,
}

#[derive(Clone)]
pub struct UnaryOperationExpression {
    node_info: NodeInfo,

    pub operation: UnaryOperation,
    pub subexpr: Box<ExpressionWrapper>,
    //pub span: Span,
}

impl UnaryOperationExpression {
    pub fn new_expr(
        node_info: NodeInfo,
        operation: Token,
        subexpr: Box<ExpressionWrapper>,
    ) -> Box<ExpressionWrapper> {
        let operation = UnaryOperation::from_token(operation)
            .expect("tried to build unop from bad operator token");
        Box::new(ExpressionWrapper::UnaryOperation(
            UnaryOperationExpression {
                node_info,
                subexpr,
                operation,
            },
        ))
    }
}

impl std::fmt::Debug for UnaryOperationExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.debug_struct("UnaryOperationExpression")
            .field("operation", &self.operation)
            .field("subexpr", &self.subexpr)
            .finish()
    }
}

impl CstNode for UnaryOperationExpression {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Debug, Clone)]
pub struct CastExpression {
    node_info: NodeInfo,

    pub subexpr: Box<ExpressionWrapper>,
    //pub typeref: Box<ExpressionWrapper>,
    pub typeref: Box<super::TypeReference>,
    //pub typeref: Box<types::TypeReference>,
    //pub span: Span,
}

impl CastExpression {
    pub fn new_expr(
        node_info: NodeInfo,
        lhs: Box<ExpressionWrapper>,
        typeref: Box<super::TypeReference>,
    ) -> Box<ExpressionWrapper> {
        Box::new(ExpressionWrapper::Cast(CastExpression {
            node_info,
            subexpr: lhs,
            typeref,
        }))
    }
}

#[derive(Debug, Clone)]
pub struct LLVMLiteralExpression {
    pub node_info: NodeInfo,

    /// This allows choosing string aliases for
    /// a set of expressions that are to be evaluated and
    /// placed in llvm bindings corresponding with the StringSymbols
    /// provided.
    ///
    /// No additional munging is done so the user should be careful to avoid
    /// any naming conflicts here
    pub bindings: Vec<(ExpressionWrapper, IStr)>,

    /// Vars that will be set aside for the llvm literal to use.
    /// They may be templated in using {{var}} notation,
    /// but will be replaced with temporary names later
    /// during encode
    pub vars: Vec<IStr>,

    /// Contains the LLVM IR text that is to be emitted with this function
    pub text: IStr,

    /// If this block is not `_ -> ()` then it has some output T
    ///
    /// The type is specified syntactically and resides in
    /// output.unwrap().0, and the llvm binding that
    /// the value will reside in will be referred to output.unwrap().1
    ///
    /// Care should be taken by the user that the binding name does not cause a name collision
    pub output: Option<(super::TypeReference, IStr)>,
}

impl CstNode for LLVMLiteralExpression {
    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        let _ = writeln!(f, " llvm{{");
        let s = self.text.resolve();
        for line in s.lines() {
            let _ = writeln!(f, "{}{}", indent(depth + 1), line);
        }
        let _ = write!(f, "{} }}llvm", indent(depth));
    }
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    /*fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(f, " llvm{{");
        let s = self.text.resolve();
        for line in s.lines() {
            let _ = writeln!(f, "{}{}", indent(depth + 1), line);
        }
        let _ = write!(f, "{} }}llvm", indent(depth));
    }*/
}

#[derive(Debug, Clone)]
pub struct ReturnExpression {
    node_info: NodeInfo,

    pub subexpr: Box<ExpressionWrapper>,
}

impl ReturnExpression {
    pub fn new_expr(
        node_info: NodeInfo,
        subexpr: Box<ExpressionWrapper>,
    ) -> Box<ExpressionWrapper> {
        //let operation = UnaryOperation::from_token(operation).expect("tried to build unop from bad operator token");
        Box::new(ExpressionWrapper::Return(ReturnExpression {
            node_info,
            subexpr,
        }))
    }
}

impl CstNode for ReturnExpression {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

impl CstNode for CastExpression {
    /*fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(f, "{}CastExpression of expression:", indent(depth),);
        [&self.subexpr]
            .iter()
            .for_each(|expr| expr.as_node().display(f, depth + 2));
        let _ = writeln!(f, "{}To type", indent(depth + 1),);
        self.typeref.display(f, depth + 2);
    }*/

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Debug)]
pub struct TernarySelectorOperationExpression {
    node_info: NodeInfo,

    pub condition: Box<ExpressionWrapper>,
    pub first: Box<ExpressionWrapper>,
    pub second: Box<ExpressionWrapper>,
    //pub span: Span,
}

/*#[derive(Debug)]
pub struct Closure {
    node_info: NodeInfo,

    pub expressions: Vec<Box<ExpressionWrapper>>,
    pub return_type: types::TypeReference,
    pub params: Vec<Box<ExpressionWrapper>>, // should all be irrefutable patterns
    pub start: usize,
    pub end: usize,
    //pub span: Span,
}*/

#[derive(Clone)]
pub struct IdentifierExpression {
    pub node_info: NodeInfo,

    //pub name: &'a str,
    //pub context: Box<ScopedName>,
    ident: IStr,
    //pub node_type: Option<types::TypeReference>,
    //pub span: Span,
}

impl IdentifierExpression {
    pub fn from_token(tw: TokenWrapper) -> ExpressionWrapper {
        ExpressionWrapper::Identifier(Self {
            ident: tw.slice,
            node_info: NodeInfo::from_token(&tw),
        })
    }
}

impl std::fmt::Debug for IdentifierExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "IdentifierExpression('{}')", self.ident.resolve())
    }
}

impl CstNode for IdentifierExpression {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Clone, Debug)]
#[allow(non_camel_case_types)]
pub enum Literal {
    StringLiteral(IStr),

    f32Literal(f32),
    f64Literal(f64),

    u128Literal(u128),
    u64Literal(u64),
    u32Literal(u32),
    u16Literal(u16),
    u8Literal(u8),

    i128Literal(i128),
    i64Literal(i64),
    i32Literal(i32),
    i16Literal(i16),
    i8Literal(i8),

    UnknownIntegerLiteral(u128),
}

/*impl Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::StringLiteral(arg0) => f.debug_tuple("StringLiteral").field(arg0).finish(),
            Self::f32Literal(arg0) => f.debug_tuple("f32Literal").field(arg0).finish(),
            Self::f64Literal(arg0) => f.debug_tuple("f64Literal").field(arg0).finish(),
            Self::u128Literal(arg0) => f.debug_tuple("u128Literal").field(arg0).finish(),
            Self::u64Literal(arg0) => f.debug_tuple("u64Literal").field(arg0).finish(),
            Self::u32Literal(arg0) => f.debug_tuple("u32Literal").field(arg0).finish(),
            Self::u16Literal(arg0) => f.debug_tuple("u16Literal").field(arg0).finish(),
            Self::u8Literal(arg0) => f.debug_tuple("u8Literal").field(arg0).finish(),
            Self::i128Literal(arg0) => f.debug_tuple("i128Literal").field(arg0).finish(),
            Self::i64Literal(arg0) => f.debug_tuple("i64Literal").field(arg0).finish(),
            Self::i32Literal(arg0) => f.debug_tuple("i32Literal").field(arg0).finish(),
            Self::i16Literal(arg0) => f.debug_tuple("i16Literal").field(arg0).finish(),
            Self::i8Literal(arg0) => f.debug_tuple("i8Literal").field(arg0).finish(),
            Self::UnknownIntegerLiteral(arg0) => f.debug_tuple("UnknownIntegerLiteral").field(arg0).finish(),
        }
    }
    /*fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {

        match self {
            //Self::StringLiteral(arg0) => write!(f, "StringLiteral({})", arg0.resolve()),
            other => write!(f, "{}", self
        }
    }*/
}*/

#[derive(Clone)]
pub struct LiteralExpression {
    node_info: NodeInfo,

    //pub contents: &'a str,
    pub contents: Literal,
    //pub span: Span,
}

impl Debug for LiteralExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LiteralExpression")
            .field("contents", &self.contents)
            .finish()
    }
}

impl LiteralExpression {
    pub fn new_expr(tw: TokenWrapper) -> Box<ExpressionWrapper> {
        let slice = tw.slice.resolve();
        let literal = match tw.token {
            Token::UnknownIntegerLiteral => {
                Literal::UnknownIntegerLiteral(tw.slice.resolve().parse().unwrap())
            }
            Token::i8Literal => Literal::i8Literal(tw.slice.resolve().parse().unwrap()),
            Token::i32Literal => Literal::i32Literal(slice[..slice.len() - 3].parse().unwrap()),
            Token::StringLiteral => Literal::StringLiteral(tw.slice),
            _ => {
                println!("No literal handler for {tw:?}");
                todo!()
            },
        };

        Box::new(ExpressionWrapper::Literal(LiteralExpression {
            node_info: NodeInfo::from_token(&tw),
            contents: literal,
        }))
    }
}

impl CstNode for LiteralExpression {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Debug, Clone)]
pub struct ImplementationModificationExpression {
    pub node_info: NodeInfo,

    pub modifying: Box<ExpressionWrapper>,
    //pub traits: Vec<super::TypeReference>,

    pub impl_block: Box<super::ImplementationDefinition>,
}

impl CstNode for ImplementationModificationExpression {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}
