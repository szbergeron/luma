use super::base::*;
use super::outer::*;
//use super::types;

use crate::helper::lex_wrap::{ParseResultError, TokenWrapper};
use crate::lex::Token;
use crate::types;

pub trait Expression<'a>: AstNode<'a> {
    fn expr_type(&self) -> Box<dyn types::Type>;
}

#[derive(Debug)]
pub struct TypeReference<'a> {
    node_info: NodeInfo,

    pub ctx: ScopedNameReference<'a>,
    pub canonicalized_name: &'a str,

    pub type_args: Vec<Box<TypeReference<'a>>>
}

impl<'a> TypeReference<'a> {
    pub fn new(ctx: ScopedNameReference<'a>, name: &'a str) -> TypeReference<'a> {
        TypeReference { node_info: NodeInfo::Builtin, ctx, type_args: Vec::new(), canonicalized_name: name }
    }
}

impl<'a> AstNode<'a> for TypeReference<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}TypeReference with child canon name {}",
            indent(depth), self.canonicalized_name
        );
        /*
        [&self.subexpr]
            .iter()
            .for_each(|expr| expr.as_node().display(f, depth + 1));*/
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Debug)]
pub enum ExpressionWrapper<'a> {
    Assignment(AssignmentExpression<'a>),
    BinaryOperation(BinaryOperationExpression<'a>),
    UnaryOperation(UnaryOperationExpression<'a>),
    Comparison(ComparisonOperationExpression<'a>),
    Cast(CastExpression<'a>),
    Literal(LiteralExpression<'a>),
    MethodCall(MethodCall<'a>),
    Access(AccessExpression<'a>),
    Statement(StatementExpression<'a>),
    Block(BlockExpression<'a>),
    IfThenElse(IfThenElseExpression<'a>),
    While(WhileExpression<'a>),
    LetExpression(LetExpression<'a>),
    Pattern(Pattern<'a>),
    Return(ReturnExpression<'a>),
    Wildcard(WildcardExpression),
}

impl<'a> ExpressionWrapper<'a> {
    //pub fn int_literal(input: &'a str) -> ExpressionWrapper<'a> {
    pub fn literal_expression(input: TokenWrapper<'a>) -> Box<ExpressionWrapper<'a>> {
        /*let span = Span {
            start: 0,
            end: 0,
        };*/

        //let node_info = NodeInfo::from_token(&input, true);

        //let inner = LiteralExpression { contents: input.slice, node_info };
        let inner = LiteralExpression::new_expr(input);
        //ExpressionWrapper::Literal(inner)

        inner
    }

    /*pub fn identifier_expression(input: TokenWrapper<'a>) -> Box<ExpressionWrapper<'a>> {
        let node_info = NodeInfo::from_token(&input, true);

        let inner = IdentifierExpression {
            node_info,
            name: input.slice,
            node_type: None,
            //span
        };

        Box::new(ExpressionWrapper::Identifier(inner))
    }*/

    pub fn wildcard(input: TokenWrapper<'a>) -> Box<ExpressionWrapper<'a>> {
        let node_info = NodeInfo::from_token(&input);

        WildcardExpression::new_expr(node_info)

        //Box::new(ExpressionWrapper::
    }
}

impl<'a> IntoAstNode<'a> for ExpressionWrapper<'a> {
    fn as_node(&self) -> &dyn AstNode<'a> {
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
            Self::Pattern(e) => e,
            Self::Access(e) => e,
            Self::Wildcard(e) => e,
            Self::While(e) => e,
            Self::Return(e) => e,
            _ => {
                println!("No implemented as_node handler for type {:?}", self);
                todo!();
            }
        }
    }

    fn as_node_mut(&mut self) -> &mut dyn AstNode<'a> {
        todo!()
    }
}

#[derive(Debug)]
pub struct WildcardExpression {
    node_info: NodeInfo,
}

impl<'a> WildcardExpression {
    pub fn new_expr(node_info: NodeInfo) -> Box<ExpressionWrapper<'a>> {
        Box::new(ExpressionWrapper::Wildcard(WildcardExpression {
            node_info,
        }))
    }
}

impl<'a> AstNode<'a> for WildcardExpression {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}WildcardExpression parsed at {}:",
            indent(depth),
            self.node_info,
        );
        //[&self.subexpr].iter().for_each(|expr| expr.as_node().display(f, depth+1));
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Debug)]
pub struct StatementExpression<'a> {
    node_info: NodeInfo,

    subexpr: Box<ExpressionWrapper<'a>>,
}

impl<'a> StatementExpression<'a> {
    pub fn new_expr(
        node_info: NodeInfo,
        subexpr: Box<ExpressionWrapper<'a>>,
    ) -> Box<ExpressionWrapper<'a>> {
        Box::new(ExpressionWrapper::Statement(StatementExpression {
            node_info,
            subexpr,
        }))
    }
}

impl<'a> AstNode<'a> for StatementExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}StatementExpression with child expression:",
            indent(depth),
        );
        [&self.subexpr]
            .iter()
            .for_each(|expr| expr.as_node().display(f, depth + 1));
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Debug)]
pub struct Pattern<'a> {
    pub node_info: NodeInfo,

    //on: &'a str,
    pub expressions: Vec<Box<ExpressionWrapper<'a>>>,
}

impl<'a> Pattern<'a> {
    pub fn new_expr(
        node_info: NodeInfo,
        expressions: Vec<Box<ExpressionWrapper<'a>>>,
    ) -> Box<ExpressionWrapper<'a>> {
        Box::new(ExpressionWrapper::Pattern(Pattern {
            node_info,
            expressions,
        }))
    }
}

impl<'a> AstNode<'a> for Pattern<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(f, "{}Pattern with child expressions:", indent(depth),);
        self.expressions
            .iter()
            .for_each(|expr| expr.as_node().display(f, depth + 1));
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Debug)]
pub struct WhileExpression<'a> {
    node_info: NodeInfo,

    if_exp: Box<ExpressionWrapper<'a>>,
    then_exp: Box<ExpressionWrapper<'a>>,
}

impl<'a> WhileExpression<'a> {
    pub fn new_expr(
        node_info: NodeInfo,
        if_exp: Box<ExpressionWrapper<'a>>,
        then_exp: Box<ExpressionWrapper<'a>>,
    ) -> Box<ExpressionWrapper<'a>> {
        Box::new(ExpressionWrapper::While(WhileExpression {
            node_info,
            if_exp,
            then_exp,
        }))
    }
}

impl<'a> AstNode<'a> for WhileExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(f, "{}IfThenElseExpression:", indent(depth),);
        let _ = writeln!(f, "{}While:", indent(depth + 1));
        self.if_exp.as_node().display(f, depth + 2);
        let _ = writeln!(f, "{}Do:", indent(depth + 1));
        self.then_exp.as_node().display(f, depth + 2);
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Debug)]
pub struct IfThenElseExpression<'a> {
    node_info: NodeInfo,

    if_exp: Box<ExpressionWrapper<'a>>,
    then_exp: Box<ExpressionWrapper<'a>>,
    else_exp: Box<ExpressionWrapper<'a>>,
}

impl<'a> IfThenElseExpression<'a> {
    pub fn new_expr(
        node_info: NodeInfo,
        if_exp: Box<ExpressionWrapper<'a>>,
        then_exp: Box<ExpressionWrapper<'a>>,
        else_exp: Box<ExpressionWrapper<'a>>,
    ) -> Box<ExpressionWrapper<'a>> {
        Box::new(ExpressionWrapper::IfThenElse(IfThenElseExpression {
            node_info,
            if_exp,
            then_exp,
            else_exp,
        }))
    }
}

impl<'a> AstNode<'a> for IfThenElseExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(f, "{}IfThenElseExpression:", indent(depth),);
        let _ = writeln!(f, "{}If:", indent(depth + 1));
        self.if_exp.as_node().display(f, depth + 2);
        let _ = writeln!(f, "{}Then:", indent(depth + 1));
        self.then_exp.as_node().display(f, depth + 2);
        let _ = writeln!(f, "{}Else:", indent(depth + 1));
        self.else_exp.as_node().display(f, depth + 2);
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Debug)]
pub struct BlockExpression<'a> {
    pub node_info: NodeInfo,

    pub contents: Vec<Result<Box<ExpressionWrapper<'a>>, ParseResultError<'a>>>,
}

impl<'a> AstNode<'a> for BlockExpression<'a> {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        findent(f, depth);
        let _ = writeln!(f, "BlockExpression {} with children:", self.node_info());

        self.contents.iter().for_each(|elem| {
            elem.iter()
                .for_each(|elem| elem.as_node().display(f, depth + 1))
        });
    }
}

impl<'a> BlockExpression<'a> {
    pub fn new_expr(
        node_info: NodeInfo,
        contents: Vec<Result<Box<ExpressionWrapper<'a>>, ParseResultError<'a>>>,
    ) -> Box<ExpressionWrapper<'a>> {
        Box::new(ExpressionWrapper::Block(BlockExpression {
            node_info,
            contents,
        }))
    }
}

#[derive(Debug)]
pub struct LetExpression<'a> {
    node_info: NodeInfo,

    pub into: Box<ExpressionWrapper<'a>>,
}

impl<'a> LetExpression<'a> {
    pub fn new_expr(
        node_info: NodeInfo,
        into: Box<ExpressionWrapper<'a>>,
    ) -> Box<ExpressionWrapper<'a>> {
        Box::new(ExpressionWrapper::LetExpression(LetExpression {
            node_info,
            into,
        }))
    }
}

impl<'a> AstNode<'a> for LetExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}LetExpression that comes from assignment:",
            indent(depth),
        );
        [&self.into]
            .iter()
            .for_each(|expr| expr.as_node().display(f, depth + 1));
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Debug)]
pub struct AssignmentExpression<'a> {
    node_info: NodeInfo,
    //lhs: Box<dyn Expression<'a>>,
    //rhs: Box<dyn Expression<'a>>,
    //pub is_let_expression: bool,
    pub lhs: Box<ExpressionWrapper<'a>>,
    pub rhs: Box<ExpressionWrapper<'a>>,
    //pub span: Span<'a>,
}

impl<'a> AssignmentExpression<'a> {
    pub fn new_expr(
        node_info: NodeInfo,
        lhs: Box<ExpressionWrapper<'a>>,
        rhs: Box<ExpressionWrapper<'a>>,
        //is_let_expression: bool,
    ) -> Box<ExpressionWrapper<'a>> {
        Box::new(ExpressionWrapper::Assignment(AssignmentExpression {
            node_info,
            lhs,
            rhs, //is_let_expression,
        }))
    }
}

impl<'a> AstNode<'a> for AssignmentExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}AssignmentExpression with child expressions:",
            indent(depth),
            //if self.is_let_expression { "is" } else { "is not" },
        );
        [&self.lhs, &self.rhs]
            .iter()
            .for_each(|expr| expr.as_node().display(f, depth + 1));
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Debug)]
pub struct BinaryOperationExpression<'a> {
    node_info: NodeInfo,

    pub operation: BinaryOperation,
    pub lhs: Box<ExpressionWrapper<'a>>,
    pub rhs: Box<ExpressionWrapper<'a>>,
    //lhs: Box<dyn Expression<'a>>,
    //rhs: Box<dyn Expression<'a>>,
    //pub span: Span<'a>,
}

impl<'a> BinaryOperationExpression<'a> {
    pub fn new_expr(
        node_info: NodeInfo,
        operation: Token,
        lhs: Box<ExpressionWrapper<'a>>,
        rhs: Box<ExpressionWrapper<'a>>,
    ) -> Box<ExpressionWrapper<'a>> {
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

impl<'a> AstNode<'a> for BinaryOperationExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}BinaryOperationExpression with operation {:?} and  child expressions:",
            indent(depth),
            self.operation,
        );
        [&self.lhs, &self.rhs]
            .iter()
            .for_each(|expr| expr.as_node().display(f, depth + 1));
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Debug)]
pub struct ComparisonOperationExpression<'a> {
    node_info: NodeInfo,

    pub operation: ComparisonOperation,
    //lhs: Box<dyn Expression<'a>>,
    //rhs: Box<dyn Expression<'a>>,
    pub lhs: Box<ExpressionWrapper<'a>>,
    pub rhs: Box<ExpressionWrapper<'a>>,
    //pub span: Span<'a>,
}

impl<'a> ComparisonOperationExpression<'a> {
    pub fn new_expr(
        node_info: NodeInfo,
        operation: Token,
        lhs: Box<ExpressionWrapper<'a>>,
        rhs: Box<ExpressionWrapper<'a>>,
    ) -> Box<ExpressionWrapper<'a>> {
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

impl<'a> AstNode<'a> for ComparisonOperationExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}ComparisonOperationExpression with operation {:?} and child expressions:",
            indent(depth),
            self.operation,
        );
        [&self.lhs, &self.rhs]
            .iter()
            .for_each(|expr| expr.as_node().display(f, depth + 1));
    }

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

#[derive(Debug)]
pub struct AccessExpression<'a> {
    pub node_info: NodeInfo,

    pub on: Option<Box<ExpressionWrapper<'a>>>,
    //pub span: Span<'a>,
    //pub field: &'a str,
    pub scope: Box<ScopedNameReference<'a>>,
    pub pattern: Option<Pattern<'a>>,
}

impl<'a> AccessExpression<'a> {
    pub fn new_expr(
        node_info: NodeInfo,
        on: Option<Box<ExpressionWrapper<'a>>>,
        scope: Box<ScopedNameReference<'a>>,
        pattern: Option<Pattern<'a>>,
    ) -> Box<ExpressionWrapper<'a>> {
        Box::new(ExpressionWrapper::Access(AccessExpression {
            node_info,
            scope,
            on,
            pattern,
        }))
    }
}

impl<'a> AstNode<'a> for AccessExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}AccessExpression parsed {} in scope {:?} has subexpr and pattern:",
            indent(depth),
            self.node_info,
            self.scope,
        );
        //self.pattern.display(f, depth+1)
        //[&self.subexpr].iter().for_each(|expr| expr.as_node().display(f, depth+1));

        match &self.on {
            Some(e) => e.as_node().display(f, depth + 1),
            None => {
                let _ = writeln!(f, "{}No subexpr", indent(depth + 1));
            }
        }

        match &self.pattern {
            Some(p) => p
                .expressions
                .iter()
                .for_each(|expr| expr.as_node().display(f, depth + 1)),
            None => {
                let _ = writeln!(f, "{}No pattern", indent(depth + 1));
            }
        }
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Debug)]
pub struct MethodCall<'a> {
    node_info: NodeInfo,

    pub on: Box<ExpressionWrapper<'a>>,
    //pub span: Span<'a>,
    pub method: &'a str,
    pub arguments: Vec<Box<ExpressionWrapper<'a>>>,
}

#[derive(Debug)]
pub struct UnaryOperationExpression<'a> {
    node_info: NodeInfo,

    pub operation: UnaryOperation,
    pub subexpr: Box<ExpressionWrapper<'a>>,
    //pub span: Span<'a>,
}

impl<'a> UnaryOperationExpression<'a> {
    pub fn new_expr(
        node_info: NodeInfo,
        operation: Token,
        subexpr: Box<ExpressionWrapper<'a>>,
    ) -> Box<ExpressionWrapper<'a>> {
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

impl<'a> AstNode<'a> for UnaryOperationExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}UnaryOperationExpression with operation {:?} and child expression:",
            indent(depth),
            self.operation,
        );
        [&self.subexpr]
            .iter()
            .for_each(|expr| expr.as_node().display(f, depth + 1));
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Debug)]
pub struct CastExpression<'a> {
    node_info: NodeInfo,

    pub subexpr: Box<ExpressionWrapper<'a>>,
    //pub typeref: Box<ExpressionWrapper<'a>>,
    pub typeref: Box<super::TypeReference<'a>>,
    //pub typeref: Box<types::TypeReference<'a>>,
    //pub span: Span<'a>,
}

impl<'a> CastExpression<'a> {
    pub fn new_expr(
        node_info: NodeInfo,
        lhs: Box<ExpressionWrapper<'a>>,
        typeref: Box<super::TypeReference<'a>>,
    ) -> Box<ExpressionWrapper<'a>> {
        Box::new(ExpressionWrapper::Cast(CastExpression {
            node_info,
            subexpr: lhs,
            typeref,
        }))
    }
}

#[derive(Debug)]
pub struct ReturnExpression<'a> {
    node_info: NodeInfo,

    pub subexpr: Box<ExpressionWrapper<'a>>,
}

impl<'a> ReturnExpression<'a> {
    pub fn new_expr(
        node_info: NodeInfo,
        subexpr: Box<ExpressionWrapper<'a>>,
    ) -> Box<ExpressionWrapper<'a>> {
        //let operation = UnaryOperation::from_token(operation).expect("tried to build unop from bad operator token");
        Box::new(ExpressionWrapper::Return(ReturnExpression {
            node_info,
            subexpr,
        }))
    }
}

impl<'a> AstNode<'a> for ReturnExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}ReturnExpression with operation child expression:",
            indent(depth),
        );
        [&self.subexpr]
            .iter()
            .for_each(|expr| expr.as_node().display(f, depth + 1));
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

impl<'a> AstNode<'a> for CastExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(f, "{}CastExpression of expression:", indent(depth),);
        [&self.subexpr]
            .iter()
            .for_each(|expr| expr.as_node().display(f, depth + 2));
        let _ = writeln!(f, "{}To type", indent(depth + 1),);
        self.typeref.display(f, depth + 2);
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Debug)]
pub struct TernarySelectorOperationExpression<'a> {
    node_info: NodeInfo,

    pub condition: Box<ExpressionWrapper<'a>>,
    pub first: Box<ExpressionWrapper<'a>>,
    pub second: Box<ExpressionWrapper<'a>>,
    //pub span: Span<'a>,
}

/*#[derive(Debug)]
pub struct Closure<'a> {
    node_info: NodeInfo,

    pub expressions: Vec<Box<ExpressionWrapper<'a>>>,
    pub return_type: types::TypeReference<'a>,
    pub params: Vec<Box<ExpressionWrapper<'a>>>, // should all be irrefutable patterns
    pub start: usize,
    pub end: usize,
    //pub span: Span<'a>,
}*/

/*#[derive(Debug)]
pub struct IdentifierExpression<'a> {
    pub node_info: NodeInfo,

    //pub name: &'a str,
    pub context: Box<ScopedName<'a>>,
    pub node_type: Option<types::TypeReference<'a>>,
    //pub span: Span<'a>,
}

impl<'a> AstNode<'a> for IdentifierExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}IdentifierExpression with name {} and type {:?}:",
            indent(depth),
            self.name,
            self.node_type,
            );
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}*/

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum Literal<'a> {
    StringLiteral(&'a str),

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

#[derive(Debug)]
pub struct LiteralExpression<'a> {
    node_info: NodeInfo,

    //pub contents: &'a str,
    pub contents: Literal<'a>,
    //pub span: Span<'a>,
}

impl<'a> LiteralExpression<'a> {
    pub fn new_expr(tw: TokenWrapper<'a>) -> Box<ExpressionWrapper<'a>> {
        let literal = match tw.token {
            Token::UnknownIntegerLiteral => {
                Literal::UnknownIntegerLiteral(tw.slice.parse().unwrap())
            }
            Token::i8Literal => Literal::i8Literal(tw.slice.parse().unwrap()),
            Token::StringLiteral => Literal::StringLiteral(tw.slice),
            _ => todo!(),
        };

        Box::new(ExpressionWrapper::Literal(LiteralExpression {
            node_info: NodeInfo::from_token(&tw),
            contents: literal,
        }))
    }
}

impl<'a> AstNode<'a> for LiteralExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}LiteralExpression with value {:?}",
            indent(depth),
            self.contents,
        );
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

/*pub trait Type: std::fmt::Debug + std::clone::Clone {
}*/
