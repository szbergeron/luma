use super::base::*;
use super::types;

use crate::helper::lex_wrap::TokenWrapper;
use crate::lex::Token;

pub trait Expression<'a>: AstNode<'a> {
    fn expr_type(&self) -> Box<dyn types::Type>;
}

#[derive(Debug)]
pub enum ExpressionWrapper<'a> {
    Assignment(AssignmentExpression<'a>),
    BinaryOperation(BinaryOperationExpression<'a>),
    UnaryOperation(UnaryOperationExpression<'a>),
    Comparison(ComparisonOperationExpression<'a>),
    Cast(CastExpression<'a>),
    Identifier(IdentifierExpression<'a>),
    IntegerLiteral(IntegerLiteralExpression<'a>),
    MethodCall(MethodCall<'a>),
    FieldAccess(FieldAccess<'a>),
}

impl<'a> ExpressionWrapper<'a> {
    //pub fn int_literal(input: &'a str) -> ExpressionWrapper<'a> {
    pub fn integer_literal_expression(input: TokenWrapper<'a>) -> ExpressionWrapper<'a> {
        /*let span = Span {
            start: 0,
            end: 0,
        };*/
        
        let node_info = NodeInfo::from_token(&input, true);

        let inner = IntegerLiteralExpression { contents: input.slice, node_info };
        ExpressionWrapper::IntegerLiteral(inner)
    }

    pub fn identifier_expression(input: TokenWrapper<'a>) -> ExpressionWrapper<'a> {
        let node_info = NodeInfo::from_token(&input, true);

        let inner = IdentifierExpression {
            node_info,
            name: input.slice,
            node_type: None,
            //span
        };

        ExpressionWrapper::Identifier(inner)
    }
}

impl<'a> IntoAstNode<'a> for ExpressionWrapper<'a> {
    fn as_node(&self) -> &dyn AstNode<'a> {
        match self {
            Self::Assignment(e) => e,
            Self::BinaryOperation(e) => e,
            Self::UnaryOperation(e) => e,
            Self::Comparison(e) => e,
            Self::IntegerLiteral(e) => e,
            Self::Identifier(e) => e,
            Self::Cast(e) => e,
            _ => {
                println!("No implemented as_node handler for type {:?}", self);
                todo!();
            },
        }
    }

    fn as_node_mut(&mut self) -> &mut dyn AstNode<'a> {
        todo!()
    }
}

#[derive(Debug)]
pub struct AssignmentExpression<'a> {
    node_info: NodeInfo,
    //lhs: Box<dyn Expression<'a>>,
    //rhs: Box<dyn Expression<'a>>,
    pub lhs: Box<ExpressionWrapper<'a>>,
    pub rhs: Box<ExpressionWrapper<'a>>,
    //pub span: Span<'a>,
}

impl<'a> AssignmentExpression<'a> {
    pub fn new_expr(
        node_info: NodeInfo,
        lhs: Box<ExpressionWrapper<'a>>,
        rhs: Box<ExpressionWrapper<'a>>,
    ) -> Box<ExpressionWrapper<'a>> {
        Box::new(
            ExpressionWrapper::Assignment(
                AssignmentExpression {
                    node_info, lhs, rhs,
                }
            )
        )
    }
}

impl<'a> AstNode<'a> for AssignmentExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}AssignmentExpression with child expressions:",
            indent(depth),
            );
        [&self.lhs, &self.rhs].iter().for_each(|expr| expr.as_node().display(f, depth+1));
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
        let operation = BinaryOperation::from_token(operation).expect("tried to build binop from bad operator token");
        Box::new(
            ExpressionWrapper::BinaryOperation(
                BinaryOperationExpression {
                    node_info, lhs, rhs, operation,
                }
            )
        )
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
        [&self.lhs, &self.rhs].iter().for_each(|expr| expr.as_node().display(f, depth+1));
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

impl<'a> AstNode<'a> for ComparisonOperationExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}ComparisonOperationExpression with operation {:?} and child expressions:",
            indent(depth),
            self.operation,
            );
        [&self.lhs, &self.rhs].iter().for_each(|expr| expr.as_node().display(f, depth+1));
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
            _ => None
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
            Token::And => Some(Self::Reference),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct FieldAccess<'a> {
    node_info: NodeInfo,

    pub on: Box<ExpressionWrapper<'a>>,
    //pub span: Span<'a>,
    pub field: &'a str,
}

impl<'a> FieldAccess<'a> {
    pub fn new_expr(node_info: NodeInfo, field: &'a str, on: Box<ExpressionWrapper<'a>>) -> Box<ExpressionWrapper<'a>> {
        Box::new(
            ExpressionWrapper::FieldAccess(
                FieldAccess {
                    node_info, field, on,
                }
            )
        )
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
        let operation = UnaryOperation::from_token(operation).expect("tried to build unop from bad operator token");
        Box::new(
            ExpressionWrapper::UnaryOperation(
                UnaryOperationExpression {
                    node_info, subexpr, operation,
                }
            )
        )
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
        [&self.subexpr].iter().for_each(|expr| expr.as_node().display(f, depth+1));
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Debug)]
pub struct CastExpression<'a> {
    node_info: NodeInfo,

    pub subexpr: Box<ExpressionWrapper<'a>>,
    pub typeref: Box<ExpressionWrapper<'a>>,
    //pub typeref: Box<types::TypeReference<'a>>,
    //pub span: Span<'a>,
}

impl<'a> CastExpression<'a> {
    pub fn new_expr(
        node_info: NodeInfo,
        lhs: Box<ExpressionWrapper<'a>>,
        rhs: Box<ExpressionWrapper<'a>>,
    ) -> Box<ExpressionWrapper<'a>> {
        Box::new(
            ExpressionWrapper::Cast(
                CastExpression {
                    node_info, subexpr: lhs, typeref: rhs
                }
            )
        )
    }
}

impl<'a> AstNode<'a> for CastExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}CastExpression of expression:",
            indent(depth),
            );
        [&self.subexpr].iter().for_each(|expr| expr.as_node().display(f, depth+2));
        let _ = writeln!(
            f,
            "{}To type",
            indent(depth+1),
            );
        self.typeref.as_node().display(f, depth+2);
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

#[derive(Debug)]
pub struct Closure<'a> {
    node_info: NodeInfo,

    pub expressions: Vec<Box<ExpressionWrapper<'a>>>,
    pub return_type: types::TypeReference<'a>,
    pub params: Vec<super::outer::VariableDeclaration<'a>>,
    pub start: usize,
    pub end: usize,
    //pub span: Span<'a>,
}

#[derive(Debug)]
pub struct IdentifierExpression<'a> {
    node_info: NodeInfo,

    pub name: &'a str,
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
}

#[derive(Debug)]
pub struct IntegerLiteralExpression<'a> {
    node_info: NodeInfo,

    pub contents: &'a str,
    //pub span: Span<'a>,
}

impl<'a> AstNode<'a> for IntegerLiteralExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}IntegerLiteralExpression with value {}",
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
