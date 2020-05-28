pub mod constants {
    // prefix UQ indicates UNQUALIFIED
    const UQ_OPERATOR_MUL: &str = "unqualified_operator_multiply"; // *
    const UQ_OPERATOR_DIV: &str = "unqualified_operator_divide"; // /
    const UQ_OPERATOR_SUB: &str = "unqualified_operator_subtract"; // -
    const UQ_OPERATOR_ADD: &str = "unqualified_operator_add"; // +
    const UQ_OPERATOR_EXP: &str = "unqualified_operator_exponentiate"; // ^

    //const UQ_COERCE_
}

use crate::helper::lex_wrap::*;

use std::cell::RefCell;
use std::rc::Rc;

pub type CodeLocation = usize;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    start: usize,
    end: usize,
}

pub struct NodeInfo {
    span: Span,
    parsed: bool,
}

pub trait AstNode<'a> {
    fn node_info(&self) -> NodeInfo;
    fn start(&self) -> CodeLocation;
    fn end(&self) -> CodeLocation;
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize);
}

impl<'a> dyn AstNode<'a> {
    pub fn start(&self) -> CodeLocation {
        self.node_info().span.start
    }

    pub fn end(&self) -> CodeLocation {
        self.node_info().span.end
    }
}

pub struct Namespace<'a> {
    node_info: NodeInfo,

    public: bool,
    name: Option<&'a str>,
    contents: Result<ParseUnit<'a>, ParseResultError<'a>>,
}

impl<'a> AstNode<'a> for Namespace<'a> {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

pub struct ParseUnit<'a> {
    node_info: NodeInfo,

    declarations: Vec<Result<SymbolDeclaration<'a>, ParseResultError<'a>>>,
}

impl<'a> std::fmt::Display for ParseUnit<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let r = write!(f, "\n");

        self.display(f, 1);

        r
    }
}

impl<'a> AstNode<'a> for ParseUnit<'a> {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

/*pub mod literal {
    pub enum */

#[derive(Debug)]
pub struct Namespace<'a> {
    // maybe add a <symbols> member for better perf checking
    // redeclaration?
    pub public: bool,
    pub name: Option<&'a str>,
    pub contents: Option<ParseUnit<'a>>,

    //
    pub failed: bool,

    pub span: Span<'a>,
}

impl<'a> AstDisplay for Namespace<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}Namespace that parsed {} with name {} and public {} has children:",
            indent(depth),
            self.failed,
            self.name.unwrap_or("<unnamed>"),
            self.public);

        self.contents.iter().for_each(|contents| contents.display(f, depth+1));
    }

}

impl<'a> AstDisplay for VariableDeclaration<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}VariableDeclaration that parsed {} with name {} and type {:?} comes from expression:",
            indent(depth),
            self.failed,
            self.name,
            self.var_type,
            );
        match &self.var_expr {
            Some(e) => e.display(f, depth+1),
            None => { let _ = writeln!(f, "{} unassigned", indent(depth+1)); },
        }
    }
}

/*impl<'a> std::fmt::Debug for Namespace<'a> {
    fn fmt(&self, &mut f: std::fmt::Formatter) -> std::fmt::Result {
        write! /**/
    }
}*/

fn indent(ind: usize) -> String {
    let mut s: String = "|".to_string();

    for _ in 0..ind {
        //s.push('\t');
        s.push_str("  ");
    }

    s
}

fn findent(f: &mut std::fmt::Formatter<'_>, depth: usize) {
    write!(f, "{}", indent(depth)).unwrap();
}

#[derive(Debug)]
pub struct ParseUnit<'a> {
    //namespaces: Vec<Namespace<'a>>,
    pub failed: bool,
    pub declarations: Vec<Result<SymbolDeclaration<'a>, ParseResultError<'a>>>,
    // modrefs (using exprs)?
    pub span: Span<'a>,
}

impl<'a> std::fmt::Display for ParseUnit<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let r = write!(f, "\n");

        self.display(f, 1);

        //self.fmt(f, 0);

        r
        //Ok(())
    }
}

impl<'a> AstDisplay for ParseUnit<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        //
        //
        //let _ = write!(f, "{}", indent(depth));

        findent(f, depth);
        let _ = writeln!(f, "ParseUnit that parsed {} with children:", self.failed);

        self.declarations
            .iter()
            .for_each(|elem| {
                      elem
                          .iter()
                          .for_each(
                                |elem| elem.display(f, depth+1)
                          )
            });

        /*for dec in self.declarations {
            dec.display(f, depth + 1);
        }*/
    }
}

#[derive(Debug)]
pub enum SymbolDeclaration<'a> {
    FunctionDeclaration(FunctionDeclaration<'a>),
    VariableDeclaration(VariableDeclaration<'a>),
    NamespaceDeclaration(Namespace<'a>),
}

impl<'a> SymbolDeclaration<'a> {
    pub fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        match self {
            //Self::FunctionDeclaration(fd) => fd.display(f, depth),
            Self::VariableDeclaration(sd) => sd.display(f, depth),
            Self::NamespaceDeclaration(ns) => ns.display(f, depth),
            _ => {},
        }
    }
}


#[derive(Debug, Clone)]
pub enum Expression<'a> {
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

impl<'a> AstDisplay for Expression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        match self {
            Self::Assignment(ae) => ae.display(f, depth),
            Self::BinaryOperation(bo) => bo.display(f, depth),
            Self::UnaryOperation(uo) => uo.display(f, depth),
            Self::Comparison(ce) => ce.display(f, depth),
            Self::Identifier(ie) => ie.display(f, depth),
            Self::IntegerLiteral(il) => il.display(f, depth),
            Self::Cast(ce) => ce.display(f, depth),
            Self::MethodCall(mc) => panic!("display not implemented for method call"),
            Self::FieldAccess(fa) => panic!("display not implemented for field access"),
        }
    }
}

impl<'a> Expression<'a> {
    pub fn int_literal(input: &'a str) -> Expression<'a> {
        let span = Span {
            start: 0,
            end: 0,
            literal: "()",
        };

        let inner = IntegerLiteralExpression { contents: input };
        Expression::IntegerLiteral(inner)
    }

    pub fn identifier(input: &'a str) -> Expression<'a> {
        let span = Span {
            start: 0,
            end: 0,
            literal: input,
        };

        let inner = IdentifierExpression {
            name: input,
            node_type: None,
            //span
        };

        Expression::Identifier(inner)
    }
}

#[derive(Debug)]
pub struct FunctionDeclaration<'a> {
    pub failed: bool,
    //pub expressions: Vec<Box<dyn Expression<'a>>>,
    pub expressions: Vec<Expression<'a>>,
    pub return_type: TypeReference<'a>,
    pub params: Vec<VariableDeclaration<'a>>,
    //pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration<'a> {
    pub failed: bool,
    pub name: &'a str,
    //pub expressions: Vec<Expression<'a>>,
    //pub var_expr: Option<Box<dyn Expression<'a>>>,
    pub var_expr: Option<Box<Expression<'a>>>,
    pub var_type: Option<TypeReference<'a>>, // None indicates request for type inference
    //pub span: Span<'a>,
}

/*pub struct ParseUnit {
    namespace: Namespace,
}*/

#[derive(Debug, Clone)]
pub struct TypeReference<'a> {
    pub failed: bool,
    pub typename: &'a str,
    pub refers_to: Option<Box<Type>>,
    //pub span: Span<'a>,
}

impl<'a> TypeReference<'a> {
    pub fn unit() -> TypeReference<'a> {
        /*let span = Span {
            start: 0,
            end: 0,
            literal: "()",
        };*/

        TypeReference { failed: false, typename: "()", refers_to: None }
    }

    pub fn from_name(name: &'a str) -> TypeReference<'a> {
        /*let span = Span {
            start,
            end,
            literal: "()",
        };*/

        TypeReference { failed: false, typename: name, refers_to: None }
    }
}

impl<'a> AstDisplay for TypeReference<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}Typereference with typename {} that refers to type {:?}",
            indent(depth),
            self.typename,
            self.refers_to,
            );
    }
}

#[derive(Debug, Clone)]
pub struct AssignmentExpression<'a> {
    //lhs: Box<dyn Expression<'a>>,
    //rhs: Box<dyn Expression<'a>>,
    pub lhs: Box<Expression<'a>>,
    pub rhs: Box<Expression<'a>>,
    //pub span: Span<'a>,
}

impl<'a> AstDisplay for AssignmentExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}AssignmentExpression with child expressions:",
            indent(depth),
            );
        [&self.lhs, &self.rhs].iter().for_each(|expr| expr.display(f, depth+1));
    }
}

#[derive(Debug, Clone)]
pub struct BinaryOperationExpression<'a> {
    pub operation: BinaryOperation,
    pub lhs: Box<Expression<'a>>,
    pub rhs: Box<Expression<'a>>,
    //lhs: Box<dyn Expression<'a>>,
    //rhs: Box<dyn Expression<'a>>,
    //pub span: Span<'a>,
}

impl<'a> AstDisplay for BinaryOperationExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}BinaryOperationExpression with operation {:?} and  child expressions:",
            indent(depth),
            self.operation,
            );
        [&self.lhs, &self.rhs].iter().for_each(|expr| expr.display(f, depth+1));
    }
}

#[derive(Debug, Clone)]
pub struct ComparisonOperationExpression<'a> {
    pub operation: ComparisonOperation,
    //lhs: Box<dyn Expression<'a>>,
    //rhs: Box<dyn Expression<'a>>,
    pub lhs: Box<Expression<'a>>,
    pub rhs: Box<Expression<'a>>,
    //pub span: Span<'a>,
}

impl<'a> AstDisplay for ComparisonOperationExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}ComparisonOperationExpression with operation {:?} and child expressions:",
            indent(depth),
            self.operation,
            );
        [&self.lhs, &self.rhs].iter().for_each(|expr| expr.display(f, depth+1));
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

#[derive(Debug, Clone)]
pub enum UnaryOperation {
    Negate,
    Invert,
    Dereference,
}

#[derive(Debug, Clone)]
pub struct FieldAccess<'a> {
    pub on: Box<Expression<'a>>,
    //pub span: Span<'a>,
    pub field: &'a str,
}

#[derive(Debug, Clone)]
pub struct MethodCall<'a> {
    pub on: Box<Expression<'a>>,
    //pub span: Span<'a>,
    pub method: &'a str,
    pub arguments: Vec<Box<Expression<'a>>>,
}

#[derive(Debug, Clone)]
pub struct UnaryOperationExpression<'a> {
    pub operation: UnaryOperation,
    pub subexpr: Box<Expression<'a>>,
    //pub span: Span<'a>,
}

impl<'a> AstDisplay for UnaryOperationExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}UnaryOperationExpression with operation {:?} and child expression:",
            indent(depth),
            self.operation,
            );
        [&self.subexpr].iter().for_each(|expr| expr.display(f, depth+1));
    }
}

#[derive(Debug, Clone)]
pub struct CastExpression<'a> {
    pub subexpr: Box<Expression<'a>>,
    pub typeref: Box<TypeReference<'a>>,
    //pub span: Span<'a>,
}

impl<'a> AstDisplay for CastExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}CastExpression of expression:",
            indent(depth),
            );
        [&self.subexpr].iter().for_each(|expr| expr.display(f, depth+2));
        let _ = writeln!(
            f,
            "{}To type",
            indent(depth+1),
            );
        self.typeref.display(f, depth+2);
    }
}

#[derive(Debug)]
pub struct TernarySelectorOperationExpression<'a> {
    pub condition: Box<Expression<'a>>,
    pub first: Box<Expression<'a>>,
    pub second: Box<Expression<'a>>,
    //pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub struct Closure<'a> {
    pub failed: bool,
    pub expressions: Vec<Expression<'a>>,
    pub return_type: TypeReference<'a>,
    pub params: Vec<VariableDeclaration<'a>>,
    pub start: usize,
    pub end: usize,
    //pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub struct IdentifierExpression<'a> {
    pub name: &'a str,
    pub node_type: Option<TypeReference<'a>>,
    //pub span: Span<'a>,
}

impl<'a> AstDisplay for IdentifierExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}IdentifierExpression with name {} and type {:?}:",
            indent(depth),
            self.name,
            self.node_type,
            );
    }
}

#[derive(Debug, Clone)]
pub struct IntegerLiteralExpression<'a> {
    pub contents: &'a str,
    //pub span: Span<'a>,
}

impl<'a> AstDisplay for IntegerLiteralExpression<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}IntegerLiteralExpression with value {}",
            indent(depth),
            self.contents,
            );
    }
}

/*pub trait Type: std::fmt::Debug + std::clone::Clone {
}*/

#[derive(Debug, Clone)]
pub enum Type {
    IntegerLiteral,
    FloatLiteral,
    Class
}

pub trait AstDisplay {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize);
}

pub trait DynExpression {
}

/*pub trait Expression<'a>: std::fmt::Debug {
}

impl<'a> Expression<'a> for ComparisonOperationExpression<'a> {
}

impl<'a> Expression<'a> for BinaryOperationExpression<'a> {
}

impl<'a> Expression<'a> for IdentifierExpression<'a> {
}*/
