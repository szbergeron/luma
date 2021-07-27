use super::base::*;
use super::outer::*;
use crate::helper::interner::*;
//use super::types;

use crate::helper::lex_wrap::{ParseResultError, TokenWrapper};
use crate::lex::Token;
use crate::types;

pub trait Expression: AstNode {
    fn expr_type(&self) -> Box<dyn types::Type>;
}

#[derive(Debug, Clone)]
pub struct TypeReference {
    node_info: NodeInfo,

    pub ctx: ScopedNameReference,
    pub canonicalized_name: StringSymbol,

    pub type_args: Vec<Box<TypeReference>>,
}

impl TypeReference {
    pub fn new(ctx: ScopedNameReference, name: StringSymbol) -> TypeReference {
        TypeReference {
            node_info: NodeInfo::Builtin,
            ctx,
            type_args: Vec::new(),
            canonicalized_name: name,
        }
    }
}

impl AstNode for TypeReference {
    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        let _ = write!(f, "{}", self.canonicalized_name);
        if !self.type_args.is_empty() {
            let _ = write!(f, "<args: !impl>");
        }
    }
    fn display(&self, f: &mut std::fmt::Formatter<'_>, _depth: usize) {
        let _ = write!(
            f,
            "{}",
            self.ctx.as_node(),
            //self.canonicalized_name.resolve() //interner().resolve(&self.canonicalized_name)
        );
        if !self.type_args.is_empty() {
            write!(f, "<").unwrap();
            for idx in 0..self.type_args.len() {
                write!(f, "{}", self.type_args[idx].as_node()).unwrap();
                if idx < self.type_args.len() - 1 {
                    write!(f, ", ").unwrap();
                }
            }
            write!(f, ">").unwrap();
        }
        /*
        [&self.subexpr]
            .iter()
            .for_each(|expr| expr.as_node().display(f, depth + 1));*/
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

}

impl IntoAstNode for TypeReference {
    /*fn as_node_mut(&mut self) -> &mut dyn AstNode {
        self
    }*/

    fn as_node(&self) -> &dyn AstNode {
        self
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionWrapper {
    Assignment(AssignmentExpression),
    BinaryOperation(BinaryOperationExpression),
    UnaryOperation(UnaryOperationExpression),
    Comparison(ComparisonOperationExpression),
    Cast(CastExpression),
    Literal(LiteralExpression),
    MethodCall(MethodCall),
    Access(AccessExpression),
    Statement(StatementExpression),
    Block(BlockExpression),
    IfThenElse(IfThenElseExpression),
    While(WhileExpression),
    LetExpression(LetExpression),
    Pattern(Pattern),
    Return(ReturnExpression),
    Wildcard(WildcardExpression),
    LLVMLiteral(LLVMLiteralExpression),
}

impl ExpressionWrapper {
    //pub fn int_literal(input: &'a str) -> ExpressionWrapper {
    pub fn literal_expression(input: TokenWrapper) -> Box<ExpressionWrapper> {
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

    /*pub fn identifier_expression(input: TokenWrapper) -> Box<ExpressionWrapper> {
        let node_info = NodeInfo::from_token(&input, true);

        let inner = IdentifierExpression {
            node_info,
            name: input.slice,
            node_type: None,
            //span
        };

        Box::new(ExpressionWrapper::Identifier(inner))
    }*/

    pub fn wildcard(input: TokenWrapper) -> Box<ExpressionWrapper> {
        let node_info = NodeInfo::from_token(&input);

        WildcardExpression::new_expr(node_info)

        //Box::new(ExpressionWrapper::
    }
}

impl IntoAstNode for ExpressionWrapper {
    fn as_node(&self) -> &dyn AstNode {
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
            Self::LLVMLiteral(e) => e,
            _ => {
                println!("No implemented as_node handler for type {:?}", self);
                todo!();
            }
        }
    }

    /*fn as_node_mut(&mut self) -> &mut dyn AstNode {
        todo!()
    }*/
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

impl AstNode for WildcardExpression {
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

    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        let _ = writeln!(f, "*");
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

impl AstNode for StatementExpression {
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

    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        self.subexpr.as_node().pretty(f, depth);
    }
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub node_info: NodeInfo,

    //on: &'a str,
    pub expressions: Vec<Box<ExpressionWrapper>>,
}

impl Pattern {
    pub fn new_expr(
        node_info: NodeInfo,
        expressions: Vec<Box<ExpressionWrapper>>,
    ) -> Box<ExpressionWrapper> {
        Box::new(ExpressionWrapper::Pattern(Pattern {
            node_info,
            expressions,
        }))
    }
}

impl IntoAstNode for Pattern {
    fn as_node(&self) -> &dyn AstNode {
        self
    }
}

impl AstNode for Pattern {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(f, "{}Pattern with child expressions:", indent(depth),);
        self.expressions
            .iter()
            .for_each(|expr| expr.as_node().display(f, depth + 1));
    }

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

#[derive(Debug, Clone)]
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

impl AstNode for WhileExpression {
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

    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        let _ = write!(f, "while(");
        self.if_exp.as_node().pretty(f, depth);
        let _ = writeln!(f, ") {{");
        let _ = write!(f, "{}", indent(depth + 1));
        self.then_exp.as_node().pretty(f, depth + 1);
        let _ = writeln!(f, "\n}}");
    }
}

#[derive(Debug, Clone)]
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

impl AstNode for IfThenElseExpression {
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

    pub contents: Vec<Result<Box<ExpressionWrapper>, ParseResultError>>,
}

impl AstNode for BlockExpression {
    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        let _ = writeln!(f, "{{");
        for c in self.contents.iter() {
            if let Ok(c) = c {
                let _ = write!(f, "{}", indent(depth + 1));
                c.as_node().pretty(f, depth + 1);
                let _ = writeln!(f, "");
            }
        }

        let _ = write!(f, "{}}}", indent(depth));
    }
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

impl BlockExpression {
    pub fn new_expr(
        node_info: NodeInfo,
        contents: Vec<Result<Box<ExpressionWrapper>, ParseResultError>>,
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
}*/

/*impl LetExpression {
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

impl AstNode for LetExpression {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}LetExpression that assigns into {} from:",
            indent(depth),
            self.primary_component.as_node()
        );

        /*write!(f, "{}", indent(depth + 1)).unwrap();
        self.primary_component.display(f, depth+1);
        writeln!(f, "");*/

        [&self.expression.as_node()]
            .iter()
            .for_each(|expr| expr.display(f, depth + 1));
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

}

#[derive(Debug, Clone)]
pub struct AssignmentExpression {
    node_info: NodeInfo,
    //lhs: Box<dyn Expression>,
    //rhs: Box<dyn Expression>,
    //pub is_let_expression: bool,
    pub lhs: Box<ExpressionWrapper>,
    pub rhs: Box<ExpressionWrapper>,
    //pub span: Span,
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

impl AstNode for AssignmentExpression {
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

#[derive(Debug, Clone)]
pub struct BinaryOperationExpression {
    node_info: NodeInfo,

    pub operation: BinaryOperation,
    pub lhs: Box<ExpressionWrapper>,
    pub rhs: Box<ExpressionWrapper>,
    //lhs: Box<dyn Expression>,
    //rhs: Box<dyn Expression>,
    //pub span: Span,
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

impl AstNode for BinaryOperationExpression {
    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        self.lhs.as_node().pretty(f, depth);
        let _ = write!(f, " {} ", self.operation.fmt());
        self.rhs.as_node().pretty(f, depth);
    }
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

#[derive(Debug, Clone)]
pub struct ComparisonOperationExpression {
    node_info: NodeInfo,

    pub operation: ComparisonOperation,
    //lhs: Box<dyn Expression>,
    //rhs: Box<dyn Expression>,
    pub lhs: Box<ExpressionWrapper>,
    pub rhs: Box<ExpressionWrapper>,
    //pub span: Span,
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

impl AstNode for ComparisonOperationExpression {
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

#[derive(Debug, Clone)]
pub struct AccessExpression {
    pub node_info: NodeInfo,

    pub on: Option<Box<ExpressionWrapper>>,
    //pub span: Span,
    //pub field: &'a str,
    pub scope: Box<ScopedNameReference>,
    pub pattern: Option<Pattern>,
}

impl AccessExpression {
    pub fn new_expr(
        node_info: NodeInfo,
        on: Option<Box<ExpressionWrapper>>,
        scope: Box<ScopedNameReference>,
        pattern: Option<Pattern>,
    ) -> Box<ExpressionWrapper> {
        Box::new(ExpressionWrapper::Access(AccessExpression {
            node_info,
            scope,
            on,
            pattern,
        }))
    }
}

impl AstNode for AccessExpression {
    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        self.scope.as_node().pretty(f, depth);
        self.on.iter().for_each(|on| on.as_node().pretty(f, depth));
        self.pattern.iter().for_each(|pattern| pattern.as_node().pretty(f, depth));
    }
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

#[derive(Debug, Clone)]
pub struct MethodCall {
    node_info: NodeInfo,

    pub on: Box<ExpressionWrapper>,
    //pub span: Span,
    pub method: StringSymbol,
    pub arguments: Vec<Box<ExpressionWrapper>>,
}

#[derive(Debug, Clone)]
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

impl AstNode for UnaryOperationExpression {
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
    node_info: NodeInfo,

    /// This allows choosing string aliases for
    /// a set of expressions that are to be evaluated and
    /// placed in llvm bindings corresponding with the StringSymbols
    /// provided.
    ///
    /// No additional munging is done so the user should be careful to avoid
    /// any naming conflicts here
    renames: Vec<(ExpressionWrapper, StringSymbol)>,

    /// Contains the LLVM IR text that is to be emitted with this function
    text: StringSymbol,

    /// If this block is not `_ -> ()` then it has some output T
    ///
    /// The type is specified syntactically and resides in
    /// output.unwrap().0, and the llvm binding that
    /// the value will reside in will be referred to output.unwrap().1
    ///
    /// Care should be taken by the user that the binding name does not cause a name collision
    output: Option<(TypeReference, StringSymbol)>
}

impl AstNode for LLVMLiteralExpression {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        todo!()
    }
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

impl AstNode for ReturnExpression {
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

impl AstNode for CastExpression {
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

/*#[derive(Debug)]
pub struct IdentifierExpression {
    pub node_info: NodeInfo,

    //pub name: &'a str,
    pub context: Box<ScopedName>,
    pub node_type: Option<types::TypeReference>,
    //pub span: Span,
}

impl AstNode for IdentifierExpression {
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

#[derive(Debug, Clone)]
#[allow(non_camel_case_types)]
pub enum Literal {
    StringLiteral(StringSymbol),

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

#[derive(Debug, Clone)]
pub struct LiteralExpression {
    node_info: NodeInfo,

    //pub contents: &'a str,
    pub contents: Literal,
    //pub span: Span,
}

impl LiteralExpression {
    pub fn new_expr(tw: TokenWrapper) -> Box<ExpressionWrapper> {
        let literal = match tw.token {
            Token::UnknownIntegerLiteral => {
                Literal::UnknownIntegerLiteral(tw.slice.resolve().parse().unwrap())
            }
            Token::i8Literal => Literal::i8Literal(tw.slice.resolve().parse().unwrap()),
            Token::StringLiteral => Literal::StringLiteral(tw.slice),
            _ => todo!(),
        };

        Box::new(ExpressionWrapper::Literal(LiteralExpression {
            node_info: NodeInfo::from_token(&tw),
            contents: literal,
        }))
    }
}

impl AstNode for LiteralExpression {
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
