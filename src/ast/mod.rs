/*pub mod constants {
    // prefix UQ indicates UNQUALIFIED
    const UQ_OPERATOR_MUL: &str = "unqualified_operator_multiply"; // *
    const UQ_OPERATOR_DIV: &str = "unqualified_operator_divide"; // /
    const UQ_OPERATOR_SUB: &str = "unqualified_operator_subtract"; // -
    const UQ_OPERATOR_ADD: &str = "unqualified_operator_add"; // +
    const UQ_OPERATOR_EXP: &str = "unqualified_operator_exponentiate"; // ^

    //const UQ_COERCE_
}*/

use crate::helper::lex_wrap::*;

pub mod base;
pub mod outer;
pub mod types;
pub mod expressions;

pub use base::*;
pub use outer::*;
pub use types::*;
pub use expressions::*;

use std::cell::RefCell;
use std::rc::Rc;



/*pub mod literal {
    pub enum */

/*#[derive(Debug)]
pub struct Namespace<'a> {
    // maybe add a <symbols> member for better perf checking
    // redeclaration?
    pub public: bool,
    pub name: Option<&'a str>,
    pub contents: Option<ParseUnit<'a>>,

    //
    pub failed: bool,

    pub span: Span<'a>,
}*/

/*impl<'a> std::fmt::Debug for Namespace<'a> {
    fn fmt(&self, &mut f: std::fmt::Formatter) -> std::fmt::Result {
        write! /**/
    }
}*/

/*fn indent(ind: usize) -> String {
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
}*/

/*impl<'a> std::fmt::Display for ParseUnit<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let r = write!(f, "\n");

        self.display(f, 1);

        //self.fmt(f, 0);

        r
        //Ok(())
    }
}*/

/*impl<'a> AstDisplay for ParseUnit<'a> {
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
}*/



/*impl<'a> AstDisplay for Expression<'a> {
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
}*/


/*pub struct ParseUnit {
    namespace: Namespace,
}*/



/*#[derive(Debug, Clone)]
pub enum Type {
    IntegerLiteral,
    FloatLiteral,
    Class
}*/

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
