use super::base::*;
use super::types::*;
use super::expressions::Expression;
use super::expressions::ExpressionWrapper;

use crate::helper::lex_wrap::ParseResultError;

#[derive(Debug)]
pub struct Namespace<'a> {
    pub node_info: NodeInfo,

    pub public: bool,
    pub name: Option<&'a str>,
    pub contents: Result<ParseUnit<'a>, ParseResultError<'a>>,
}

impl<'a> Namespace<'a> {
    pub fn set_public(&mut self, public: bool) {
        self.public = public;
    }
}

impl<'a> AstNode<'a> for Namespace<'a> {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}Namespace with name {} and public {} and info {} has children:",
            indent(depth),
            self.name.unwrap_or("<unnamed>"),
            self.public,
            self.node_info,
            );

        self.contents.iter().for_each(|contents| contents.display(f, depth+1));
    }
}

#[derive(Debug)]
pub struct ParseUnit<'a> {
    pub node_info: NodeInfo,

    pub declarations: Vec<Result<SymbolDeclaration<'a>, ParseResultError<'a>>>,
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

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        //
        //
        //let _ = write!(f, "{}", indent(depth));

        findent(f, depth);
        let _ = writeln!(f, "ParseUnit {} with children:", self.node_info());

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
pub struct FunctionDeclaration<'a> {
    pub node_info: NodeInfo,

    pub expressions: Vec<Box<dyn Expression<'a>>>,
    pub return_type: TypeReference<'a>,
    pub params: Vec<VariableDeclaration<'a>>,
}

impl<'a> AstNode<'a> for FunctionDeclaration<'a> {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        panic!()
    }
}

#[derive(Debug)]
pub struct VariableDeclaration<'a> {
    pub node_info: NodeInfo,
    
    pub name: &'a str,
    pub var_expr: Option<Box<ExpressionWrapper<'a>>>,
    pub var_type: Option<TypeReference<'a>>, // None indicates request for type inference
}

impl<'a> AstNode<'a> for VariableDeclaration<'a> {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}VariableDeclaration that parsed {} with name {} and type {:?} comes from expression:",
            indent(depth),
            self.node_info(),
            self.name,
            self.var_type,
            );
        match &self.var_expr {
            Some(e) => e.as_node().display(f, depth+1),
            None => { let _ = writeln!(f, "{} unassigned", indent(depth+1)); },
        }
    }
}

#[derive(Debug)]
pub enum SymbolDeclaration<'a> {
    FunctionDeclaration(FunctionDeclaration<'a>),
    VariableDeclaration(VariableDeclaration<'a>),
    NamespaceDeclaration(Namespace<'a>),
}

impl<'a> IntoAstNode<'a> for SymbolDeclaration<'a> {
    fn as_node_mut(&mut self) -> &mut dyn AstNode<'a> {
        match self {
            Self::FunctionDeclaration(fd) => fd,
            Self::VariableDeclaration(vd) => vd,
            Self::NamespaceDeclaration(nd) => nd,
        }
    }

    fn as_node(&self) -> &dyn AstNode<'a> {
        match self {
            Self::FunctionDeclaration(fd) => fd,
            Self::VariableDeclaration(vd) => vd,
            Self::NamespaceDeclaration(nd) => nd,
        }
    }
}

impl<'a> SymbolDeclaration<'a> {
    pub fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        match self {
            //Self::FunctionDeclaration(fd) => fd.display(f, depth),
            Self::VariableDeclaration(sd) => sd.display(f, depth),
            Self::NamespaceDeclaration(ns) => ns.display(f, depth),
            _ => panic!("can't display something"),
        }
    }
}
