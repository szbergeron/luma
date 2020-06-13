use super::base::*;
use super::types::*;
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
    pub name: &'a str,

    //pub expressions: Vec<Box<dyn Expression<'a>>>,
    pub body: Box<ExpressionWrapper<'a>>,
    pub return_type: TypeReference<'a>,
    pub params: Vec<(Box<super::ExpressionWrapper<'a>>, super::TypeReference<'a>)>,
}

impl<'a> AstNode<'a> for FunctionDeclaration<'a> {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}FunctionDeclaration that parsed {} with name {} and rtype {:?} takes",
            indent(depth),
            self.node_info(),
            self.name,
            self.return_type,
            );

        let _ = writeln!(
            f,
            "{}Parameters:",
            indent(depth+1),
            );

        //write!(f, "{}", indent(depth + 1));
        for vd in self.params.iter() {
            let _ = writeln!(
                f,
                "{}Parameter of type {:?} which comes from expression:",
                indent(depth+2),
                vd.1,
            );

            vd.0.as_node().display(f, depth+3);
        }
        let _ = writeln!(
            f,
            "{}And body:",
            indent(depth+1),
            );

        self.body.as_node().display(f, depth+2);
    }
}

/*#[derive(Debug)]
pub struct VariableDeclaration<'a> {
    pub node_info: NodeInfo,
    
    //pub name: &'a str,
    pub lhs: Box<ExpressionWrapper<'a>>,
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
            "{}VariableDeclaration that parsed {} has lhs:",
            indent(depth),
            self.node_info(),
            );
        self.lhs.as_node().display(f, depth+2);
        let _ = writeln!(
            f,
            "{}And type {:?} comes from expression:",
            indent(depth+1),
            self.var_type,
            );

        match &self.var_expr {
            Some(e) => e.as_node().display(f, depth+2),
            None => { let _ = writeln!(f, "{} unassigned", indent(depth+2)); },
        }
    }
}*/

#[derive(Debug)]
pub struct ScopedName<'a> {
    pub node_info: NodeInfo,

    pub scope: Vec<&'a str>,

    pub silent: bool,
}



#[derive(Debug)]
pub enum SymbolDeclaration<'a> {
    FunctionDeclaration(FunctionDeclaration<'a>),
    NamespaceDeclaration(Namespace<'a>),
    ExpressionDeclaration(Box<ExpressionWrapper<'a>>),
    //VariableDeclaration(VariableDeclaration<'a>),
}

impl<'a> IntoAstNode<'a> for SymbolDeclaration<'a> {
    fn as_node_mut(&mut self) -> &mut dyn AstNode<'a> {
        match self {
            Self::FunctionDeclaration(fd) => fd,
            Self::NamespaceDeclaration(nd) => nd,
            Self::ExpressionDeclaration(ed) => ed.as_node_mut(),
        }
    }

    fn as_node(&self) -> &dyn AstNode<'a> {
        match self {
            Self::FunctionDeclaration(fd) => fd,
            Self::NamespaceDeclaration(nd) => nd,
            Self::ExpressionDeclaration(ed) => ed.as_node(),
        }
    }
}

impl<'a> SymbolDeclaration<'a> {
    pub fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        match self {
            //Self::FunctionDeclaration(fd) => fd.display(f, depth),
            Self::FunctionDeclaration(fd) => fd.display(f, depth),
            Self::NamespaceDeclaration(ns) => ns.display(f, depth),
            Self::ExpressionDeclaration(sd) => sd.as_node().display(f, depth),
        }
    }
}
