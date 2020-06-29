use super::base::*;
use super::expressions::ExpressionWrapper;
use super::types::*;

use crate::helper::lex_wrap::ParseResultError;
//use std::rc::Rc;
use std::sync::Arc;
//use std::cell::RefCell;
use std::sync::RwLock;
use crate::mid_repr::*;

#[derive(Debug)]
pub struct Namespace<'a> {
    pub node_info: NodeInfo,

    pub public: bool,
    pub name: Option<&'a str>,
    pub contents: Result<OuterScope<'a>, ParseResultError<'a>>,
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

        self.contents
            .iter()
            .for_each(|contents| contents.display(f, depth + 1));
    }
}

#[derive(Debug)]
pub struct OuterScope<'a> {
    pub node_info: NodeInfo,

    //pub declarations: Vec<Arc<RwLock<Result<SymbolDeclaration<'a>, ParseResultError<'a>>>>>,
    pub declarations: Vec<Arc<RwLock<SymbolDeclaration<'a>>>>,
}

impl<'a> OuterScope<'a> {
    /*pub fn prepass<'context>(&self, context: &Arc<RwLock<ScopeContext<'context, 'context>>>) where 'a: 'context {
        let mut context = context.write().unwrap();
        for dec in self.declarations.iter() {
            let dg = dec.read().unwrap();
            let cdec = dec.clone();
            context.add_definition(cdec);
        }

        std::mem::drop(context);
    }*/

    pub fn new(node_info: NodeInfo, declarations: Vec<Arc<RwLock<SymbolDeclaration<'a>>>>) -> OuterScope<'a> {
        OuterScope {
            node_info, declarations,
        }
    }
}

impl<'a> std::fmt::Display for OuterScope<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let r = write!(f, "\n");

        self.display(f, 1);

        r
    }
}

impl<'a> AstNode<'a> for OuterScope<'a> {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        //
        //
        //let _ = write!(f, "{}", indent(depth));

        findent(f, depth);
        let _ = writeln!(f, "ParseUnit {} with children:", self.node_info());

        self.declarations.iter().for_each(|elem| {
            elem.read()
                .expect("locking error within parsunit display")
                //.iter()
                //.for_each(|elem| elem.display(f, depth + 1))
                .display(f, depth+1)
        });

        /*for dec in self.declarations {
            dec.display(f, depth + 1);
        }*/
    }
}

#[derive(Debug)]
pub struct FunctionDeclaration<'a> {
    pub node_info: NodeInfo,

    pub public: bool,
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

        let _ = writeln!(f, "{}Parameters:", indent(depth + 1),);

        //write!(f, "{}", indent(depth + 1));
        for vd in self.params.iter() {
            let _ = writeln!(
                f,
                "{}Parameter of type {:?} which comes from expression:",
                indent(depth + 2),
                vd.1,
            );

            vd.0.as_node().display(f, depth + 3);
        }
        let _ = writeln!(f, "{}And body:", indent(depth + 1),);

        self.body.as_node().display(f, depth + 2);
    }
}

#[derive(Debug)]
pub struct StructDeclaration<'a> {
    pub node_info: NodeInfo,

    pub public: bool,
    pub name: &'a str,
    pub typeparams: Vec<&'a str>,
    pub fields: Vec<(
        &'a str,
        TypeReference<'a>,
        Option<Box<super::ExpressionWrapper<'a>>>,
    )>,
}

impl<'a> AstNode<'a> for StructDeclaration<'a> {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}StructDeclaration that parsed {} with name {} has fields:",
            indent(depth),
            self.node_info(),
            self.name,
        );

        //write!(f, "{}", indent(depth + 1));
        for (name, tr, exp) in self.fields.iter() {
            let _ = write!(f, "{}{} : {:?} = ", indent(depth + 1), name, tr,);
            match exp {
                None => {
                    let _ = writeln!(f, "<undefined>",);
                }
                Some(exp) => {
                    let _ = writeln!(f);
                    exp.as_node().display(f, depth + 2);
                }
            }
        }
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
pub struct ScopedNameReference<'a> {
    pub node_info: NodeInfo,

    pub scope: Vec<&'a str>,

    pub silent: bool,
}

impl<'a> ScopedNameReference<'a> {
    pub fn to_raw_scope(&self) -> ScopedName<'a> {
        ScopedName::new(self.scope.clone())
    }
}

/*impl<'a> std::cmp::PartialEq for ScopedNameReference<'a> {
    fn eq(&self, other: &ScopedNameReference) -> bool {
        other.scope == self.scope
    }
}*/

#[derive(Debug, PartialEq, Hash)]
pub struct ScopedName<'a> {
    pub scope: Vec<&'a str>,
}

impl<'a> ScopedName<'a> {
    pub fn new(s: Vec<&'a str>) -> ScopedName<'a> {
        ScopedName { scope: s }
    }
}

#[derive(Debug)]
pub struct StaticVariableDeclaration<'a> {
    pub node_info: NodeInfo,

    pub public: bool,
    pub expression: Box<ExpressionWrapper<'a>>,
}

impl<'a> AstNode<'a> for StaticVariableDeclaration<'a> {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        self.expression.as_node().display(f, depth);
    }
}

#[derive(Debug)]
pub enum SymbolDeclaration<'a> {
    FunctionDeclaration(FunctionDeclaration<'a>),
    NamespaceDeclaration(Namespace<'a>),
    StructDeclaration(StructDeclaration<'a>),
    ExpressionDeclaration(StaticVariableDeclaration<'a>),
    //VariableDeclaration(VariableDeclaration<'a>),
}

impl<'a> IntoAstNode<'a> for SymbolDeclaration<'a> {
    fn as_node_mut(&mut self) -> &mut dyn AstNode<'a> {
        match self {
            Self::FunctionDeclaration(fd) => fd,
            Self::NamespaceDeclaration(nd) => nd,
            Self::StructDeclaration(sd) => sd,
            Self::ExpressionDeclaration(ed) => ed,
        }
    }

    fn as_node(&self) -> &dyn AstNode<'a> {
        match self {
            Self::FunctionDeclaration(fd) => fd,
            Self::NamespaceDeclaration(nd) => nd,
            Self::StructDeclaration(sd) => sd,
            Self::ExpressionDeclaration(ed) => ed,
        }
    }
}

impl<'a> SymbolDeclaration<'a> {
    pub fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        match self {
            //Self::FunctionDeclaration(fd) => fd.display(f, depth),
            Self::FunctionDeclaration(fd) => fd.display(f, depth),
            Self::NamespaceDeclaration(ns) => ns.display(f, depth),
            Self::StructDeclaration(sd) => sd.display(f, depth),
            Self::ExpressionDeclaration(sd) => sd.display(f, depth),
        }
    }

    pub fn mark_public(&mut self) {
        match self {
            Self::FunctionDeclaration(fd) => fd.public = true,
            Self::NamespaceDeclaration(fd) => fd.public = true,
            Self::StructDeclaration(fd) => fd.public = true,
            Self::ExpressionDeclaration(fd) => fd.public = true,
        }
    }

    pub fn is_public(&self) -> bool {
        match self {
            Self::FunctionDeclaration(fd) => fd.public,
            Self::NamespaceDeclaration(fd) => fd.public,
            Self::StructDeclaration(fd) => fd.public,
            Self::ExpressionDeclaration(fd) => fd.public,
        }
    }

    pub fn symbol_name(&self) -> Option<&'a str> {
        match self {
            Self::FunctionDeclaration(fd) => Some(fd.name),
            Self::NamespaceDeclaration(ns) => ns.name,
            Self::StructDeclaration(sd) => Some(sd.name),
            Self::ExpressionDeclaration(_ed) => None,
        }
    }

    pub fn is_context(&self) -> bool {
        match self {
            Self::NamespaceDeclaration(_) => true,
            _ => false,
        }
    }

    pub fn symbols(&self) -> &[Arc<RwLock<SymbolDeclaration<'a>>>] {
        match self {
            Self::NamespaceDeclaration(ns) => {
                match ns.contents.as_ref() {
                    Ok(contents) => {
                        &contents.declarations[..]
                    },
                    Err(_) => &[],
                }
            }
            _ => &[],
        }
    }

    /*pub fn as_ns(&self) -> Option<&Namespace<'a>> {
        match self {
            Self::Nam
        }
    }*/
}
