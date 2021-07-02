use super::base::*;
use super::expressions::ExpressionWrapper;



use crate::helper::lex_wrap::ParseResultError;
//use std::rc::Rc;
use std::sync::Arc;
//use std::cell::RefCell;

use std::sync::RwLock;
use super::expressions::TypeReference;

use crate::helper::interner::*;


#[derive(Debug)]
pub struct Namespace {
    pub node_info: NodeInfo,

    pub public: bool,
    pub name: Option<StringSymbol>,
    pub contents: Result<OuterScope, ParseResultError>,
}

impl Namespace {
    pub fn set_public(&mut self, public: bool) {
        self.public = public;
    }
}

impl AstNode for Namespace {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}Namespace with name {} and public {} and info {} has children:",
            indent(depth),
            //self.name.unwrap_or("<unnamed>"),
            //interner().try_resolve(self.name
            //self.name.map(|e| e.try_resolve()).unwrap_or("<unnamed>"),
            self.name.unwrap_or(intern("<unnamed>")).resolve(),
            self.public,
            self.node_info,
        );

        self.contents
            .iter()
            .for_each(|contents| contents.display(f, depth + 1));
    }

}

#[derive(Debug)]
pub struct OuterScope {
    pub node_info: NodeInfo,

    //pub declarations: Vec<Arc<RwLock<Result<SymbolDeclaration, ParseResultError>>>>,
    pub declarations: Vec<Arc<RwLock<SymbolDeclaration>>>,
}

impl OuterScope {
    /*pub fn prepass<'context>(&self, context: &Arc<RwLock<ScopeContext<'context, 'context>>>) where 'a: 'context {
        let mut context = context.write().unwrap();
        for dec in self.declarations.iter() {
            let dg = dec.read().unwrap();
            let cdec = dec.clone();
            context.add_definition(cdec);
        }

        std::mem::drop(context);
    }*/

    pub fn new(
        node_info: NodeInfo,
        declarations: Vec<Arc<RwLock<SymbolDeclaration>>>,
    ) -> OuterScope {
        OuterScope {
            node_info,
            declarations,
        }
    }
}

impl std::fmt::Display for OuterScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let r = write!(f, "\n");

        self.display(f, 1);

        r
    }
}

impl AstNode for OuterScope {
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
                .display(f, depth + 1)
        });
    }

    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        
        let _ = writeln!(f, "{}File {}", indent(depth), self.node_info);

        for decl in self.declarations.iter() {
            let l = decl.read();
            let _ = write!(f, "{}", indent(depth + 1));

            l.unwrap().as_node().pretty(f, depth + 1);
        }
    }
}

#[derive(Debug)]
pub struct LetComponentScopedDestructure {
    // TODO: need to pub more work into this, revisit when relevant for
    // product types down the line
}

#[derive(Debug)]
pub struct LetComponentTuple {
    pub node_info: NodeInfo,

    // pub arity: usize, // implicit in length of component vec

    pub elements: Vec<LetComponent>,

    pub type_specifier: Option<Box<TypeReference>>,
}

#[derive(Debug)]
pub struct LetComponentIdentifier {
    pub node_info: NodeInfo,

    pub identifier_string: StringSymbol,

    pub type_specifier: Option<Box<TypeReference>>,
}

#[derive(Debug)]
pub enum LetComponent {
    ScopedDestructure(LetComponentScopedDestructure),
    Tuple(LetComponentTuple),
    Identifier(LetComponentIdentifier),
    Discard(NodeInfo),
}

impl IntoAstNode for LetComponent {
    fn as_node_mut(&mut self) -> &mut dyn AstNode {
        self
    }

    fn as_node(&self) -> &dyn AstNode {
        self
    }
}

impl AstNode for LetComponent {
    fn node_info(&self) -> NodeInfo {
        match self {
            Self::ScopedDestructure(_lcsd) => todo!("LetComponentScopedDestructure not implemented for fmt"),
            Self::Discard(ni) => *ni,
            Self::Tuple(lct) => lct.node_info,
            Self::Identifier(lci) => lci.node_info,
        }
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        match self {
            Self::ScopedDestructure(_lcsd) => todo!("LetComponentScopedDestructure not implemented for fmt"),
            Self::Discard(_) => write!(f, "_").unwrap(),
            Self::Identifier(lci) => {
                write!(f, "{}: {}", lci.identifier_string.resolve(), lci.type_specifier.as_ref().map(|tr| tr.as_node()).as_node()).unwrap();
            },
            Self::Tuple(lct) => {
                write!(f, "(").unwrap();
                for idx in 0..lct.elements.len() {
                    lct.elements[idx].display(f, depth);
                    if idx < lct.elements.len() - 1 {
                        write!(f, ", ").unwrap();
                    }
                }
                /*for cmp in lct.elements.iter() {
                    cmp.display(f, depth);
                    write!(f, ", ").unwrap();
                }*/
                write!(f, ")").unwrap();
                write!(f, ": {}", lct.type_specifier.as_ref().map(|tr| tr.as_node()).as_node()).unwrap();
            },
        }
    }

}

#[derive(Debug)]
pub struct LetExpression {
    pub node_info: NodeInfo,

    //pub binding_type: TypeReference,
    pub primary_component: Box<LetComponent>,

    pub expression: Box<ExpressionWrapper>,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub node_info: NodeInfo,

    pub public: bool,
    pub name: StringSymbol,

    pub body: Box<ExpressionWrapper>,
    pub return_type: TypeReference,
    pub params: Vec<(Box<super::ExpressionWrapper>, super::TypeReference)>,
}

impl AstNode for FunctionDeclaration {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}FunctionDeclaration that parsed {} with name {}",
            indent(depth),
            self.node_info(),
            self.name.resolve(),
            //self.return_type.display(f, 0), // TODO
        );

        let _ = writeln!(f, "{}Parameters:", indent(depth + 1),);

        for vd in self.params.iter() {
            let _ = writeln!(
                f,
                "{}Parameter of type {:?} which comes from expression:",
                indent(depth + 2),
                vd.1,
            );

            vd.0.as_node().display(f, depth + 3);
        }

        if self.params.is_empty() {
            let _ = writeln!(f, "{}(None)", indent(depth + 3));
        }

        let _ = write!(f, "{}Return type:", indent(depth + 1));

        self.return_type.display(f, depth + 3);
        let _ = writeln!(f);

        let _ = writeln!(f, "{}And body:", indent(depth + 1),);

        self.body.as_node().display(f, depth + 2);
    }

}

#[derive(Debug)]
pub struct StructDeclaration {
    pub node_info: NodeInfo,

    pub public: bool,
    pub name: StringSymbol,
    pub typeparams: Vec<StringSymbol>,
    pub fields: Vec<(
        StringSymbol,
        TypeReference,
        Option<Box<super::ExpressionWrapper>>,
    )>,
}

impl AstNode for StructDeclaration {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}StructDeclaration that parsed {} with name {} has fields:",
            indent(depth),
            self.node_info(),
            self.name.resolve(),
            //interner().resolve(self.name),
        );

        //write!(f, "{}", indent(depth + 1));
        for (name, tr, exp) in self.fields.iter() {
            let _ = write!(f, "{}{} : {:?} = ", indent(depth + 1), name.resolve(), tr,);
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

#[derive(Debug)]
pub struct UseDeclaration {
    pub node_info: NodeInfo,

    pub public: bool,

    pub scope: Vec<StringSymbol>,
}

impl UseDeclaration {
}

impl AstNode for UseDeclaration {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let uses: Vec<&'static str> = self.scope.iter().map(|ss| ss.resolve()).collect();
        let _ = writeln!(f, "{}UseDeclaration of {:?} as {}",
                 indent(depth),
                 uses,
                 "<unaliased>",
                 );
    }

}

#[derive(Debug)]
pub struct ScopedNameReference {
    pub node_info: NodeInfo,

    pub scope: Vec<StringSymbol>,

    pub silent: bool,
}

impl ScopedNameReference {
    pub fn to_raw_scope(&self) -> ScopedName {
        ScopedName::new(self.scope.clone())
    }
}

impl AstNode for ScopedNameReference {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, _depth: usize) {
        for idx in 0..self.scope.len() {
            write!(f, "{}", self.scope[idx].resolve()).unwrap();
            if idx < self.scope.len() - 1 {
                write!(f, "::").unwrap();
            }
        }
    }

}

impl IntoAstNode for ScopedNameReference {
    fn as_node_mut(&mut self) -> &mut dyn AstNode {
        self
    }

    fn as_node(&self) -> &dyn AstNode {
        self
    }
}

#[derive(Debug, PartialEq, Hash)]
pub struct ScopedName {
    pub scope: Vec<StringSymbol>,
}

impl ScopedName {
    pub fn new(s: Vec<StringSymbol>) -> ScopedName {
        ScopedName { scope: s }
    }
}

#[derive(Debug)]
pub struct StaticVariableDeclaration {
    pub node_info: NodeInfo,

    pub public: bool,
    pub expression: Box<ExpressionWrapper>,
}

impl AstNode for StaticVariableDeclaration {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        self.expression.as_node().display(f, depth);
    }

}

#[derive(Debug)]
pub enum SymbolDeclaration {
    FunctionDeclaration(FunctionDeclaration),
    NamespaceDeclaration(Namespace),
    StructDeclaration(StructDeclaration),
    ExpressionDeclaration(StaticVariableDeclaration),
    UseDeclaration(UseDeclaration),
    //VariableDeclaration(VariableDeclaration),
}

impl IntoAstNode for SymbolDeclaration {
    fn as_node_mut(&mut self) -> &mut dyn AstNode {
        match self {
            Self::FunctionDeclaration(fd) => fd,
            Self::NamespaceDeclaration(nd) => nd,
            Self::StructDeclaration(sd) => sd,
            Self::ExpressionDeclaration(ed) => ed,
            Self::UseDeclaration(ud) => ud,
        }
    }

    fn as_node(&self) -> &dyn AstNode {
        match self {
            Self::FunctionDeclaration(fd) => fd,
            Self::NamespaceDeclaration(nd) => nd,
            Self::StructDeclaration(sd) => sd,
            Self::ExpressionDeclaration(ed) => ed,
            Self::UseDeclaration(ud) => ud,
        }
    }
}

impl SymbolDeclaration {
    pub fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        match self {
            //Self::FunctionDeclaration(fd) => fd.display(f, depth),
            Self::FunctionDeclaration(fd) => fd.display(f, depth),
            Self::NamespaceDeclaration(ns) => ns.display(f, depth),
            Self::StructDeclaration(sd) => sd.display(f, depth),
            Self::ExpressionDeclaration(sd) => sd.display(f, depth),
            Self::UseDeclaration(ud) => ud.display(f, depth),
        }
    }

    pub fn mark_public(&mut self) {
        match self {
            Self::FunctionDeclaration(v) => v.public = true,
            Self::NamespaceDeclaration(v) => v.public = true,
            Self::StructDeclaration(v) => v.public = true,
            Self::ExpressionDeclaration(v) => v.public = true,
            Self::UseDeclaration(v) => v.public = true,
        }
    }

    pub fn is_public(&self) -> bool {
        match self {
            Self::FunctionDeclaration(fd) => fd.public,
            Self::NamespaceDeclaration(fd) => fd.public,
            Self::StructDeclaration(fd) => fd.public,
            Self::ExpressionDeclaration(fd) => fd.public,
            Self::UseDeclaration(fd) => fd.public,
        }
    }

    pub fn symbol_name(&self) -> Option<StringSymbol> {
        match self {
            Self::FunctionDeclaration(fd) => Some(fd.name),
            Self::NamespaceDeclaration(ns) => ns.name,
            Self::StructDeclaration(sd) => Some(sd.name),
            Self::ExpressionDeclaration(_ed) => None, // no symbol to export
            Self::UseDeclaration(_ud) => None,
        }
    }

    pub fn is_context(&self) -> bool {
        match self {
            Self::NamespaceDeclaration(_) => true,
            _ => false,
        }
    }

    pub fn symbols(&self) -> &[Arc<RwLock<SymbolDeclaration>>] {
        match self {
            Self::NamespaceDeclaration(ns) => match ns.contents.as_ref() {
                Ok(contents) => &contents.declarations[..],
                Err(_) => &[],
            },
            _ => &[],
        }
    }

    /*pub fn as_ns(&self) -> Option<&Namespace<'a>> {
        match self {
            Self::Nam
        }
    }*/
}
