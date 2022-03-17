use super::EnumDefinition;
use super::FunctionDefinition;
use super::Implementation;
use super::StructDefinition;
use super::TraitDefinition;
use super::base::*;
use super::expressions::ExpressionWrapper;
use super::TypeDefinition;
use super::TypeReference;
use crate::helper::VecOps;
use crate::types::FunctionDeclaration;
use std::fmt::Debug;
use std::io::Write;

use crate::lex::ParseResultError;
use crate::types::GlobalCtxNode;
use crate::types::Import;
use crate::types::Resolution;
use std::pin::Pin;
//use std::rc::Rc;

//use std::cell::RefCell;

//use super::expressions::TypeReference;

use crate::helper::interner::*;
use async_recursion::async_recursion;
use futures::future::join_all;
use indent::indent_by;
use pretty::Doc;
use pretty::RcDoc;

#[derive(Debug)]
pub struct Namespace {
    pub node_info: NodeInfo,

    pub public: bool,
    pub name: Option<IStr>,
    pub contents: OuterScope,
}

impl Namespace {
    pub fn set_public(&mut self, public: bool) {
        self.public = public;
    }

    #[async_recursion]
    /// Unsafe: requires &parent and &global to live at least as long as self, even if this is not
    /// specified
    pub async unsafe fn into_ctx(
        self,
        parent_scope: &[IStr],
        parent: &GlobalCtxNode,
        global: &GlobalCtxNode,
    ) -> Pin<Box<GlobalCtxNode>> {
        //let new_slice = parent_scope.to_vec().appended(self.name.unwrap());

        //let ctx = GlobalCtx::get().get_nsctx(new_slice.as_slice());
        /*let ctx = GlobalCtxNode::new(
            *parent_scope
                .last()
                .expect("parent_scope had no last member"),
            Some(parent),
            Some(global),
            None,
        );*/

        /*if let Ok(outer) = self.contents {
            for dec in outer.declarations.into_iter() {
            }
        }*/

        //let sym = self.as_symbol();

        //let mut ctxs = Vec::new();

        if let os = self.contents {
            return os
                .into_ctx(
                    parent_scope.to_vec().appended_opt(self.name),
                    parent,
                    global,
                )
                .await;
        } else {
            todo!("Need to handle unhappy path for parse failure");
        }

        // force destructuring the rest of the subtree
        //let joined: Vec<Pin<Box<GlobalCtxNode>>> = join_all(ctxs).await;

        /*for cctx in joined {
            ctx.add_child(cctx);
        }*/

        /*if let Ok(content) = self.contents.as {
            for dec in content.declarations.iter() {
                match dec {
                    SymbolDeclaration::FunctionDeclaration(fd) => {
                        //ctx.func_ctx().define(
                        ctx.func_ctx().add(fd);
                    },
                    SymbolDeclaration::NamespaceDeclaration(nd) => {
                        nd.into_ctx(&new_slice);
                    },
                    _ => todo!(),
                }
            }
        }*/

        //self.contents.map(|outer| outer.into_ctxlist(new_slice.as_slice())
        //todo!()
        //ctx
    }

    /*fn as_symbol(&self) -> SymbolDeclaration {
        SymbolDeclaration::NamespaceDeclaration(Arc::new(self))
    }*/
}

impl AstNode for Namespace {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        let _ = writeln!(
            f,
            "mod {} {{",
            self.name.map(|ss| ss.resolve()).unwrap_or("<unknown")
        );

        if let os = &self.contents {
            os.pretty(f, depth + 1);
        }
        let _ = write!(f, "{}}}", indent(depth));
    }
}

pub struct OuterScope {
    pub node_info: NodeInfo,

    //pub declarations: Vec<Arc<RwLock<Result<SymbolDeclaration, ParseResultError>>>>,
    pub declarations: Vec<TopLevel>,
}

impl Debug for OuterScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("OuterScope")
            .field("declarations", &self.declarations)
            .finish()
    }
}

impl OuterScope {
    #[async_recursion]
    pub async unsafe fn into_ctx(
        self,
        module: Vec<IStr>,
        parent: &GlobalCtxNode,
        global: &GlobalCtxNode,
    ) -> Pin<Box<GlobalCtxNode>> {
        let ctx = GlobalCtxNode::new(
            module.last().cloned().unwrap_or_else(|| intern("")),
            Some(parent),
            Some(global),
            None,
        );

        let mut ctxs = Vec::new();

        let items = self.declarations;

        for dec in items.into_iter() {
            match dec {
                TopLevel::FunctionDeclaration(fd) => {
                    //ctx.func_ctx().define(
                    ctx.func_ctx().add(fd);
                }
                TopLevel::NamespaceDeclaration(nd) => {
                    ctxs.push(nd.into_ctx(&module, parent, global));
                }
                TopLevel::UseDeclaration(ud) => {
                    ctx.import(ud.into_import());
                }
                TopLevel::Struct(sd) => {
                    ctx.type_ctx().define_struct(sd);
                }
                TopLevel::Enum(e) => {
                    todo!()
                }
                TopLevel::Implementation(i) => {
                    todo!()
                }
                _ => todo!(),
            }
        }

        // force destructuring the rest of the subtree
        let joined: Vec<Pin<Box<GlobalCtxNode>>> = join_all(ctxs).await;

        for cctx in joined {
            ctx.add_child(cctx);
        }

        ctx
    }
    /*pub fn prepass<'context>(&self, context: &Arc<RwLock<ScopeContext<'context, 'context>>>) where 'a: 'context {
        let mut context = context.write().unwrap();
        for dec in self.declarations.iter() {
            let dg = dec.read().unwrap();
            let cdec = dec.clone();
            context.add_definition(cdec);
        }

        std::mem::drop(context);
    }*/

    pub fn new(node_info: NodeInfo, declarations: Vec<TopLevel>) -> OuterScope {
        OuterScope {
            node_info,
            declarations,
        }
    }
}

impl AstNode for OuterScope {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        //let _ = writeln!(f, "{}File {}", indent(depth), self.node_info);

        for decl in self.declarations.iter() {
            let l = decl;
            let _ = write!(f, "{}", indent(depth + 1));

            l.as_node().pretty(f, depth + 1);
            let _ = writeln!(f, "");
        }
    }
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

    pub type_specifier: Option<Box<TypeReference>>,
}

#[derive(Debug, Clone)]
pub struct LetComponentIdentifier {
    pub node_info: NodeInfo,

    pub identifier_string: IStr,

    pub type_specifier: Option<Box<TypeReference>>,
}

#[derive(Debug, Clone)]
pub enum LetComponent {
    ScopedDestructure(LetComponentScopedDestructure),
    Tuple(LetComponentTuple),
    Identifier(LetComponentIdentifier),
    Discard(NodeInfo),
}

impl IntoAstNode for LetComponent {
    /*fn as_node_mut(&mut self) -> Option<&mut dyn AstNode> {
        Some(self)
    }*/

    fn as_node(&self) -> &dyn AstNode {
        self
    }
}

impl AstNode for LetComponent {
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

#[derive(Debug, Clone)]
pub struct LetExpression {
    pub node_info: NodeInfo,

    pub primary_component: Box<LetComponent>,

    pub expression: Box<ExpressionWrapper>,
}
//pub struct

/*#[derive(Debug)]
pub struct TypeDefinition {
    pub node_info: NodeInfo,

    pub public: bool,
    pub name: IStr,
    pub typeparams: Vec<IStr>,
    pub fields: Vec<(
        IStr,
        TypeReference,
        Option<Box<super::ExpressionWrapper>>,
    )>,
}

impl AstNode for TypeDefinition {
    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        let _ = writeln!(f, "struct {} {{", self.name);
        for (field, ftype, _) in self.fields.iter() {
            let _ = write!(f, "{}{}: ", indent(depth + 1), field);
            ftype.pretty(f, depth + 1);
            let _ = writeln!(f, "");
        }
        let _ = write!(f, "{}}}", indent(depth));
    }
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
}*/

#[derive(Debug)]
pub struct UseDeclaration {
    pub node_info: NodeInfo,

    pub public: bool,

    pub scope: Vec<IStr>,

    pub alias: Option<IStr>,
}

impl UseDeclaration {
    pub fn into_import(self) -> Import {
        Import {
            alias: self.alias.clone(),
            resolution: Resolution::Unresolved(self.scope.clone()),
            origin: self.node_info,
        }
    }
}

impl AstNode for UseDeclaration {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn pretty(&self, f: &mut dyn std::fmt::Write, _depth: usize) {
        //let uses: Vec<&'static str> = self.scope.iter().map(|ss| ss.resolve()).collect();

        let first = self.scope[0].resolve(); // a valid `use` statement always has at least a first string

        let mut ui = self.scope.iter();
        ui.next();

        let uses =
            first.to_owned() + &ui.fold(String::new(), |acc, next| acc + "::" + next.resolve())[..];

        //let uses = first.to_owned() + ui.fold(String::new(), |inc, next| { inc.to_owned() + "::".to_owned() + next.resolve() });

        //let uses = uses.iter().fold("".to_owned(), |inc, next| { inc
        let _ = write!(
            f,
            "use {}{}",
            uses,
            self.alias
                .map(|ss| " as ".to_owned() + ss.resolve())
                .unwrap_or("".to_owned())
        );
    }
}

#[derive(Debug, Clone)]
pub struct ScopedNameReference {
    pub node_info: NodeInfo,

    pub scope: Vec<IStr>,

    pub silent: bool,
}

impl ScopedNameReference {
    pub fn to_raw_scope(&self) -> ScopedName {
        ScopedName::new(self.scope.clone())
    }
}

impl AstNode for ScopedNameReference {
    fn pretty(&self, f: &mut dyn std::fmt::Write, _depth: usize) {
        let s: String = self
            .scope
            .iter()
            .map(|ss| ss.resolve())
            .intersperse("::")
            .collect();
        let _ = write!(f, "{}", s);
    }
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

impl IntoAstNode for ScopedNameReference {
    /*fn as_node_mut(&mut self) -> &mut dyn AstNode {
        self
    }*/

    fn as_node(&self) -> &dyn AstNode {
        self
    }
}

#[derive(Debug, PartialEq, Hash)]
pub struct ScopedName {
    pub scope: Vec<IStr>,
}

impl ScopedName {
    pub fn new(s: Vec<IStr>) -> ScopedName {
        ScopedName { scope: s }
    }
}

pub struct StaticVariableDeclaration {
    pub node_info: NodeInfo,

    pub public: bool,
    pub expression: Box<ExpressionWrapper>,
}

impl Debug for StaticVariableDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StaticVariableDeclaration")
            .field("public", &self.public)
            .field("expression", &self.expression)
            .finish()
    }
}

impl AstNode for StaticVariableDeclaration {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        write!(f, "{} ", if self.public { "pub" } else { "" });
        self.expression.as_node().pretty(f, depth);
    }
}

#[derive(Debug)]
pub enum TopLevel {
    FunctionDeclaration(FunctionDefinition),
    NamespaceDeclaration(Namespace),
    Implementation(Implementation),
    Specification(TraitDefinition),
    Struct(StructDefinition),
    Enum(EnumDefinition),
    UseDeclaration(UseDeclaration),
    ExpressionDeclaration(Box<ExpressionWrapper>),
    //VariableDeclaration(VariableDeclaration),
}

impl IntoAstNode for TopLevel {
    /*fn as_node_mut(&mut self) -> Option<&mut dyn AstNode> {
        None
    }*/
    /*fn as_node_mut(&mut self) -> &mut dyn AstNode {
        match self {
            Self::FunctionDeclaration(fd) => fd,
            Self::NamespaceDeclaration(nd) => nd,
            Self::StructDeclaration(sd) => sd,
            Self::ExpressionDeclaration(ed) => ed,
            Self::UseDeclaration(ud) => ud,
        }
    }*/

    fn as_node(&self) -> &dyn AstNode {
        match self {
            Self::FunctionDeclaration(fd) => fd,
            Self::NamespaceDeclaration(nd) => nd,
            Self::Implementation(sd) => sd,
            Self::UseDeclaration(ud) => ud,
            Self::Enum(e) => e,
            Self::Struct(e) => e,
            Self::Specification(e) => e,
            Self::ExpressionDeclaration(e) => e.as_node(),
        }
    }
}

impl TopLevel {
    /*pub fn mark_public(&mut self) {
        match self {
            Self::FunctionDeclaration(v) => v.public = true,
            Self::NamespaceDeclaration(v) => v.public = true,
            Self::StructDeclaration(v) => v.public = true,
            Self::ExpressionDeclaration(v) => v.public = true,
            Self::UseDeclaration(v) => v.public = true,
        }
    }*/

    pub fn is_context(&self) -> bool {
        match self {
            Self::NamespaceDeclaration(_) => true,
            _ => false,
        }
    }

    pub fn symbols(&self) -> &[TopLevel] {
        match &self {
            Self::NamespaceDeclaration(ns) => match &ns.contents {
                contents => &contents.declarations[..],
                //Err(_) => &[],
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
