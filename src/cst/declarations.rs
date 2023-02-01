use super::expressions::ExpressionWrapper;

use crate::cst as cst;

//use crate::ast::tree::GenericHandle;
/*use super::EnumDefinition;
use super::FunctionDefinition;
use super::Implementation;
use super::StructDefinition;
use super::TraitDefinition;
use super::TypeDefinition;
use super::TypeReference;*/

//use crate::types::FunctionDeclaration;
use std::fmt::Debug;
//use std::io::Write;


/*use crate::types::GlobalCtxNode;
use crate::types::Import;
use crate::types::Resolution;*/


use super::cst_traits::*;
//use std::rc::Rc;

//use std::cell::RefCell;

//use super::expressions::TypeReference;

use crate::helper::interner::*;
//use async_recursion::async_recursion;
//use futures::future::join_all;
//use indent::indent_by;
//use pretty::Doc;
//use pretty::RcDoc;

#[derive(Debug)]
pub struct NamespaceDefinition {
    pub node_info: NodeInfo,

    pub public: bool,
    pub name: Option<IStr>,
    pub contents: OuterScope,
}

impl NamespaceDefinition {
    pub fn set_public(&mut self, public: bool) {
        self.public = public;
    }

    /*#[async_recursion]
    /// Unsafe: requires &parent and &global to live at least as long as self, even if this is not
    /// specified
    pub async unsafe fn into_ctx(
        self,
        parent_scope: &[IStr],
        parent: &!,
        global: &!,
    ) -> Pin<Box<!>> {
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
    }*/

    /*fn as_symbol(&self) -> SymbolDeclaration {
        SymbolDeclaration::NamespaceDeclaration(Arc::new(self))
    }*/
}

impl CstNode for NamespaceDefinition {
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
    //#[async_recursion]
    /*pub async unsafe fn into_ctx(self, module: Vec<IStr>, parent: &!, global: &!) -> Pin<Box<!>> {
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
    }*/
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

impl CstNode for OuterScope {
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
    /*pub fn into_import(self) -> Import {
        Import {
            alias: self.alias.clone(),
            resolution: Resolution::Unresolved(self.scope.clone()),
            origin: self.node_info,
        }
    }*/
}

impl CstNode for UseDeclaration {
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

impl CstNode for StaticVariableDeclaration {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        let _ = write!(f, "{} ", if self.public { "pub" } else { "" });
        self.expression.as_node().pretty(f, depth);
    }
}

#[derive(Debug)]
pub enum TopLevel {
    Function(FunctionDefinition),
    Namespace(cst::NamespaceDefinition),
    Impl(cst::ImplementationDefinition),
    Trait(cst::TraitDefinition),
    Struct(cst::StructDefinition),
    Enum(cst::EnumDefinition),
    UseDeclaration(UseDeclaration),
    ExpressionDeclaration(Box<ExpressionWrapper>),
    //VariableDeclaration(VariableDeclaration),
}

impl IntoCstNode for TopLevel {
    fn as_node(&self) -> &dyn CstNode {
        match self {
            Self::Function(fd) => fd,
            Self::Namespace(nd) => nd,
            Self::Impl(sd) => sd,
            Self::UseDeclaration(ud) => ud,
            Self::Enum(e) => e,
            Self::Struct(e) => e,
            Self::Trait(e) => e,
            Self::ExpressionDeclaration(e) => e.as_node(),
        }
    }
}

impl TopLevel {
    pub fn is_context(&self) -> bool {
        match self {
            Self::Namespace(_) => true,
            _ => false,
        }
    }

    pub fn symbols(&self) -> &[TopLevel] {
        match &self {
            Self::Namespace(ns) => match &ns.contents {
                contents => &contents.declarations[..],
                //Err(_) => &[],
            },
            _ => &[],
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub info: NodeInfo,

    pub public: bool,
    pub name: IStr,

    pub body: Box<ExpressionWrapper>,
    pub return_type: cst::SyntaxTypeReference,
    //pub params: Vec<(Box<super::ExpressionWrapper>, super::TypeReference)>,
    pub params: Vec<(IStr, cst::SyntaxTypeReference)>,
}

impl CstNode for FunctionDefinition {
    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        let _ = write!(f, "fn {} (", self.name.resolve(),);
        for p in self.params.iter() {
            let _ = write!(f, "{}:", p.0.resolve());
            p.1.pretty(f, depth + 1);
            let _ = write!(f, ", ");
        }
        let _ = write!(f, ") -> ");
        self.return_type.pretty(f, depth + 1);
        let _ = write!(f, " ");
        //let _ =
        //let _ = writeln!(f, " {{");
        //let _ = write!(f, "{}", indent(depth + 1));

        self.body.as_node().pretty(f, depth);
        //let _ = writeln!(f, "");
        //let _ = writeln!(f, "\n{}}}", indent(depth));
    }
    fn node_info(&self) -> NodeInfo {
        self.info
    }

    /*fn format(&self) -> RcDoc {
        let r = RcDoc::text("FunctionDeclaration with name ")
            .append(self.name.resolve())
            .append(RcDoc::line());

        let r = r
            .append("Parameters: ")
            .append(
                RcDoc::intersperse(
                    self.params
                        .iter()
                        .map(|(name, tr)| RcDoc::text(name.resolve()).append(": ").append(tr.format())),
                    comma_break(),
                )
                .nest(1),
            )
            .append(RcDoc::line());

        let r = r
            .append("Returns: ")
            .append(self.return_type.format().nest(1))
            .append(RcDoc::line());

        let r = r
            .append("Body:")
            .append(self.body.as_node().format().nest(1));

        r
    }*/
}

#[derive(Debug, Clone)]
pub struct MemberAttributes {
    pub public: bool,
    pub mutable: bool,
    //deferred: bool,
}
