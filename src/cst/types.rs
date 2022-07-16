use std::sync::atomic::{AtomicUsize, Ordering};

use crate::helper::interner::{IStr, SpurHelper};

use super::{NodeInfo, CstNode, IntoCstNode, FunctionDefinition};

#[derive(Clone, Debug)]
pub struct StructDefinition {
    pub info: NodeInfo,

    pub generics: Vec<GenericHandle>,

    pub name: IStr,

    pub fields: Vec<Field>,
}

impl CstNode for StructDefinition {
    fn node_info(&self) -> NodeInfo {
        self.info
    }
}

#[derive(Clone, Debug)]
pub struct ImplementationDefinition {
    pub info: NodeInfo,

    pub generics: Vec<GenericHandle>,

    pub for_type: Option<TypeReference>,
    pub of_trait: Option<TypeReference>,

    pub functions: Vec<FunctionDefinition>,
    pub fields: Vec<Field>,
}

impl CstNode for ImplementationDefinition {
    fn node_info(&self) -> NodeInfo {
        self.info
    }
}

#[derive(Clone, Debug)]
pub struct TraitDefinition {
    pub info: NodeInfo,

    pub generics: Vec<GenericHandle>,

    pub name: IStr,

    pub constraint: Option<TypeReference>,

    pub functions: Vec<FunctionDefinition>,
}

impl CstNode for TraitDefinition {
    fn node_info(&self) -> NodeInfo {
        self.info
    }
}

#[derive(Clone, Debug)]
pub struct EnumDefinition {
    unimpl: !,
}

impl CstNode for EnumDefinition {
    fn node_info(&self) -> NodeInfo {
        todo!()
    }
}

#[derive(Clone, Debug)]
pub struct Field {
    pub info: NodeInfo,

    pub has_type: TypeReference,
    pub has_name: IStr,
}

pub struct GenericParameters {
    pub params: Vec<GenericHandle>,
}

/// The "T" in Something<T>
///
/// A GenericHandle aims to be a key type
/// that can map down to an actual type during
/// usage and implementation solving
///
/// If a handle is reused, then simply clone the existing one
/// that you wish to refer to (don't use new!)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericHandle {
    name: IStr,
    id: usize,
}

impl GenericHandle {
    pub fn new(name: IStr) -> GenericHandle {
        static ID: AtomicUsize = AtomicUsize::new(1);

        GenericHandle { name, id: ID.fetch_add(1, Ordering::SeqCst)}
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

impl CstNode for ScopedNameReference {
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

impl IntoCstNode for ScopedNameReference {
    fn as_node(&self) -> &dyn CstNode {
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



#[derive(Debug, Clone)]
pub struct TypeReference {
    pub info: NodeInfo,

    pub ctx: ScopedNameReference,
    pub name: IStr,

    pub type_args: Vec<TypeReference>,
}

impl TypeReference {
    pub fn new(ctx: ScopedNameReference, name: IStr, node_info: NodeInfo) -> TypeReference {
        Self::generic_new(ctx, name, node_info, Vec::new())
    }

    pub fn generic_new(ctx: ScopedNameReference, name: IStr, node_info: NodeInfo, type_args: Vec<TypeReference>) -> TypeReference {
        TypeReference {
            info: node_info,
            ctx,
            type_args,
            name,
        }
    }
}

impl CstNode for TypeReference {
    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        let _ = write!(f, "{}", self.name);
        if !self.type_args.is_empty() {
            let _ = write!(f, "<args: !impl>");
        }
    }
    /*fn display(&self, f: &mut std::fmt::Formatter<'_>, _depth: usize) {
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
    }*/

    fn node_info(&self) -> NodeInfo {
        self.info
    }
}

impl IntoCstNode for TypeReference {
    /*fn as_node_mut(&mut self) -> &mut dyn AstNode {
        self
    }*/

    fn as_node(&self) -> &dyn CstNode {
        self
    }
}
