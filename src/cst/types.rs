use std::{
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

use dashmap::DashMap;
use itertools::Itertools;
use smallvec::{smallvec, SmallVec};

use crate::{
    ast::types::AbstractTypeReference,
    helper::interner::{IStr, Internable, SpurHelper},
};

use super::{CstNode, FunctionDefinition, IntoCstNode, NodeInfo};

#[derive(Clone, Debug)]
pub struct VisibilityQualifier {

}

#[derive(Clone, Debug)]
pub struct StructDefinition {
    pub info: NodeInfo,

    pub public: bool,

    pub generics: Vec<(IStr, SyntacticTypeReferenceRef)>,

    pub name: IStr,

    pub fields: Vec<Field>,

    pub attrs: StructuralTyAttrs,

    pub methods: Vec<FunctionDefinition>,
}

#[derive(Debug, Clone, Default)]
pub struct StructuralTyAttrs {
    /// States that this is a ref type or a non-ref type (by-value)
    /// non-ref types must not be self-referential!
    pub is_ref: bool,

    /// Whether this type allows adding additional dyn fields
    pub is_modif: bool,

    pub is_builtin: Option<IStr>,
}

impl CstNode for StructDefinition {
    fn node_info(&self) -> NodeInfo {
        self.info
    }
}

#[derive(Clone, Debug)]
pub struct ImplementationDefinition {
    pub info: NodeInfo,

    pub generics: Vec<(IStr, SyntacticTypeReferenceRef)>,

    pub for_type: Option<SyntacticTypeReferenceRef>,
    pub of_trait: Option<SyntacticTypeReferenceRef>,

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

    pub vis: VisibilityQualifier,

    pub generics: Vec<(IStr, SyntacticTypeReferenceRef)>,

    pub name: IStr,

    pub constraint: Option<SyntacticTypeReferenceRef>,

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

    pub has_type: SyntacticTypeReferenceRef,
    pub has_name: IStr,
}

pub struct GenericParameters {
    pub params: Vec<(IStr, SyntacticTypeReferenceRef)>,
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

        GenericHandle {
            name,
            id: ID.fetch_add(1, Ordering::SeqCst),
        }
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
        let s: String =
            Itertools::intersperse(self.scope.iter().map(|ss| ss.resolve()), "::").collect();
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

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub struct ScopedName {
    pub scope: SmallVec<[IStr; 3]>,
}

impl ScopedName {
    pub fn new<S>(s: S) -> ScopedName
    where
        S: Into<SmallVec<[IStr; 3]>>,
    {
        ScopedName { scope: s.into() }
    }

    pub fn from_many<S: Into<String>>(s: S) -> Self {
        let s: String = s.into();

        let inner = s.split("::").map(|s| s.intern()).collect();

        Self { scope: inner }
    }

    pub fn from_one(s: IStr) -> ScopedName {
        ScopedName {
            scope: smallvec![s],
        }
    }
}

/*#[derive(Debug, Clone)]
pub enum OldTypeReference {
    Syntactic(SyntacticTypeReferenceRef),

    // Includes both what it is resolved to now and what it was resolved from first
    //Abstract(Box<AbstractTypeReference>, SyntacticTypeReferenceRef),
}*/

/*impl OldTypeReference {
    pub fn from_std(s: &str) -> Self {
        todo!("syntactic ref from here?")
    }
}*/

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct SyntacticTypeReferenceRef(uuid::Uuid, NodeInfo);

impl SyntacticTypeReferenceRef {
    pub fn new_nil() -> Self {
        Self(uuid::Uuid::nil(), NodeInfo::Builtin)
    }

    pub fn new_rand(location: NodeInfo) -> Self {
        Self(uuid::Uuid::new_v4(), location)
    }

    pub fn resolve(self) -> Option<SyntacticTypeReference> {
        let v = SYNTACTIC_FROM_REF.get(&self).map(|s| s.value().clone());

        v.map(|mut v| {
            v.info = self.1;

            v
        })
    }

    pub fn is_nil(self) -> bool {
        self.0.is_nil()
    }

    /*pub fn resolve_mut(self) -> Option<dashmap::mapref::one::RefMut<'static, Self, SyntacticTypeReference>> {
        SYNTACTIC_FROM_REF.get_mut(&self)
    }*/

    pub fn to_abstract(self, within_generic_context: &[IStr]) -> AbstractTypeReference {
        AbstractTypeReference::from_cst(self, within_generic_context)
    }

    pub fn from_std(s: &'static str) -> Self {
        let sn = ScopedName::from_many(s);

        let inner = SyntacticTypeReferenceInner::Single { name: sn };

        let syntr = SyntacticTypeReference {
            id: Self::new_nil(),
            info: NodeInfo::Builtin,
            inner,
        };

        syntr.intern()
    }

    //pub fn resolve_within_context(self, context: &HashMap<IStr, TypeID>) ->
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct SyntacticTypeReference {
    pub id: SyntacticTypeReferenceRef,

    pub info: NodeInfo,

    pub inner: SyntacticTypeReferenceInner,
}

impl SyntacticTypeReference {
    pub fn unconstrained() -> SyntacticTypeReference {
        Self {
            id: SyntacticTypeReferenceRef::new_nil(),
            info: NodeInfo::Builtin,
            inner: SyntacticTypeReferenceInner::Unconstrained(),
        }
    }

    pub fn as_plain_type(self, sub_generics: &HashMap<IStr, IStr>) -> IStr {
        match self.inner {
            SyntacticTypeReferenceInner::Unconstrained() => todo!(),
            SyntacticTypeReferenceInner::Tuple(t) => {
                todo!()
            }
            SyntacticTypeReferenceInner::Single { name } => {
                let n = name.scope.into_iter().join("::").intern();
                //let n = name.scope.last().copied().unwrap_or("unknown".intern());

                n
                //format!("{name}").intern()
            }
            SyntacticTypeReferenceInner::Generic { label } => {
                sub_generics.get(&label).copied().unwrap()
            }
            SyntacticTypeReferenceInner::Parameterized { name, generics } => {
                let n = name.scope.into_iter().join("::").intern();
                //let n = name.scope.last().copied().unwrap_or("unknown".intern());
                let g = generics
                    .into_iter()
                    .map(|g| g.as_plain_type(sub_generics))
                    .join(", ");

                format!("{n}<{g}>").intern()
            }
            SyntacticTypeReferenceInner::Reference { to, mutable } => todo!(),
            SyntacticTypeReferenceInner::Pointer { to, mutable } => todo!(),
        }
    }

    pub fn intern(mut self) -> SyntacticTypeReferenceRef {
        self.id = SyntacticTypeReferenceRef::new_nil();

        let ent = SYNTACTIC_TO_REF
            .entry(self.clone())
            .or_insert(SyntacticTypeReferenceRef::new_rand(self.info));

        SYNTACTIC_FROM_REF.entry(*ent.value()).or_insert(self);

        // now we drop the original entry, ent, which clears the locking behavior

        *ent.value()
    }

    pub fn as_c_id(&self) -> IStr {
        match &self.inner {
            SyntacticTypeReferenceInner::Unconstrained() => todo!(),
            SyntacticTypeReferenceInner::Tuple(_) => todo!(),
            SyntacticTypeReferenceInner::Single { name } => {
                name.scope.iter().map(|e| e.resolve().to_owned()).join("_")
                //name.scope.last().copied().unwrap_or("unknown".intern()).resolve()
            }
            SyntacticTypeReferenceInner::Generic { label } => todo!(),
            SyntacticTypeReferenceInner::Parameterized { name, generics } => todo!(),
            SyntacticTypeReferenceInner::Reference { to, mutable } => todo!(),
            SyntacticTypeReferenceInner::Pointer { to, mutable } => todo!(),
        }
        .intern()
    }
}

impl std::fmt::Debug for SyntacticTypeReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //f.debug_struct("SyntacticTypeReference").field("id", &self.id).field("info", &self.info).field("inner", &self.inner).finish()
        let inner = match &self.inner {
            SyntacticTypeReferenceInner::Unconstrained() => "_".to_owned(),
            SyntacticTypeReferenceInner::Tuple(t) => {
                let values = t.iter().map(|syntr| format!("{syntr:?}")).join(", ");
                let base = format!("Tuple{}<{values}>", t.len());

                base
            }
            SyntacticTypeReferenceInner::Single { name } => {
                name.scope.iter().map(|v| v.resolve().to_owned()).join("::")
            }
            SyntacticTypeReferenceInner::Generic { label } => label.resolve().to_owned(),
            SyntacticTypeReferenceInner::Parameterized { name, generics } => {
                format!("{name:?}<{generics:?}>")
            }
            SyntacticTypeReferenceInner::Reference { to, mutable } => {
                format!("&{}<{to:?}>", if *mutable { "mut" } else { "" })
            }
            SyntacticTypeReferenceInner::Pointer { to, mutable } => todo!(),
        };
        write!(f, "{inner}")
    }
}

lazy_static! {
    static ref SYNTACTIC_TO_REF: DashMap<SyntacticTypeReference, SyntacticTypeReferenceRef> =
        DashMap::new();
    static ref SYNTACTIC_FROM_REF: DashMap<SyntacticTypeReferenceRef, SyntacticTypeReference> =
        DashMap::new();
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SyntacticTypeReferenceInner {
    /// If a type reference is just a _, then it is
    /// unconstrained. By default, generics are like this
    Unconstrained(),

    Tuple(Vec<SyntacticTypeReference>),

    Single {
        name: ScopedName,
    },

    Generic {
        label: IStr,
    },

    Parameterized {
        name: ScopedName,
        generics: Vec<SyntacticTypeReference>,
    },

    Reference {
        to: Box<SyntacticTypeReference>,
        mutable: bool,
    },

    Pointer {
        to: Box<SyntacticTypeReference>,
        mutable: bool,
    },
}

impl CstNode for SyntacticTypeReference {
    fn node_info(&self) -> NodeInfo {
        self.info
    }
}

impl IntoCstNode for SyntacticTypeReference {
    fn as_node(&self) -> &dyn CstNode {
        self
    }
}

/*#[derive(Debug, Clone)]
pub struct SyntaxTypeReference {
    pub info: NodeInfo,

    pub ctx: ScopedNameReference,
    pub name: IStr,

    pub type_args: Vec<SyntaxTypeReference>,
}

impl SyntaxTypeReference {
    pub fn new(ctx: ScopedNameReference, name: IStr, node_info: NodeInfo) -> SyntaxTypeReference {
        Self::generic_new(ctx, name, node_info, Vec::new())
    }

    pub fn generic_new(ctx: ScopedNameReference, name: IStr, node_info: NodeInfo, type_args: Vec<SyntaxTypeReference>) -> SyntaxTypeReference {
        SyntaxTypeReference {
            info: node_info,
            ctx,
            type_args,
            name,
        }
    }
}

impl CstNode for SyntaxTypeReference {
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

impl IntoCstNode for SyntaxTypeReference {
    /*fn as_node_mut(&mut self) -> &mut dyn AstNode {
        self
    }*/

    fn as_node(&self) -> &dyn CstNode {
        self
    }
}*/
