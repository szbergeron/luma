use std::sync::atomic::{AtomicUsize, Ordering};

use dashmap::DashMap;
use smallvec::{SmallVec, smallvec};

use crate::{helper::interner::{IStr, SpurHelper, Internable}, ast::types::AbstractTypeReference};

use super::{NodeInfo, CstNode, IntoCstNode, FunctionDefinition};

#[derive(Clone, Debug)]
pub struct StructDefinition {
    pub info: NodeInfo,

    pub public: bool,

    pub generics: Vec<(IStr, SyntacticTypeReferenceRef)>,

    pub name: IStr,

    pub fields: Vec<Field>,

    pub methods: Vec<FunctionDefinition>,
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

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub struct ScopedName {
    pub scope: SmallVec<[IStr; 3]>,
}

impl ScopedName {
    pub fn new<S>(s: S) -> ScopedName where S: Into<SmallVec<[IStr; 3]>> {
        ScopedName { scope: s.into() }
    }

    pub fn from_one(s: IStr) -> ScopedName {
        ScopedName { scope: smallvec![s] }
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
pub struct SyntacticTypeReferenceRef(uuid::Uuid);

impl SyntacticTypeReferenceRef {
    pub fn new_nil() -> Self {
        Self(uuid::Uuid::nil())
    }

    pub fn new_rand() -> Self {
        Self(uuid::Uuid::new_v4())
    }

    pub fn resolve(self) -> Option<dashmap::mapref::one::Ref<'static, Self, SyntacticTypeReference>> {
        SYNTACTIC_FROM_REF.get(&self)
    }

    pub fn resolve_mut(self) -> Option<dashmap::mapref::one::RefMut<'static, Self, SyntacticTypeReference>> {
        SYNTACTIC_FROM_REF.get_mut(&self)
    }

    pub fn to_abstract(self, within_generic_context: &[IStr]) -> AbstractTypeReference {
        AbstractTypeReference::from_cst(self, within_generic_context)
    }

    pub fn from_std(s: &'static str) -> Self {
        let inner = s.split("::").map(|s| s.intern()).collect();

        let inner = SyntacticTypeReferenceInner::Single { name: ScopedName { scope: inner } };

        let syntr = SyntacticTypeReference {
            id: Self::new_nil(),
            info: NodeInfo::Builtin,
            inner,
        };

        syntr.intern()
    }

    //pub fn resolve_within_context(self, context: &HashMap<IStr, TypeID>) -> 
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SyntacticTypeReference {
    pub id: SyntacticTypeReferenceRef,

    pub info: NodeInfo,

    pub inner: SyntacticTypeReferenceInner,
}

impl SyntacticTypeReference {
    pub fn intern(mut self) -> SyntacticTypeReferenceRef {
        self.id = SyntacticTypeReferenceRef::new_nil();

        let ent = SYNTACTIC_TO_REF.entry(self.clone()).or_insert(SyntacticTypeReferenceRef::new_rand());

        SYNTACTIC_FROM_REF.entry(*ent.value()).or_insert(self);

        // now we drop the original entry, ent, which clears the locking behavior

        *ent.value()
    }
}

lazy_static! {
    static ref SYNTACTIC_TO_REF: DashMap<SyntacticTypeReference, SyntacticTypeReferenceRef> = DashMap::new();
    static ref SYNTACTIC_FROM_REF: DashMap<SyntacticTypeReferenceRef, SyntacticTypeReference> = DashMap::new();
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SyntacticTypeReferenceInner {
    /// If a type reference is just a _, then it is
    /// unconstrained. By default, generics are like this
    Unconstrained(),

    Tuple(Vec<SyntacticTypeReference>),

    Single { name: ScopedName },

    Generic { label: IStr },

    Parameterized { name: ScopedName, generics: Vec<SyntacticTypeReference> },

    Reference { to: Box<SyntacticTypeReference>, mutable: bool },

    Pointer { to: Box<SyntacticTypeReference>, mutable: bool },
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
