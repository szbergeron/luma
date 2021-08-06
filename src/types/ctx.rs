use super::impls::*;

use crate::avec::{AtomicVec, AtomicVecIndex};
use crate::helper::interner::{intern, SpurHelper, StringSymbol};
use crate::types::Quark;
use dashmap::{DashMap, DashSet};

#[allow(unused_imports)]
use rayon::prelude::*;
use smallvec::SmallVec;
use std::fmt::Formatter;
//use tokio::sync::OnceCell;

#[allow(unused_imports)]
use std::fmt::{Debug, Write};

use std::lazy::SyncOnceCell;
//use std::mem::MaybeUninit;
use std::ptr::NonNull;
use std::sync::atomic::{fence, Ordering};

use std::sync::Arc;

use crate::ast::{AstNode, FunctionDefinition, NodeInfo, StructDefinition, indent};
//use once_cell::sync::OnceCell;
use static_assertions::assert_impl_all;
use std::pin::Pin;

pub type TypeHandle = Arc<dyn Type>;

struct RefPtr<T> { inner: NonNull<T> }

unsafe impl<T> Send for RefPtr<T> {}
unsafe impl<T> Sync for RefPtr<T> {}

impl<T> RefPtr<T> {

    /// only sound to call if this refptr has been created
    /// from `from` with a valid pointer, never sound if created
    /// through `dangling`
    pub unsafe fn as_ref(&self) -> &T {
        self.inner.as_ref()
    }

    pub unsafe fn dangling() -> RefPtr<T> {
        Self { inner: NonNull::dangling() }
    }

    pub unsafe fn from(from: *const T) -> RefPtr<T> {
        // the cast here is only sound because this type exposes an immutable ref
        // to T only
        Self { inner: NonNull::new(from as *mut T).expect("was given a null `from`") }
    }
}

//pub type TypeID = u64;
#[derive(PartialEq, Eq, Ord, PartialOrd, Hash, Clone, Copy, Debug)]
pub struct TypeID(pub u64);

#[derive(PartialEq, Eq, Ord, PartialOrd, Hash, Clone, Copy, Debug)]
pub struct FunctionID(AtomicVecIndex);

#[derive(PartialEq, Eq, Ord, PartialOrd, Hash, Clone, Copy, Debug)]
pub struct CtxID(pub u64);

#[derive(PartialEq, Eq, Ord, PartialOrd, Hash, Clone, Copy, Debug)]
pub struct SymbolID(AtomicVecIndex);
//pub type SymbolID = u64;

//pub type GlobalTypeID = (CtxID, TypeID);
#[derive(PartialEq, Eq, Ord, PartialOrd, Hash, Clone, Copy, Debug)]
pub struct GlobalTypeID {
    pub cid: CtxID,
    pub tid: TypeID,
}

#[derive(PartialEq, Eq, Ord, PartialOrd, Hash, Clone, Copy, Debug)]
pub struct GlobalFunctionID {
    pub cid: CtxID,
    pub fid: FunctionID,
}

impl std::fmt::Display for GlobalFunctionID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GlobalFunctionID({}, {})", self.cid.0, self.fid.0)
    }
}

impl std::fmt::Display for GlobalTypeID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GlobalTypeID({}, {})", self.cid.0, self.tid.0)
    }
}

impl std::fmt::Display for CtxID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CtxID({})", self.0)
    }
}

/// Contains an actual textual definition of the function
/// as well as some related metadata so that we can track that definition
/// uniquely.
///
/// This repr exists before any specialization or resolution,
/// and does not actually contain any type resolutions
pub struct FunctionDeclaration {
    //   
}

pub enum TypeDefinition {
    Struct(StructDefinition),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Resolution {
    /// An unresolved import is represented by the sequence of
    /// qualifying strings (including the final qualification)
    /// that were provided with the statement within a context
    Unresolved(Vec<StringSymbol>),

    /// A resolved function import specifies a global
    /// function reference within this scope. This
    /// refers to the definition only, not any monomorphization
    /// or de-canonicalized form of the function
    Function(GlobalFunctionID),

    /// A resolved type import specifies a global
    /// type reference within this scope. This
    /// refers to the definition only, not any monomorphization
    /// or de-canonicalized form of the type
    Type(GlobalTypeID),

    /// A resolved scope import specifies the id
    /// of a GlobalCtxNode, and can be used for doing
    /// further qualifying resolutions later
    Scope(CtxID),
}

impl std::fmt::Display for Resolution {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unresolved(v) => {
                let s: String = v.iter().fold(String::new(), |p, ss| p + ss.resolve());

                write!(f, "{}", s)
            }
            Self::Function(fid) => write!(f, "{}", fid),
            Self::Type(tid) => write!(f, "{}", tid),
            Self::Scope(cid) => write!(f, "{}", cid),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Import {
    pub origin: NodeInfo,
    pub alias: Option<StringSymbol>,
    pub resolution: Resolution,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Export {
    pub alias: Option<StringSymbol>,
    pub resolution: Resolution,
}

impl std::fmt::Display for Import {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "({})", self.0)
        write!(
            f,
            "use {} {}",
            self.resolution,
            self.alias
                .map(|ss| "as ".to_owned() + ss.resolve())
                .unwrap_or(String::new())
        )
    }
}

impl std::fmt::Display for Export {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "({})", self.0)
        write!(
            f,
            "export {} {}",
            self.resolution,
            self.alias
                .map(|ss| "as ".to_owned() + ss.resolve())
                .unwrap_or(String::new())
        )
    }
}

/// CastDistance represents how far one type is from another
/// It can be applied to individual types (a class to its direct supertype is
/// Finite(1) distance from the supertype), or to composite types such as
/// tuples (distance is sum of distances of elements) or function objects
/// (distance is the sum of the contravariant distance of the tuple of parameters and the
/// covariant distance of the return type)
///
/// CastDistance has the Infinite() variant for representing an impossible
/// cast, or one that is currently unable to be computed by the type checker
///
/// @Obsolete: decided against allowing explicit inheritance.
/// This may change in the future, but for now cast distance is not a useful metric
/*pub enum CastDistance {
    FiniteImplicit(u64),
    FiniteExplicit(u64),
    Infinite(),
}*/

pub struct FunctionCall {
    pub name: StringSymbol,
    pub arguments: Vec<StringSymbol>,

    pub generic_specifications: Vec<TypeID>,

    /// If this was a method call or a qualifying path was provided,
    /// this is set to true
    ///
    /// If this call was instead unscoped and "bare" then this is set to false
    ///
    /// If true, search primary context first
    ///
    /// If false, only search secondary context
    ///
    /// This may obsolete having primary and secondary contexts separate later, but TBD
    pub scoped_call: bool,

    /// Represents the context of the type that this (may have been) called on,
    /// so this could be a method of some sort, and if it was we
    /// want to make sure we search the type this was called in first
    pub primary_context: Option<Arc<GlobalCtxNode>>,

    /// Represents the context for the *position* in the code this was called in,
    /// this could be a function that was just locally defined or that was imported
    /// into the current context
    pub secondary_context: Option<Arc<GlobalCtxNode>>,
}

/// Represents a function definition with nothing resolved,
/// and only wraps the AST node that provides the implementation
///
/// Types do not exist within this, and it is only a wrapper struct
/// that may (at some point) represent a future on an actually resolved
/// *generic supporting* function.
#[allow(dead_code)]
pub struct NaiveFunctionDefinition {
    definition: FunctionDefinition,
}

const TYPE_SIGNATURE_DUPLICATE_MAX_FREQ: usize = 3;

static CTX_ID: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);
static TCTX_ID: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);

/*impl Method {
    fn returns(&self) -> TypeID {
        self.return_type
    }

    fn param_names(&self) -> &[String] {
        self.param_names
    }

    fn param_types(&self) -> &[TypeID] {
        self.param_types
    }

    fn name(&self) -> &str {
    }

    /*fn encode_reference(&self, lvals: &[VariableReference]) -> &str {
    }*/
}*/

pub fn generate_typeid() -> TypeID {
    static GENERATOR: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);
    TypeID(GENERATOR.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
}

// interior mut type
/// Note to modifiers: ownership must travel down the tree.
/// No references using & notation should ever refer to a child node,
/// since children may be removed and dropped before a node "above" in the tree.
///
/// Any downward references should use either ID notation or scoped identification
#[allow(dead_code)]
pub struct GlobalCtxNode {
    canonical_local_name: StringSymbol,

    children: DashMap<StringSymbol, Pin<Box<GlobalCtxNode>>>,

    //parent: OnceCell<&'tree GlobalCtxNode<'tree>>,
    //global: OnceCell<&'tree GlobalCtxNode<'tree>>,
    parent: RefPtr<GlobalCtxNode>,
    global: RefPtr<GlobalCtxNode>,

    selfref: RefPtr<GlobalCtxNode>,

    type_ctx: Option<TypeCtx>,
    func_ctx: Option<FuncCtx>,

    quark: Option<super::Quark>,

    //selfref: OnceCell<&'tree GlobalCtxNode<'tree>>,
    id: CtxID,

    //imports: DashMap<StringSymbol, Import>,
    //exports: DashMap<StringSymbol, Export>,
    imports: DashSet<Import>,
    exports: DashSet<Export>,
}

impl Debug for GlobalCtxNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.display(f, 0);
        /*writeln!(f, "in GlobalCtxNode named {} [[", self.canonical_local_name.resolve());

        for child in self.children.iter() {
            writeln!(f, "{:?}, ", child.value());
            //child.value().fmt(f);
        }

        writeln!(f, "]]")*/
        Ok(())
    }
}

impl AstNode for GlobalCtxNode {
    fn node_info(&self) -> NodeInfo {
        todo!()
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}GlobalCtxNode with name {}",
            indent(depth),
            self.canonical_local_name.resolve(),
            );

        let _ = writeln!(f, "{}Imports: ", indent(depth + 1));
        for dr in self.imports.iter() {
            let _ = writeln!(f, "{}{}", indent(depth + 2), *dr);
        }

        let _ = writeln!(f, "{}Exports: ", indent(depth + 1));
        for dr in self.exports.iter() {
            let _ = writeln!(f, "{}{}", indent(depth + 2), *dr);
        }

        let _ = writeln!(f, "{}Children: ", indent(depth + 1));
        for child in self.children.iter() {
            child.value().display(f, depth + 2);
            //child.value().fmt(f);
        }

        let _ = writeln!(f, "{}FuncCtx:", indent(depth + 1));
        //println!("fctx is {}", self.func_ctx.is_some());
        self.func_ctx.iter().for_each(|fctx| fctx.display(f, depth + 2));
    }
}

impl AstNode for FuncCtx {
    fn node_info(&self) -> NodeInfo {
        todo!()
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(f, "{}FuncCtx ", indent(depth));

        for fidv in self.by_name.iter() {
            //println!("got an fidv");
            for fid in fidv.iter() {
                //dbg!(fid);
                let _ = write!(f, "{}", indent(depth+1));
                GlobalCtx::get().func_ctx().lookup(*fid).pretty(f, depth + 1);
                let _ = writeln!(f, "");

            }
        }
    }
}

assert_impl_all!(GlobalCtxNode: Send);

/// NOTE: lots of unsafe here, major contracts around the parent/global/selfref members,
/// tread carefully when modifying this code!
impl GlobalCtxNode {
    pub fn display_internal(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}Global context <canon {}> has id {}",
            indent(depth),
            self.canonical_local_name.resolve(),
            self.id
        );

        let _ = writeln!(f, "{}Imports:", indent(depth + 1));
        for dr in self.imports.iter() {
            let _ = writeln!(f, "{}{}", indent(depth + 2), *dr);
        }

        let _ = writeln!(f, "{} Exports:", indent(depth + 1));
        for dr in self.exports.iter() {
            let _ = writeln!(f, "{}{}", indent(depth + 2), *dr);
        }

        let _ = writeln!(f, "{}Children:", indent(depth + 1));
        for child in self.children.iter() {
            child.value().display(f, depth + 2);
        }
    }

    pub fn import(&self, import: Import) {
        self.imports.insert(import);
    }

    pub fn export(&self, export: Export) {
        self.exports.insert(export);
    }

    pub fn type_ctx(&self) -> &TypeCtx {
        self.type_ctx.as_ref().unwrap()
    }

    pub fn func_ctx(&self) -> &FuncCtx {
        self.func_ctx.as_ref().unwrap()
    }

    pub fn quark(&self) -> &super::Quark {
        self.quark.as_ref().unwrap()
        //&self.quark.unwrap()
    }

    pub fn add_child(&self, child: Pin<Box<GlobalCtxNode>>) {
        //println!("Adds child {} to parent {}", child.id, self.id);
        //dbg!(child.canonical_local_name);
        //dbg!(self.canonical_local_name);
        self.children.insert(child.canonical_local_name, child);
    }

    /// TODO: change how we pass ptr to parent/global to be actual pointer instead of ref
    /// See notes inside function for notes on soundness/safety contracts for usage,
    /// the 'parent lifetime is of significant importance here
    #[allow(unused_unsafe, unused_mut)]
    pub unsafe fn new<'parent>(
        name: StringSymbol,
        parent: Option<&'parent GlobalCtxNode>,
        global: Option<&'parent GlobalCtxNode>,
        id: Option<CtxID>,
    ) -> Pin<Box<GlobalCtxNode>> {
        let id = id.unwrap_or_else(|| Self::generate_ctxid());
        //dbg!(name.resolve());
        //dbg!(id);

        unsafe {
            let mut node = GlobalCtxNode {
                id,
                imports: DashSet::new(),
                exports: DashSet::new(),

                canonical_local_name: name,
                children: DashMap::new(),

                parent: RefPtr::dangling(),
                global: RefPtr::dangling(),
                selfref: RefPtr::dangling(),

                type_ctx: Some(TypeCtx::new(id)),
                func_ctx: Some(FuncCtx::new(id)),

                quark: None,
                //quark: std::mem::MaybeUninit::uninit(),
            };
            let as_ptr: *mut GlobalCtxNode = Box::into_raw(Box::new(node));
            //let boxed = Pin::new(Box::new(node));

            let mref = as_ptr.as_ref().expect("directly leaked box was null");

            (*as_ptr).selfref = std::mem::transmute(as_ptr);

            (*as_ptr).quark = Some(Quark::new_within(mref));

            (*as_ptr).parent = match parent {
                // NOTE: this transmute is only sound if the parent ref truly only lives for the
                // parent lifetime
                //
                // This transition is why this function is marked as unsafe,
                // as the contract that parent live at least as long as 'self
                // must be upheld
                Some(ptr) => std::mem::transmute(ptr),
                None => RefPtr::from(as_ptr),
            };

            (*as_ptr).global = match global {
                Some(ptr) => std::mem::transmute(ptr),
                None => RefPtr::from(as_ptr),
            };

            let pinned = Pin::new_unchecked(Box::from_raw(as_ptr));

            fence(Ordering::Release);

            pinned
        }
    }

    pub fn get_selfref(&self) -> &GlobalCtxNode {
        unsafe { self.selfref.as_ref() }
    }

    pub fn name(&self) -> &str {
        self.canonical_local_name.resolve()
    }

    pub fn generate_ctxid() -> CtxID {
        let id = CTX_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let id = CtxID(id);
        id
    }

    pub fn generate_tctxid(within: CtxID) -> GlobalTypeID {
        let id = TCTX_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let id = GlobalTypeID { cid: within, tid: TypeID(id) };
        id
    }

    fn nsctx_for(&self, scope: &[StringSymbol]) -> Option<&GlobalCtxNode> {
        //match scope {
        //[last] //
        match scope {
            [] => Some(self.get_selfref()),
            [first, rest @ ..] => {
                //self.
                self.children
                    .get(first)
                    .map(|nsctx|
                         unsafe {
                             // NOTE: this is sound because of the drop ordering guarantees
                             // for this structure.
                             // The ref here may only go invalid during the drop of
                             // self, and thus must be valid until drop for self is first
                             // called
                             std::mem::transmute(nsctx.nsctx_for(rest)) })
                    .flatten()
            }
        }
    }
}

pub struct GlobalCtx {
    entry: Pin<Box<GlobalCtxNode>>,

    contexts: DashMap<CtxID, Pin<Box<GlobalCtxNode>>>,
    
    funcs: GlobalFuncCtx,
    types: GlobalTypeCtx,

    //functions: DashMap<GlobalFunctionID, Arc<FunctionImplementation>>,
    //types: DashMap<GlobalTypeID, TypeHandle>,
}

impl GlobalCtx {
    pub fn get_global(&self) -> &GlobalCtxNode {
        self.entry.get_selfref()
    }

    pub fn get() -> &'static GlobalCtx {
        static GCTX: SyncOnceCell<GlobalCtx> = SyncOnceCell::new(); //GlobalCtx::new();

        GCTX.get_or_init(|| GlobalCtx::new())
    }

    pub fn new() -> GlobalCtx {
        unsafe {
            GlobalCtx {
                entry: GlobalCtxNode::new(
                    intern("global"),
                    None,
                    None,
                    None,
                ),
                contexts: DashMap::new(),
                funcs: GlobalFuncCtx::new(),
                types: GlobalTypeCtx::new(),
                //functions: DashMap::new(),
                //types: DashMap::new(),
            }
        }
    }

    /// tries to get a namespace ctx if one exists, and None if not
    #[allow(unused)]
    fn get_nsctx(&self, scope: &[StringSymbol]) -> Option<&GlobalCtxNode> {
        self.entry.nsctx_for(scope)
    }

    pub fn by_id(&self, id: CtxID) -> Option<Pin<&GlobalCtxNode>> {
        //self.contexts.get(&id).map(|opt| opt.value().get_selfref())
        let opt = self.contexts.get(&id)?;
        let opt = opt.value();

        unsafe {
            let r = opt.get_selfref();

            // we know the ref inside the pin must live as long as we do not drop a child,
            // so long as we maintain that invariant we can take the ref and return it

            Some(std::mem::transmute(r))
        }
        //let opt = opt.value();
        //Some(opt.as_ref())
        //Some(opt.get_selfref())
    }

    pub fn func_ctx(&self) -> &GlobalFuncCtx {
        &self.funcs
    }

    pub fn type_ctx(&self) -> &GlobalTypeCtx {
        &self.types
    }
}

/// Interior mutable container representing a function context
/// (set of functions from a module context)
#[derive(Debug)]
pub struct FuncCtx {
    ctx_id: CtxID,
    //by_id: DashMap<FunctionID, FunctionHandle>,
    //by_signature: DashMap<FunctionSignature, FunctionHandle>,
    by_name: DashMap<StringSymbol, Vec<GlobalFunctionID>>,

    all: Vec<()>,
    //id_gen: u64,
}

impl FuncCtx {
    pub fn new(within: CtxID) -> FuncCtx {
        FuncCtx {
            //by_id: DashMap::new(),
            /*by_signature: DashMap::new(),*/
            by_name: DashMap::new(),
            //id_gen: 1,
            ctx_id: within,

            all: Vec::new(),
        }
    }

    pub fn merge_local(&self, _other: Arc<FuncCtx>) {}

    pub fn add(&self, func: FunctionDefinition) {
        println!("got a func");

        let name = func.name;
        
        let id = GlobalCtx::get().func_ctx().define(self.ctx_id, func);
        //println!("id was {}", id);

        fence(Ordering::Release);

        self.by_name.entry(name)
            .or_insert(Vec::new()) // note: vec::new is (basically) free and doesn't allocate :)
            .push(id);
    }

    /*pub fn query(
        &self,
        name: Option<StringSymbol>,
        params: &[Option<TypeID>],
        _returns: Option<TypeID>,
    ) -> Vec<FunctionID> {
        //let name = name.expect("TODO: function name elision");

        for param in params.iter() {
            param.expect("TODO: function lookups on partial parameter types");
        }

        let mut initial_set = name
            .map(|st| self.by_name.get(&st))
            .flatten()
            .map(|entry| entry.value().clone())
            .unwrap_or_else(|| {
                (0..self.all.len())
                    .map(|idx| GlobalFunctionID {
                        cid: self.ctx_id,
                        fid: FunctionID(idx as u64),
                    })
                    .collect()
            });

        initial_set = initial_set
            .into_par_iter()
            .filter(|gfid| {
                if let Some(f) = self.lookup(gfid.fid) {
                    if f.params.len() != params.len() {
                        return false;
                    };

                    for (arg_opt, _param) in params.iter().zip(f.params.iter().map(|(tid, _)| tid))
                    {
                        if let Some(_arg) = arg_opt {}
                    }

                    unimplemented!()
                } else {
                    false
                }
            })
            .collect();

        //self.by_name.get(&name).map(|map_entry| map_entry.value().first())

        unimplemented!()
    }*/

    /*pub fn id_to_sig(&self, fid: FunctionID) -> Option<TypeSignature> {
        self.
    }*/
}

pub struct GlobalFuncCtx {
    //functions: DashMap<GlobalFunctionID, FunctionDefinition>,
    functions: crate::avec::AtomicVec<FunctionDefinition>,
}

impl GlobalFuncCtx {
    pub fn new() -> GlobalFuncCtx {
        GlobalFuncCtx { functions: AtomicVec::new() }
    }
    pub fn define(&self, within: CtxID, func: FunctionDefinition) -> GlobalFunctionID {
        let (avi, _) = unsafe {
            self.functions.push_with(func, |_f, _avi| {})
        };

        let id = GlobalFunctionID { cid: within, fid: FunctionID(avi) };

        id
    }

    pub fn lookup(&self, id: GlobalFunctionID) -> &FunctionDefinition {
        // NOTE: this is only sound if we do not remove from self.functions, since
        // these references should live as long as 'self
        //
        // This means that we can hand out refs of &'self freely since
        // the borrow checker will verify that they do not last any longer than we do
        /*unsafe {
            //std::mem::transmute(self.functions.get(&id).unwrap().value())
        }*/

        //self.functions.get(&id).unwrap().value()
        //todo!()
        self.functions.get(id.fid.0)
    }
}

pub struct GlobalTypeCtx {
    //types: DashMap<GlobalTypeID, TypeDefinition>,
    types: AtomicVec<TypeDefinition>,
}

impl GlobalTypeCtx {
    pub fn new() -> GlobalTypeCtx {
        GlobalTypeCtx { types: AtomicVec::new() }
    }
    /*pub fn define(&self, within: CtxID, func: FunctionDefinition) -> GlobalFunctionID {
        static FCTX_ID: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);
        let id = FCTX_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let id = GlobalFunctionID { cid: within, fid: FunctionID(id) };

        self.functions.insert(id, func);

        id
    }

    pub fn lookup(&self, id: GlobalFunctionID) -> &FunctionDefinition {
        // NOTE: this is only sound if we do not remove from self.functions, since
        // these references should live as long as 'self
        //
        // This means that we can hand out refs of &'self freely since
        // the borrow checker will verify that they do not last any longer than we do
        unsafe {
            std::mem::transmute(self.functions.get(&id).unwrap().value())
        }
    }*/
}

/// Interior mutable container representing a type context
pub struct TypeCtx {
    ctx_id: CtxID,

    types: DashMap<TypeID, TypeHandle>,
    signatures: DashMap<TypeSignature, SmallVec<[TypeID; TYPE_SIGNATURE_DUPLICATE_MAX_FREQ]>>,
    ids: DashMap<TypeID, TypeSignature>,
}

impl TypeCtx {
    pub fn id(&self) -> CtxID {
        self.ctx_id
    }

    pub fn merge_local(&self, _other: Arc<TypeCtx>) {}

    pub fn new(within: CtxID) -> TypeCtx {
        TypeCtx {
            ctx_id: within,
            types: DashMap::new(),
            signatures: DashMap::new(),
            ids: DashMap::new(),
        }
    }

    pub fn add(&self, t: TypeDefinition) {
    }

    pub fn define<T: 'static>(&self, newtype: T) -> TypeID
    where
        T: Type,
    {
        let tid = generate_typeid();
        newtype.set_tid(tid);
        //let arcd = Arc::new(RwLock::new(newtype));
        let arcd = Arc::new(newtype);
        self.types.insert(tid, arcd);
        tid
    }

    pub fn define_struct(&self, sd: StructDefinition) -> () {
       // do nothing, just drop for now 
       // TODO
    }

    pub fn implement(&self /* handle */) {}

    pub fn mix(&self /* handle 1, handle 2 */) -> TypeID {
        todo!()
    }

    //pub fn inherit(&self, /* handle src, handle dst */) {}

    /*pub fn query(&self, canonicalized_string: String) -> TypeHandle {
        panic!("not implemented")
    }*/

    pub fn lookup(&self, tid: TypeID) -> Option<TypeHandle> {
        self.types.get(&tid).map(|r| (*r.value()).clone())
    }

    pub fn id_to_sig(&self, tid: TypeID) -> Option<TypeSignature> {
        self.ids.get(&tid).map(|r| (*r.value()).clone())
    }

    pub fn sig_to_id(&self, t: &TypeSignature) -> Option<Vec<TypeID>> {
        self.signatures
            .get(t)
            .map(|r| (*r.value()).iter().map(|&v| v).collect())
    }
}

//impl Send for CtxInner {} // should this have send?
unsafe impl Sync for TypeCtx {}

//pub type Ctx = Arc<CtxInner>;
