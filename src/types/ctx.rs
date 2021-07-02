use super::impls::*;

use crate::helper::interner::{StringSymbol, SpurHelper};
use dashmap::DashMap;
use rayon::prelude::*;
use smallvec::SmallVec;
use std::cell::UnsafeCell;
use std::fmt::Write;
use std::sync::atomic;
use std::sync::atomic::Ordering;
use std::sync::RwLock;
use std::sync::{Arc, Weak};

use crate::ast::{AstNode, indent};

pub type TypeHandle = Arc<dyn Type>;

pub type FunctionHandle = Arc<Function>;

//pub type TypeID = u64;
#[derive(PartialEq, Eq, Ord, PartialOrd, Hash, Clone, Copy, Debug)]
pub struct TypeID(pub u64);

#[derive(PartialEq, Eq, Ord, PartialOrd, Hash, Clone, Copy, Debug)]
pub struct FunctionID(pub u64);

#[derive(PartialEq, Eq, Ord, PartialOrd, Hash, Clone, Copy, Debug)]
pub struct CtxID(pub u64);

#[derive(PartialEq, Eq, Ord, PartialOrd, Hash, Clone, Copy, Debug)]
pub struct SymbolID(pub u64);
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
                let s: String = v.iter().fold(String::new(), |p, ss| { p + ss.resolve() });

                write!(f, "{}", s)
            },
            Self::Function(fid) => write!(f, "{}", fid),
            Self::Type(tid) => write!(f, "{}", tid),
            Self::Scope(cid) => write!(f, "{}", cid),
        }
    }
}

pub struct Import {
    pub alias: Option<StringSymbol>,
    pub resolution: Resolution,
}

pub struct Export {
    pub alias: Option<StringSymbol>,
    pub resolution: Resolution,
}

impl std::fmt::Display for Import {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "({})", self.0)
        write!(f, "use {} {}", self.resolution, self.alias.map(|ss| "as ".to_owned() + ss.resolve()).unwrap_or(String::new()))
    }
}

impl std::fmt::Display for Export {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "({})", self.0)
        write!(f, "export {} {}", self.resolution, self.alias.map(|ss| "as ".to_owned() + ss.resolve()).unwrap_or(String::new()))
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

/// This links a call to a function, and forces creation of
/// monomorphized versions of any generic functions.
pub struct MonomorphisedFunction {
    //pub munged_id:
//
}

pub struct Function {
    pub signature: FunctionSignature,
    pub id: FunctionID,

    params: Vec<(TypeID, String)>,
    returns: TypeID,

    from: Implementation,
}

impl Function {}

//pub type CtxID = u64;

//pub type GlobalFunctionID = (CtxID, FunctionID);
//pub type GlobalSymbolID = (CtxID, SymbolID);

#[allow(dead_code)]
enum Implementation {
    Builtin {
        ll_content: String,
        ll_result: String,
        ll_vars: Vec<(String, String)>,
    },
}

#[allow(dead_code)]
pub struct FunctionImplementation {
    self_type: GlobalTypeID,
    name: String,
    return_type: GlobalTypeID,
    params: Vec<(GlobalTypeID, String)>,

    implementation: Implementation,
}

impl FunctionImplementation {
    /*pub fn new_from_builtin(
        self_type: TypeID,
        name: String,
        params: Vec<(TypeID, String)>,
        return_type: TypeID,
        ll_vars: Vec<(String, String)>,
        ll_result: String,
        ll_content: String,
    ) -> FunctionImplementation {
        FunctionImplementation {
            self_type,
            name,
            return_type,
            params,
            implementation: Implementation::Builtin {
                ll_content,
                ll_result,
                ll_vars,
            },
        }
    }*/

    pub fn encode_definition(&self) -> Result<String, Box<dyn std::error::Error>> {
        let mut builder = String::new();

        writeln!(&mut builder, "; encoding a method by name {}", self.name)?;
        //todo!("Method encode not complete")

        Ok(builder)
    }

    //pub fn encode_reference(&self,
}

const TYPE_SIGNATURE_DUPLICATE_MAX_FREQ: usize = 3;

static CTX_ID: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);

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
pub struct GlobalCtxNode {
    canonical_local_name: String,

    children: DashMap<String, Arc<GlobalCtxNode>>,
    parent: Option<Weak<GlobalCtxNode>>,

    type_ctx: Arc<TypeCtx>,
    func_ctx: Arc<FuncCtx>,
    quark: super::Quark,

    selfref: Weak<GlobalCtxNode>,
    id: CtxID,

    imports: DashMap<StringSymbol, Import>,
    exports: DashMap<StringSymbol, Export>,
}

impl GlobalCtxNode {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{} Global context <canon {}> has id {}",
            indent(depth),
            self.canonical_local_name,
            self.id);

        let _ = writeln!(
            f,
            "{} Imports:",
            indent(depth + 1));

        let _ = writeln!(
            f,
            "{} Exports:",
            indent(depth + 1));
    }

    fn type_ctx(&self) -> Arc<TypeCtx> {
        self.type_ctx.clone()
    }

    fn new(
        name: &str,
        id: CtxID,
        parent: Option<Weak<GlobalCtxNode>>,
        global: Option<Weak<GlobalCtxNode>>,
    ) -> Arc<GlobalCtxNode> {
        let ctxnode = Arc::new_cyclic(|wr| GlobalCtxNode {
            canonical_local_name: name.to_owned(),
            children: DashMap::new(),
            type_ctx: Arc::new(TypeCtx::new(id)),
            func_ctx: Arc::new(FuncCtx::new(id)),

            imports: DashMap::new(),
            exports: DashMap::new(),

            selfref: wr.clone(),
            quark: match global {
                Some(global) => super::Quark::new_within(*wr, global),
                None => super::Quark::new_within(wr.clone(), *wr),
            },
            parent,
            id,
        });

        ctxnode
    }

    fn get_selfref_arc(&self) -> Option<Arc<GlobalCtxNode>> {
        unsafe { self.selfref.upgrade() }
    }

    fn name(&self) -> &str {
        self.canonical_local_name.as_str()
    }

    /*fn type_ctx(&self) -> Arc<TypeCtx> {
        self.type_ctx.clone()
    }*/

    fn nsctx_for(&self, scope: &[&str]) -> Option<Arc<GlobalCtxNode>> {
        //match scope {
        //[last] //
        match scope {
            [] => self.get_selfref_arc(),
            [first, rest @ ..] => {
                //self.
                self.children
                    .get(first.to_owned())
                    .map(|nsctx| nsctx.nsctx_for(rest))
                    .flatten()
            }
        }
    }

    fn register_nsctx(&self, scope: &[&str], into: &DashMap<CtxID, Arc<GlobalCtxNode>>) {
        match scope {
            [] => {
                // no action for empty case, this is base case (self is already registered)
            }
            [first, rest @ ..] => {
                self.children
                    .entry(first.to_string())
                    .or_insert_with(|| {
                        let id = CTX_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                        let gcn = GlobalCtxNode::new(
                            first,
                            CtxID(id),
                            None,
                            Some(Arc::downgrade(&GlobalCtx::get().get_global())),
                        );
                        into.insert(CtxID(id), gcn.clone());
                        gcn
                    })
                    .register_nsctx(rest, into);
            }
        }
    }

    //fn register_type_ctx(&self, scope: &[&'input str])

    //fn register_type_ctx
}

pub struct GlobalCtx {
    entry: Arc<GlobalCtxNode>,

    contexts: DashMap<CtxID, Arc<GlobalCtxNode>>,

    functions: DashMap<GlobalFunctionID, Arc<FunctionImplementation>>,
    types: DashMap<GlobalTypeID, TypeHandle>,

}

impl GlobalCtx {
    pub fn get_global(&self) -> Arc<GlobalCtxNode> {
        self.entry.clone()
    }

    pub fn get() -> &'static GlobalCtx {
        static GCTX: GlobalCtx = GlobalCtx::new();

        &GCTX
    }

    pub fn new() -> GlobalCtx {
        GlobalCtx {
            entry: GlobalCtxNode::new(
                "global",
                CtxID(CTX_ID.fetch_add(1, Ordering::SeqCst)),
                None,
                None,
            ),
            contexts: DashMap::new(),
            functions: DashMap::new(),
            types: DashMap::new(),
        }
    }

    /// tries to get a namespace ctx if one exists, and None if not
    #[allow(unused)]
    fn get_nsctx(&self, scope: &[&str]) -> Option<Arc<GlobalCtxNode>> {
        self.entry.nsctx_for(scope)
    }

    /// tries to get a namespace ctx if one exists, and not then creates the necessary path of
    /// nsctxs to have one to return. This builds out the tree, but can make future ns lookups
    /// believe that an ns was actually declared/referenced and should thus be avoided for
    /// strict read-query lookups
    #[allow(unused)]
    fn get_or_create_nsctx(&self, scope: &[&str]) -> Arc<GlobalCtxNode> {
        self.entry.register_nsctx(scope, &self.contexts);

        self.get_nsctx(scope).expect(
            "Created nsctx was not found directly afterward, failed to create? Failed to search?",
        )
    }

    #[allow(unused)]
    /// tries to get a namespace ctx if one exists, and not then creates the necessary path of
    /// nsctxs to have one to use for the return value. This builds out the tree, but can make future ns lookups
    /// believe that an ns was actually declared/referenced and should thus be avoided for
    /// strict read-query lookups. This returns the TypeCtx directly off of the
    /// internal NS looked up by `scope`, so that the last level type (decl itself) can be inserted
    pub fn get_or_create_typectx(&self, scope: &[&str]) -> Arc<TypeCtx> {
        self.get_or_create_nsctx(scope).type_ctx()
    }

    pub fn register_name_ctx(&self, scope: &[&str]) {
        match scope {
            // global scope handled same as default scope
            ["global", rest @ ..] | [rest @ ..] => {
                self.entry.register_nsctx(rest, &self.contexts);
            }
        }
    }

    pub fn by_id(&self, id: CtxID) -> Option<Arc<GlobalCtxNode>> {
        self.contexts.get(&id).map(|opt| opt.value().to_owned())
    }
}

/// Interior mutable container representing a function context
/// (set of functions from a module context)
pub struct FuncCtx {
    ctx_id: CtxID,
    //by_id: DashMap<FunctionID, FunctionHandle>,
    //by_signature: DashMap<FunctionSignature, FunctionHandle>,
    by_name: DashMap<StringSymbol, Vec<GlobalFunctionID>>,

    all: Vec<FunctionHandle>,
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

    pub fn define(&self, mut newfunc: Function) -> FunctionID {
        //let id = self.id_gen;
        //self.id_gen += 1;
        //newfunc.set_id(id);
        //newfunc.id = FunctionID(id);

        //let id = self.all.len();
        let id = FunctionID(self.all.len() as u64);

        newfunc.id = id;

        let fh = Arc::new(newfunc);

        self.all.push(fh);

        id

        //self.by_id.insert(FunctionID(id), fh.clone());
    }

    pub fn lookup(&self, fid: FunctionID) -> Option<FunctionHandle> {
        /*self.by_id
        .get(&fid)
        .map(|map_entry| map_entry.value().clone())*/
        //unimplemented!()
        self.all.get(fid.0 as usize).map(|r| r.clone())
    }

    pub fn query(
        &self,
        name: Option<StringSymbol>,
        params: &[Option<TypeID>],
        returns: Option<TypeID>,
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

                    for (arg_opt, param) in params.iter().zip(f.params.iter().map(|(tid, _)| tid)) {
                        if let Some(arg) = arg_opt {}
                    }

                    unimplemented!()
                } else {
                    false
                }
            })
            .collect();

        //self.by_name.get(&name).map(|map_entry| map_entry.value().first())

        unimplemented!()
    }

    /*pub fn id_to_sig(&self, fid: FunctionID) -> Option<TypeSignature> {
        self.
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
    pub fn new(within: CtxID) -> TypeCtx {
        TypeCtx {
            ctx_id: within,
            types: DashMap::new(),
            signatures: DashMap::new(),
            ids: DashMap::new(),
        }
    }

    pub fn define<T: 'static>(&self, mut newtype: T) -> TypeID
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
