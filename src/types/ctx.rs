use super::impls::*;

use dashmap::DashMap;
use smallvec::SmallVec;
use std::cell::UnsafeCell;
use std::fmt::Write;
use std::sync::RwLock;
use std::sync::{Arc, Weak};
use std::sync::atomic;
use std::sync::atomic::Ordering;

pub type TypeHandle = Arc<RwLock<dyn Type>>;

//pub type TypeID = u64;
#[derive(PartialEq, Eq, Ord, PartialOrd, Hash, Clone, Copy)]
pub struct TypeID(pub u64);

/*impl std::ops::Deref for TypeID {
    type Target = u64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}*/

//pub type CtxID = u64;
#[derive(PartialEq, Eq, Ord, PartialOrd, Hash, Clone, Copy)]
pub struct CtxID(pub u64);

#[derive(PartialEq, Eq, Ord, PartialOrd, Hash, Clone, Copy)]
pub struct SymbolID(pub u64);
//pub type SymbolID = u64;

pub type GlobalTypeID = (CtxID, TypeID);
pub type GlobalSymbolID = (CtxID, SymbolID);

#[allow(dead_code)]
enum Implementation {
    Builtin {
        ll_content: String,
        ll_result: String,
        ll_vars: Vec<(String, String)>,
    },
}

#[allow(dead_code)]
pub struct Method {
    self_type: TypeID,
    name: String,
    return_type: TypeID,
    params: Vec<(TypeID, String)>,

    implementation: Implementation,
}

impl Method {
    pub fn new_from_builtin(
        self_type: TypeID,
        name: String,
        params: Vec<(TypeID, String)>,
        return_type: TypeID,
        ll_vars: Vec<(String, String)>,
        ll_result: String,
        ll_content: String,
    ) -> Method {
        Method {
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
    }

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
struct GlobalCtxNode {
    canonical_local_name: String,
    children: DashMap<String, Arc<GlobalCtxNode>>,
    //localtypes: DashMap<&'input str, Arc<Ctx<'input>>>,
    type_ctx: Arc<TypeCtx>,
    func_ctx: Arc<FuncCtx>,

    selfref: std::cell::UnsafeCell<Weak<GlobalCtxNode>>,
    id: CtxID,
}

impl GlobalCtxNode {
    fn new(name: &str, id: CtxID) -> Arc<GlobalCtxNode> {
        let ctxnode = GlobalCtxNode {
            canonical_local_name: name.to_owned(),
            children: DashMap::new(),
            type_ctx: Arc::new(TypeCtx::new()),
            func_ctx: Arc::new(FuncCtx::new()),
            selfref: UnsafeCell::new(Weak::default()),
            id,
        };

        std::sync::atomic::compiler_fence(std::sync::atomic::Ordering::SeqCst);
        let owner = unsafe {
            let owner = Arc::new(ctxnode);

            // TODO: check how to avoid this being unsafe/potentially UB (*const is
            // given by Arc::as_ptr, not sure how to make the circular
            // ref in a safe way during init without requiring mutex + interior mut
            GlobalCtxNode::set_selfref(Arc::as_ptr(&owner), Arc::downgrade(&owner));

            owner
        };
        std::sync::atomic::compiler_fence(std::sync::atomic::Ordering::SeqCst);

        owner
    }

    // takes ptr to hopefully avoid aliasing concerns
    /// Only call during a context where s is not concurrently
    /// accessible!! This modifies s.selfref in a way that is unchecked
    unsafe fn set_selfref(s: *const Self, sr: Weak<Self>) {
        //(*s).selfref = sr;
        *((*s).selfref).get() = sr
    }

    fn get_selfref_arc(&self) -> Option<Arc<GlobalCtxNode>> {
        unsafe {
            // NOTE: relies on selfref being initialized during construction before this
            // point
            (*self.selfref.get()).upgrade()
        }
    }

    fn name(&self) -> &str {
        self.canonical_local_name.as_str()
    }

    fn type_ctx(&self) -> Arc<TypeCtx> {
        self.type_ctx.clone()
    }

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
                        let gcn = GlobalCtxNode::new(first, CtxID(id));
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
}

impl GlobalCtx {
    pub fn new() -> GlobalCtx {
        GlobalCtx { entry: GlobalCtxNode::new("global", CtxID(CTX_ID.fetch_add(1, Ordering::SeqCst))), contexts: DashMap::new(), }
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
}

pub struct FuncCtx {
}

impl FuncCtx {
    pub fn new() -> FuncCtx {
        FuncCtx {}
    }
}

/// Interior mutable container representing a type context
pub struct TypeCtx {
    types: DashMap<TypeID, TypeHandle>,
    signatures:
        DashMap<TypeSignature, SmallVec<[TypeID; TYPE_SIGNATURE_DUPLICATE_MAX_FREQ]>>,
    ids: DashMap<TypeID, TypeSignature>,
}

impl TypeCtx {
    pub fn new() -> TypeCtx {
        TypeCtx {
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
        let arcd = Arc::new(RwLock::new(newtype));
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
