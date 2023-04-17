use crate::avec::AtomicVec;
use crate::cst::{
    ScopedName, SyntacticTypeReference, SyntacticTypeReferenceInner, SyntacticTypeReferenceRef,
    TopLevel, FunctionBuiltin, StructuralTyAttrs,
};
use crate::helper::interner::Internable;
use crate::{avec::AtomicVecIndex, cst::UseDeclaration};
use either::Either;
use itertools::Itertools;
use smallvec::smallvec;
use tracing::info;

use crate::ast;

use std::collections::HashMap;
use std::default::default;
use std::{
    ptr::NonNull,
    sync::{
        atomic::{compiler_fence, AtomicIsize, Ordering},
        Mutex,
    },
};

use dashmap::DashMap;
use once_cell::sync::OnceCell;

use crate::{
    compile::parse_tree::ParseTreeNode,
    cst,
    helper::interner::{IStr, SpurHelper},
};

/// Designed to be a "global"
/// interner of sorts for contexts, storing their
/// handles for the overall tree to be cheap to
/// traverse and build without worrying about nice lifetimes
/// or refcounting
//#[derive(Default)]
pub struct Contexts {
    owning: AtomicVec<Node>,

    node_ids: Mutex<Vec<CtxID>>,
    //by_path: DashMap<Box<[IStr]>, CtxID>,
}

impl Contexts {
    pub fn new() -> Self {
        Self {
            owning: AtomicVec::new(),
            //by_path: DashMap::new(),
            node_ids: Mutex::new(Vec::new()),
        }
    }

    pub fn intern(&self, node: Node) -> CtxID {
        let (index, _ref) = self.owning.push(node);
        let id = CtxID(index);

        self.owning
            .get(index)
            .node_id
            .set(id)
            .expect("User already set id for node");

        self.node_ids.lock().unwrap().push(id);

        id

        // TODO: need to add by_path for lookups, will require
        // nodes to know their path
    }

    pub fn get_ids(&self) -> Vec<CtxID> {
        self.node_ids.lock().unwrap().clone()
    }

    pub fn get(&self, r: &CtxID) -> &Node {
        self.owning.get(r.0)
        //.expect("was given an incorrectly constructed CtxID in a NodeReference, source was not Contexts?")
    }

    pub fn instance() -> &'static Contexts {
        //static s: Contexts = Contexts::default();

        lazy_static! {
            static ref S: Contexts = Contexts::new();
        }

        &S
    }
}

#[derive(PartialEq, Eq, Ord, PartialOrd, Hash, Clone, Copy)]
pub struct CtxID(pub AtomicVecIndex);

impl std::fmt::Debug for CtxID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
        //self.0::<as std::fmt::Display>::fmt(f)
        //self.resolve().fmt(f)
    }
}

impl CtxID {
    pub fn resolve(self) -> &'static Node {
        Contexts::instance().get(&self)
    }
}

impl CtxID {
    pub fn to_ref(&self) -> &Node {
        Contexts::instance().get(self)
    }
}

/// A NodeReference is a resolvable reference
/// to a node. It is constructed early on while building the CST
/// for import statements, for scoping and any type reference
/// it is implicitly at the root of any bare call or reference.
/// (method call syntax is special and does not resolve the same way)
///
/// When resolved, it will point to a node ID
///
/// Should not be constructed except by Contexts. This
/// guarantees that no panic will ever occur when calling ::get()
///
#[derive(Debug, Clone)]
pub struct NodeReference {
    node_id: OnceCell<CtxID>,
    within: CtxID,
    relative_path: Vec<IStr>,
}

impl NodeReference {
    /// If this reference
    pub fn resolve(&self) {
        match self.node_id.get() {
            Some(_id) => (), // we've already been resolved
            None => (),
        }
    }
}

/// A Node is the building block for a type, module, or function
///
/// It represents the "thing" that is being imported at any given
/// place in the code, it acts almost like a type itself in a lot
/// of ways. It has generics, it has members in the form of
/// associated functions, types, or even submodules!
///
/// Note for any weak handle: they may *only*
/// ever be deref'd when self.frozen is true
///
pub struct Node {
    name: IStr,

    pub node_id: OnceCell<CtxID>,

    //node_id: CtxID,
    pub generics: Vec<(IStr, SyntacticTypeReferenceRef)>,

    pub children: DashMap<IStr, CtxID>,

    pub public: bool,

    /// UNSAFE: no deref is allowed until node
    /// itself is frozen, no modifications
    /// of these fields are allowed unless node
    /// is unfrozen and the modification is
    /// guarded by a OneWayBoolGuard
    pub parent: Option<CtxID>,
    pub global: Option<CtxID>,

    pub inner: NodeUnion,

    pub use_statements: Vec<UseDeclaration>,

    pub builtin: bool,

    frozen: Fuse,
}

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Node")
            .field("name", &self.name)
            .field("node_id", &self.node_id.get().unwrap())
            .field("generics", &self.generics)
            .field("children", &self.children)
            .field("parent", &self.parent)
            .field("global", &self.global)
            .field("inner", &self.inner)
            .field("frozen", &self.frozen)
            .finish()
    }
}

impl Node {
    pub fn canonical_typeref(&self) -> SyntacticTypeReferenceRef {
        fn build_back_up(cid: CtxID) -> ScopedName {
            let n = cid.resolve();
            let mut base = match n.parent {
                Some(p) => {
                    let s = build_back_up(p);
                    s
                }
                None => ScopedName { scope: default() },
            };

            base.scope.push(n.name);

            base
        }

        let s = build_back_up(*self.node_id.get().unwrap());

        tracing::error!("implement actual generics for canonical ref here");
        SyntacticTypeReference {
            inner: SyntacticTypeReferenceInner::Single { name: s },
            info: cst::NodeInfo::Builtin, // for now, we can get origins and such later
            id: SyntacticTypeReferenceRef::new_nil(),
        }
        .intern()
    }

    // we know that frozen only
    // ever mutates in "one direction": unfrozen -> frozen
    //
    // This means that if we ever read a frozen value
    // from the region,
    pub fn is_frozen(&self) -> bool {
        self.frozen.is_fused()
    }

    pub fn new(
        name: IStr,
        generics: Vec<(IStr, SyntacticTypeReferenceRef)>,
        parent: Option<CtxID>,
        global: Option<CtxID>,
        inner: NodeUnion,
        public: bool,
        mut use_statements: Vec<UseDeclaration>,
    ) -> CtxID {
        use_statements.push(UseDeclaration {
            node_info: cst::NodeInfo::Builtin,
            public: false,
            scope: ScopedName {
                scope: smallvec!["global".intern(), "std".intern()],
            },
            alias: Some("std".intern()),
        });

        let generics = generics
            .into_iter()
            .map(|(name, ty)| (name, ty))
            .collect_vec();

        let builtin = inner.is_builtin();

        let n = Node {
            node_id: OnceCell::new(),
            name,
            generics,
            parent,
            global,
            inner,
            children: DashMap::new(),
            builtin,
            //implementations_in_scope: RwLock::new(Vec::new()),
            frozen: Fuse::new(),
            public,
            use_statements,
        };

        Contexts::instance().intern(n)
    }

    pub fn from_decl(decl: TopLevel, parent: CtxID, global: CtxID) -> Option<CtxID> {
        tracing::debug!("Got a decl: {decl:?}");
        match decl {
            TopLevel::Namespace(n) => {
                let cst::NamespaceDefinition {
                    name,
                    node_info,
                    contents,
                    public,
                } = n;

                let node = Self::from_outer(
                    contents,
                    name.expect("namespace had no name?"),
                    vec![], // TODO
                    Some(parent),
                    global,
                    n.public,
                );

                parent
                    .to_ref()
                    .children
                    .entry(name.expect("namespace had no name?"))
                    .or_insert(node);

                Some(node)
            }
            TopLevel::Function(f) => {
                let name = f.name;
                let public = f.public;
                let generics = f.generics.clone();
                let mut fd = ast::types::FunctionDefinition::from_cst(f);
                let imp = fd.implementation.take();

                let inner = NodeUnion::Function(fd, Mutex::new(imp));

                let node = Self::new(
                    name,
                    generics, // TODO
                    Some(parent),
                    Some(global),
                    inner,
                    public,
                    vec![],
                );

                Contexts::instance()
                    .get(&parent)
                    .children
                    .insert(name, node);

                Some(node)
            }

            TopLevel::Impl(i) => {
                println!("Detected an impl!");
                let cst::ImplementationDefinition {
                    info,
                    generics,
                    for_type,
                    of_trait,
                    functions,
                    fields,
                } = i;

                todo!()
            }
            TopLevel::Trait(t) => {
                println!("Detected a trait!");

                todo!()
            }
            TopLevel::Struct(s) => {
                let cst::StructDefinition {
                    info,
                    generics,
                    name,
                    fields,
                    public,
                    methods,
                    attrs,
                } = s;

                let fields = fields
                    .into_iter()
                    .map(|field| {
                        let cst::Field {
                            info,
                            has_type,
                            has_name,
                        } = field;

                        let field = ast::types::FieldMember {
                            name: has_name,
                            has_type: Some(has_type),
                            initialization: None, // TODO
                        };

                        field
                    })
                    .collect();

                let td = ast::types::StructuralDataDefinition {
                    fields,
                    methods: HashMap::new(),
                    attrs,
                };

                let inner = NodeUnion::Type(Mutex::new(td));

                let node = Self::new(
                    name,
                    generics,
                    Some(parent),
                    Some(global),
                    inner,
                    public,
                    vec![],
                );

                let mut method_pairs = HashMap::new();

                for method in methods {
                    let mname = method.name;

                    let node = Self::from_decl(TopLevel::Function(method), node, global);

                    method_pairs.insert(
                        mname,
                        node.expect("inserted a function, should get back id"),
                    );

                    //Self::from_parse(n, name, parent, global);
                }

                if let NodeUnion::Type(inner) = &node.resolve().inner {
                    inner.lock().unwrap().methods = method_pairs;
                } else {
                    unreachable!()
                }

                parent
                    .to_ref()
                    .children
                    .entry(name)
                    .and_modify(|prior| {
                        let prior = prior.resolve();
                        let p = prior.node_id.get().unwrap().to_ref();
                        println!("");
                        println!("There was an existing entry! Conflict between:");
                        println!("{p}");
                        println!("{}", node.to_ref());
                        println!("");
                    })
                    .or_insert(node);
                /*.or_insert(NodeReference {
                    node_id: child.into(),
                    within: node,
                    relative_path: vec![name],
                });*/
                Some(node)
            }
            TopLevel::UseDeclaration(ud) => {
                // just pass here, already added them
                info!("ignoring use decl since already added");

                None
            }
            _ => todo!(),
        }
    }

    pub fn from_outer(
        o: cst::OuterScope,
        name: IStr,
        generics: Vec<(IStr, SyntacticTypeReferenceRef)>,
        parent: Option<CtxID>,
        global: CtxID,
        public: bool,
    ) -> CtxID {
        let cst::OuterScope {
            node_info,
            declarations,
        } = o;

        //let parent = parent.map(|e| OnceCell::with_value(e)).unwrap_or(OnceCell::new());
        //let global = global.map(|e| OnceCell::with_value(e)).unwrap_or(OnceCell::new());
        // pre-collect use decls

        let mut use_decls = Vec::new();

        for decl in declarations.iter() {
            if let TopLevel::UseDeclaration(u) = decl {
                use_decls.push(u.clone());
            }
        }

        let node = Self::new(
            name,
            generics,
            parent,
            Some(global),
            NodeUnion::Empty(),
            public,
            use_decls,
        );

        // iterate through decls and put as children here
        for decl in declarations {
            Self::from_decl(decl, node, global);
        }

        node
    }

    pub fn from_parse(n: ParseTreeNode, name: IStr, parent: CtxID, global: CtxID) -> CtxID {
        let ParseTreeNode {
            files,
            parsed,
            children,
        } = n;

        let inner = NodeUnion::Empty();

        //let node = Self::new(name, vec![], parent.clone(), global.clone(), inner);

        //let mut aggregate = cst::OuterScope::new(cst::NodeInfo::Builtin, vec![]);
        let mut aggregate = Vec::new();

        for f in parsed {
            if let Some(v) = f.value {
                let cst::OuterScope {
                    node_info,
                    mut declarations,
                } = v;
                aggregate.append(&mut declarations);
                //let child = Self::from_outer(v, name, parent.clone(), global.clone());
            } else {
                println!("invalid parse node, had no value")
            }
        }

        let aggregate = cst::OuterScope::new(cst::NodeInfo::Builtin, aggregate);

        let s = Self::from_outer(aggregate, name, vec![], Some(parent), global, true); // TODO:
                                                                                       // public
                                                                                       // markers
                                                                                       // for
                                                                                       // nodes

        /*let global = match global {
            Some(v) => Some(v),
            None => Some(s),
        };*/

        for (cname, child) in children {
            let cid = Self::from_parse(child, cname, s.into(), global.into());
            s.to_ref().children.insert(cname, cid);
            /*NodeReference {
                    node_id: cid.into(),
                    within: s,
                    relative_path: vec![cname],
                },
            );*/
        }

        s
    }

    pub fn get_struct_attrs(&self) -> StructuralTyAttrs {
        match &self.inner {
            NodeUnion::Type(t) => t.lock().unwrap().attrs,
            _ => panic!("tried to get whether a non-ty has attrs")
        }
    }
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.inner {
            NodeUnion::Type(td) => {
                writeln!(
                    f,
                    "struct {} with fields {:?}",
                    self.name.resolve(),
                    td.lock().unwrap().fields
                )
            }
            NodeUnion::Empty() => {
                writeln!(f, "namespace {}", self.name.resolve())
            }
            _ => todo!(),
        }
    }
}

#[derive(Debug)]
pub enum NodeUnion {
    Type(Mutex<ast::types::StructuralDataDefinition>),
    Function(
        ast::types::FunctionDefinition,
        Mutex<Option<Either<cst::expressions::ExpressionWrapper, FunctionBuiltin>>>,
    ),

    Global(!),
    Empty(),
    //Implementation(Implementation),
}

#[derive(Debug)]
struct FunctionTemplate {
    template: String,
}

impl NodeUnion {
    pub fn is_builtin(&self) -> bool {
        match self {
            Self::Function(fd, fe) => {
                fe.lock().unwrap().as_ref().map(|v| v.is_right()).unwrap_or(false)
            },
            _ => false,
        }
    }
}

struct RefPtr<T> {
    inner: NonNull<T>,
}

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
        Self {
            inner: NonNull::dangling(),
        }
    }

    pub unsafe fn from(from: *const T) -> RefPtr<T> {
        // the cast here is only sound because this type exposes an immutable ref
        // to T only
        Self {
            inner: NonNull::new(from as *mut T).expect("was given a null `from`"),
        }
    }
}

#[derive(Debug)]
struct Fuse {
    /// State holds either -1 (if fused) or an
    /// integer number of writers that block
    /// fusing the bool
    state: AtomicIsize,
    // If quick has been set to true in any thread,
    // then we know it has long since been set to true
    // and we don't need to do any atomic sync
    //quick: UnsafeCell<bool>,
}

impl Fuse {
    pub fn new() -> Fuse {
        let r = Fuse {
            state: AtomicIsize::new(0),
            //quick: UnsafeCell::new(false),
        };
        compiler_fence(std::sync::atomic::Ordering::SeqCst);

        r
    }

    /// If returns None, then the bool is already fused
    /// so it is impossible to block fusing
    pub fn block(&self) -> Option<OneWayBoolGuard> {
        while let prior = self.state.load(std::sync::atomic::Ordering::Acquire) {
            if prior < 0 {
                return None;
            }

            let new = self.state.compare_exchange(
                prior,
                prior + 1,
                std::sync::atomic::Ordering::SeqCst,
                std::sync::atomic::Ordering::SeqCst,
            );

            if let Err(_) = new {
                continue;
            } else {
                return Some(OneWayBoolGuard { guards: &self });
            }
        }

        unreachable!("Very weird bug")
    }

    /// If returns true, then fusing was able to complete
    /// If false, then there were still open blocks
    pub fn try_fuse(&self) -> bool {
        match self
            .state
            .compare_exchange(0, -1, Ordering::SeqCst, Ordering::SeqCst)
        {
            Err(_) => return self.state.load(Ordering::SeqCst) == -1,
            Ok(_) => true,
        }
    }

    pub fn is_fused(&self) -> bool {
        //compiler_fence(std::sync::atomic::Ordering::Acquire);

        //let quick = unsafe { *self.quick.get() };
        let quick = false;

        // if already fused long enough for quick to have propagated a true
        // even without atomics, then we don't need
        quick || (self.state.load(std::sync::atomic::Ordering::Acquire) == -1)
    }
}

pub struct OneWayBoolGuard<'b> {
    guards: &'b Fuse,
}

impl<'b> std::ops::Drop for OneWayBoolGuard<'b> {
    fn drop(&mut self) {
        self.guards
            .state
            .fetch_sub(1, std::sync::atomic::Ordering::Release);
    }
}
