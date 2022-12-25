use std::{
    pin::Pin,
    ptr::NonNull,
    sync::{
        atomic::{compiler_fence, AtomicIsize, Ordering},
        RwLock,
    },
};

use dashmap::DashMap;
use once_cell::sync::OnceCell;

use crate::{
    ast,
    compile::parse_tree::ParseTreeNode,
    cst,
    cst::expressions::ExpressionWrapper,
    cst::SyntaxTypeReference,
    helper::interner::{IStr, SpurHelper},
};

//use super::GenericConstraint;

mod makers {
    use super::*;
    use crate::cst;
    use crate::helper::interner::IStr;

    /*pub fn new_struct(
        named: IStr,
        generics: Vec<cst::GenericHandle>,
        fields: Vec<FieldMember>,
    ) -> CtxID {
        let t = TypeDefinition { fields };
        let inner = NodeUnion::Type(t);

        let node = Node::new(named, generics, OnceCell::new(), OnceCell::new(), inner);

        node

        //within.add_child(node);
        //let inner = NodeUnion::
    }

    pub fn new_namespace(named: IStr, generics: Vec<cst::GenericHandle>) -> CtxID {
        let inner = NodeUnion::Empty();

        let node = Node::new(named, generics, OnceCell::new(), OnceCell::new(), inner);

        node
    }*/
}

/// Designed to be a "global"
/// interner of sorts for contexts, storing their
/// handles for the overall tree to be cheap to
/// traverse and build without worrying about nice lifetimes
/// or refcounting
//#[derive(Default)]
pub struct Contexts {
    owning: boxcar::Vec<Node>,

    by_path: DashMap<Box<[IStr]>, CtxID>,
}

impl Contexts {
    pub fn new() -> Self {
        Self {
            owning: boxcar::Vec::new(),
            by_path: DashMap::new(),
        }
    }

    pub fn intern(&self, node: Node) -> CtxID {
        let index = self.owning.push(node);
        let id = CtxID(index);

        self.owning
            .get(index)
            .unwrap()
            .node_id
            .set(id)
            .expect("User already set id for node");

        id

        // TODO: need to add by_path for lookups, will require
        // nodes to know their path
    }

    pub fn get(&self, r: &CtxID) -> &Node {
        self
            .owning
            .get(r.0)
            .expect("was given an incorrectly constructed CtxID in a NodeReference, source was not Contexts?")
    }

    pub fn instance() -> &'static Contexts {
        //static s: Contexts = Contexts::default();

        lazy_static! {
            static ref S: Contexts = Contexts::new();
        }

        &S
    }
}

#[derive(PartialEq, Eq, Ord, PartialOrd, Hash, Clone, Copy, Debug)]
pub struct CtxID(pub usize);

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
#[derive(Debug)]
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
#[derive(Debug)]
pub struct Node {
    name: IStr,

    node_id: OnceCell<CtxID>,

    //node_id: CtxID,
    generics: Vec<cst::GenericHandle>,

    children: DashMap<IStr, NodeReference>,

    /// UNSAFE: no deref is allowed until node
    /// itself is frozen, no modifications
    /// of these fields are allowed unless node
    /// is unfrozen and the modification is
    /// guarded by a OneWayBoolGuard
    parent: Option<CtxID>,
    global: Option<CtxID>,

    inner: NodeUnion,

    frozen: Fuse,
}

impl Node {
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
        generics: Vec<cst::GenericHandle>,
        parent: Option<CtxID>,
        global: Option<CtxID>,
        inner: NodeUnion,
    ) -> CtxID {

        let n = Node {
            node_id: OnceCell::new(),
            name,
            generics,
            parent,
            global,
            inner,
            children: DashMap::new(),
            //implementations_in_scope: RwLock::new(Vec::new()),
            frozen: Fuse::new(),
        };

        Contexts::instance().intern(n)
    }

    pub fn from_outer(
        o: cst::OuterScope,
        name: IStr,
        generics: Vec<cst::GenericHandle>,
        parent: Option<CtxID>,
        global: Option<CtxID>,
    ) -> CtxID {
        let cst::OuterScope {
            node_info,
            declarations,
        } = o;

        //let parent = parent.map(|e| OnceCell::with_value(e)).unwrap_or(OnceCell::new());
        //let global = global.map(|e| OnceCell::with_value(e)).unwrap_or(OnceCell::new());

        let node = Self::new(name, generics, parent, global.clone(), NodeUnion::Empty());

        // iterate through decls and put as children here
        for decl in declarations {
            println!("Got a decl: {decl:?}");
            use cst::TopLevel;
            match decl {
                TopLevel::Namespace(n) => {
                    let cst::NamespaceDefinition {
                        name,
                        node_info,
                        contents,
                        public,
                    } = n;

                    let child = Self::from_outer(
                        contents,
                        name.expect("namespace had no name?"),
                        vec![], // TODO
                        node.into(),
                        global.clone(),
                    );

                    node.to_ref()
                        .children
                        .entry(name.expect("namespace had no name?"));
                }
                TopLevel::Function(f) => {
                    let name = f.name;
                    //let cst::FunctionDefinition { info, public, name, body, return_type, params } = f;

                    let fd = ast::types::CallableDefinition::from_cst(f);

                    let inner = NodeUnion::Function(fd);

                    let child = Self::new(
                        name,
                        vec![], // TODO
                        Some(node),
                        global,
                        inner,
                    );

                    //let fd = ast::FunctionDefinition { parameters}
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
                }
                TopLevel::Trait(t) => {
                    println!("Detected a trait!");
                }
                TopLevel::Struct(s) => {
                    let cst::StructDefinition {
                        info,
                        generics,
                        name,
                        fields,
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
                                has_type: ast::types::InstanceConstraint::from_cst(has_type),
                            };

                            field
                        })
                        .collect();

                    let td = ast::types::StructuralDataDefinition { fields };

                    let inner = NodeUnion::Type(td);

                    let child = Self::new(name, generics, Some(node), global, inner);

                    node.to_ref()
                        .children
                        .entry(name)
                        .and_modify(|prior| {
                            let p = prior.node_id.get().unwrap().to_ref();
                            println!("");
                            println!("There was an existing entry! Conflict between:");
                            println!("{p}");
                            println!("{}", child.to_ref());
                            println!("");
                        })
                        .or_insert(NodeReference {
                            node_id: child.into(),
                            within: node,
                            relative_path: vec![name],
                        });
                }
                _ => todo!(),
            };
        }

        node
    }

    pub fn from_parse(
        n: ParseTreeNode,
        name: IStr,
        parent: Option<CtxID>,
        global: Option<CtxID>,
    ) -> CtxID {
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
            }
        }

        let aggregate = cst::OuterScope::new(cst::NodeInfo::Builtin, aggregate);

        let s = Self::from_outer(aggregate, name, vec![], parent.clone(), global.clone());

        let global = match global {
            Some(v) => Some(v),
            None => Some(s),
        };

        for (cname, child) in children {
            let cid = Self::from_parse(child, cname, s.into(), global.into());
            s.to_ref().children.insert(
                cname,
                NodeReference {
                    node_id: cid.into(),
                    within: s,
                    relative_path: vec![cname],
                },
            );
        }

        s
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
                    td.fields
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
    Type(ast::types::StructuralDataDefinition),
    Function(ast::types::CallableDefinition),
    Global(!),
    Empty(),
    //Implementation(Implementation),
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
