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

use crate::{cst, cst::expressions::ExpressionWrapper, cst::TypeReference, helper::interner::IStr};

//use super::GenericConstraint;

mod makers {
    use super::*;
    use crate::cst;
    use crate::helper::interner::IStr;

    pub fn new_struct(
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
    }
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
pub struct NodeReference {
    node_id: OnceCell<CtxID>,
    within: CtxID,
    relative_path: Vec<IStr>,
}

impl NodeReference {
    /// If this reference
    pub fn resolve(&self, _within: &NodeReference) {
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

    node_id: OnceCell<CtxID>,

    //node_id: CtxID,
    generics: Vec<cst::GenericHandle>,

    children: DashMap<IStr, NodeReference>,

    /// UNSAFE: no deref is allowed unti node
    /// itself is frozen, no modifications
    /// of these fields are allowed unless node
    /// is unfrozen and the modification is
    /// guarded by a OneWayBoolGuard
    parent: OnceCell<CtxID>,
    global: OnceCell<CtxID>,

    inner: NodeUnion,

    implementations_in_scope: RwLock<Vec<Implementation>>,

    frozen: OneWayBool,
    //implementations_for_self: RwLock<Vec<Implementation>>,
}

struct OneWayBool {
    /// State holds either -1 (if fused) or an
    /// integer number of writers that block
    /// fusing the bool
    state: AtomicIsize,
    // If quick has been set to true in any thread,
    // then we know it has long since been set to true
    // and we don't need to do any atomic sync
    //quick: UnsafeCell<bool>,
}

impl OneWayBool {
    pub fn new() -> OneWayBool {
        let r = OneWayBool {
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
        match self.state.compare_exchange(0, -1, Ordering::SeqCst, Ordering::SeqCst) {
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
    guards: &'b OneWayBool,
}

impl<'b> std::ops::Drop for OneWayBoolGuard<'b> {
    fn drop(&mut self) {
        self.guards
            .state
            .fetch_sub(1, std::sync::atomic::Ordering::Release);
    }
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
        parent: OnceCell<CtxID>,
        global: OnceCell<CtxID>,
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
            implementations_in_scope: RwLock::new(Vec::new()),
            frozen: OneWayBool::new(),
        };

        //let mut as_ptr = Box::into_raw(Box::new(n));

        /*let pinned = unsafe {
            (*as_ptr).selfref = Some(NonNull::new(as_ptr).expect("as_ptr should not be null"));

            // this should be fine but no promises.
            // Currently there is no way to construct a pinned box in place
            // from a raw pointer
            Pin::new_unchecked(Box::from_raw(as_ptr))
        };*/

        Contexts::instance().intern(n)
    }

    /*pub fn set_parent(&mut self, parent: WeakNodeHandle) {
        let guard = self
            .frozen
            .block()
            .expect("tried to add a child to a frozen node");

        self.parent = parent;

        std::mem::drop(guard);
    }

    pub fn add_child(&self, child: Pin<Box<Node>>) {
        let name = child.name;

        // Since we have the only valid-to-deref handle to the child,
        // we can freely set parent
        child.set_parent(self.selfref);

        self.children.insert(name, child);
    }*/
}

pub enum NodeUnion {
    Type(TypeDefinition),
    Function(FunctionDefinition),
    Global(!),
    Empty(),
    //Implementation(Implementation),
}

/// A TypeDefinition is ...
pub struct TypeDefinition {
    fields: Vec<FieldMember>,
}

#[derive(Debug, Clone)]
/// A fieldmember can be either a method or a sized field.
/// For value types within the inherent impl context, fields exist within
/// the structural record and methods are fully devirtualized.
///
/// For virtual types, fieldmembers exist as entries within the virttype vtable whether
/// they are methods or variables.
pub struct FieldMember {
    /// Every attribute of a type is named (even if not uniquely).
    pub name: IStr,

    /// Every field is uniquely described by the pair of name and type
    pub ftype: TypeReference,

    /// Any field may have a provided default value for a member.
    ///
    /// Struct inherent implementations of functions demand default falues,
    /// but virtual types do not. Every other (known) instance of FieldMember does not require
    /// any default literal value
    ///
    /// A default value is not really a "value", but is instead implicitly a callable with the
    /// contents wrapped in a parameterless closure. These closures have static constants all
    /// visible to them, as well as typical literals and all imported constants.
    pub default: (),
    //pub default: Callable<()>,
}

pub struct FunctionDefinition {
    parameters: Vec<(IStr, TypeReference)>,
    return_type: TypeReference,

    implementation: ExpressionWrapper,
}

/// Implementations are not a variant within NodeUnion because they don't actually represent
/// any referenceable symbol. Instead, an implementation (with constraints) is "applied"
/// as it exists to the matching children of any node that has "imported" it (which
/// implicitly includes the context in which it is defined)
///
/// Eventually, implementations might be a named property that can be imported and act as a regular
/// node.
pub struct Implementation {
    of_type: TypeReference,
    for_type: TypeReference,

    implementation: !,
}

type OwningNodeHandle = Pin<Box<Node>>;
type WeakNodeHandle = Option<NonNull<Node>>;

/// An Instance is a (partially | fully) resolved
/// representation of "some type".
///
/// Types can usually have instances, modules and functions
/// aren't really types and thus can't really have "instances",
/// though a function is/can be held as an "instance" of a type.
///
/// An Instance is basically best thought of as the type
/// information for a variable or expression
pub struct Instance {
    narrowings: TypeConstraint,
}

/// Represents a fully resolved type, with all generics filled in
/// (or at least a required subset of them)
pub struct TypeInstiation {
    instantiates: WeakNodeHandle,
    generic_arguments: DashMap<cst::GenericHandle, TypeInstiation>,
}

pub enum TypeConstraint {
    /// A "has" type constraint states that the type must "have"
    /// a symbol with the given name with the given Instance,
    /// for example "v.foo(bar)" is described by `Instance(v)`
    /// having a Has constraint with symbol `foo` and
    /// type `(Instance(bar)) -> *`.
    ///
    /// This is not implemented yet since we haven't explored
    /// type inference (only simple deduction so far)
    Has(!),

    /// An "of" type constraint states that the type must
    /// accept a compatible generic argument list to the
    /// one provided
    Of(!),

    /// An "in" type constraint states that the target type
    /// must be exported (either aliased or declared) within a specific module
    ///
    /// Refers to a module context that has been fully resolved by name
    In(WeakNodeHandle),

    /// A "named" type constraint states that the target type
    /// has a base name matching the provided string
    ///
    /// TODO: consider adding a "generic redirect" here
    /// that can provide for GATs later on or for
    /// templated outer generics as at least a "poor man's GATs"
    Named(IStr),

    /// If a deduction/reduction can be made that narrows to
    /// a single, fully defined, type (such as accessing a field
    /// on a known type struct of a known type member), then
    /// a "complete" TypeConstraint can be constructed that
    /// need not specify any more complicated relationships
    Complete(TypeInstiation),

    /// If multiple constraints are put on a single object
    /// then Multiple can encompass them.
    ///
    /// This works for a variable that is constrained in two or more places,
    /// or for example with generics and specialization
    Multiple(Vec<TypeConstraint>),

    /// If an instance has no guiding information "yet" (for example,
    /// a simple generic argument or a wildcard during assignment),
    /// then Unconstrained can be used as a placeholder
    /// and folded into Multiple later on
    Unconstrained(),
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
