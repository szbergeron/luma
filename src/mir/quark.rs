use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use itertools::Itertools;
use tracing::{info, warn};
use uuid::Uuid;

use crate::{
    ast::{
        self, executor::Executor, resolver2::NameResolutionMessage, tree::CtxID,
        types::AbstractTypeReference,
    },
    avec::{AtomicVec, AtomicVecIndex},
    compile::per_module::{Content, ControlMessage, Destination, Earpiece, Message, Service},
    cst::{GenericHandle, TypeReference, ScopedName},
    helper::{interner::IStr, CompilationError, VecOps},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeID(AtomicVecIndex);

/// Quark instances are provided a single
/// function context to try to resolve within,
/// and convert that function into a fully
/// linearized, SSA form callable with types
/// fully resolved from a static-compilation
/// perspective
///
/// Inputs for types are TypeConstraints,
/// while outputs are references to actual Nodes
/// with IDs and filled in generics
pub struct Quark {
    type_args: Vec<SymbolicType>,

    //value: Linear,

    //allocations: HashMap<usize, Allocation>,
    allocations: Vec<Allocation>,

    allocation_references: HashMap<usize, OwnedAllocationReference>,

    variables: Vec<(IStr, AllocationReference)>,
    frames: Vec<usize>,

    earpiece: Earpiece,

    node_id: CtxID,
}

impl Quark {
    fn as_dest(&self) -> Destination {
        Destination { node: self.node_id, service: Service::Quark() }
    }

    pub fn for_node(node_id: CtxID, earpiece: Earpiece) -> Self {
        warn!("quark is being improperly initialized to make things happy");

        Self {
            type_args: Vec::new(),
            allocations: Vec::new(),
            allocation_references: HashMap::new(),
            variables: Vec::new(),
            frames: Vec::new(),
            earpiece,
            node_id,
        }
    }

    pub async fn thread(mut self, executor: &'static Executor) {
        info!("starts quark thread");

        match &mut *self.node_id.resolve().inner.lock().unwrap() {
            ast::tree::NodeUnion::Function(f) => {
                warn!("quark for a function starts up");
                self.entry(f).await
            }
            _ => {
                // we don't do anything in the other cases, we only make sense in the case of being
                // a function
                warn!(
                    "quark for node {:?} is shutting down, as it is not a function",
                    self.node_id
                );

                while let Ok(v) = self.earpiece.wait().await {
                    info!("Quark got a message");

                    self.earpiece.send(Message {
                        to: v.send_reply_to,
                        send_reply_to: Destination::nil(),
                        from: Destination {
                            node: self.node_id,
                            service: Service::Quark(),
                        },
                        conversation: v.conversation,
                        content: Content::Control(ControlMessage::CanNotReply()),
                    });
                }
            }
        }
    }

    pub fn start_resolve_typeref(&mut self, tb: &mut TypeReference) {
        let abstrakt: &mut AbstractTypeReference = match tb {
            TypeReference::Syntactic(s) => {
                let generic_args = self
                    .node_id
                    .resolve()
                    .generics
                    .iter()
                    .map(|(name, _tr)| *name)
                    .collect_vec();
                let abstrakt = s.to_abstract(generic_args.as_slice());

                *tb = TypeReference::Abstract(box abstrakt, *s);

                match tb {
                    TypeReference::Abstract(a, s) => &mut *a,
                    _ => unreachable!(),
                }
            }
            TypeReference::Abstract(a, s) => {
                panic!("this shouldn't have already been made abstract yet, that was our job!")
            }
        };

        let typ_bases = abstrakt.bases.read().unwrap();

        for base in typ_bases.iter() {
            match base {
                crate::ast::types::TypeBase::Generic(_) => {
                    todo!("we don't yet handle generics")
                }
                crate::ast::types::TypeBase::Resolved(_) => {
                    //todo!("we shouldn't be trying to handle an already resolved typeref")
                    // do nothing here, since maybe someone else resolved it?
                }
                crate::ast::types::TypeBase::UnResolved(r) => {
                    assert!(r.generics.is_empty()); // we don't yet handle generics

                    self.start_resolve_symref(r.named.clone());
                }
            }
        }
    }

    fn start_resolve_symref(&mut self, nr: ScopedName) {
        /*match self.name_to_ref.contains_key(&nr) {
            true => {
                // do nothing, already gonna resolve it
            }
            false => {
                let convo_id = self.next_convo();

                let cc = ConversationContext {
                    publish_as: None, // this isn't an import
                    remaining_scope: nr.clone().scope,
                    original_scope: nr.clone(),
                    // we search within parent here because we're a typeref within
                    // a Type, which implicitly looks for things within the parent
                    // scope unless we *explicitly* use the Self qualifier
                    searching_within: self.self_ctx.resolve().parent.unwrap(),
                    for_ref_id: convo_id,
                    public: false,
                };

                self.name_to_ref.insert(nr, convo_id);
                self.waiting_to_resolve.insert(convo_id, cc.clone());

                self.step_resolve(convo_id);
            }
        };*/
        self.earpiece.send(Message {
            to: Destination::resolver(self.node_id),
            from: self.as_dest(),
            send_reply_to: self.as_dest(),
            conversation: Uuid::new_v4(),
            content: Content::NameResolution(NameResolutionMessage::WhatIs {
                composite_symbol: nr,
                given_root: self.node_id.resolve().parent.expect("there wasn't a parent node for a type"),
            }),
        })
    }

    pub async fn entry(mut self, f: &mut crate::ast::types::FunctionDefinition) {
        for (name, tr) in f.parameters.iter_mut() {
            self.start_resolve_typeref(tr);
        }
    }

    /// Convert tree form into serial-evaluated sea of nodes (to be translated down to LLVM later)
    fn build_from(root: ast::types::FunctionDefinition) -> Quark {
        todo!()
    }

    /// Entry point to start the augmented Hindley Milner algorithm from a built quark
    fn do_hm(&mut self) {}

    fn push_frame(&mut self) {
        self.frames.push(self.variables.len())
    }

    fn pop_frame(&mut self) {
        self.variables.truncate(self.frames.pop().unwrap_or(0))
    }

    fn declare_variable(&mut self, named: IStr) -> AllocationReference {
        if let Some((name, alloc)) = self.variables.iter().find(|e| e.0 == named) {
            println!("Shadows a variable named {name}");
        }

        let aref = self.alloc_any();

        self.variables.push((named, aref));

        aref
    }

    fn get_variable(&mut self, named: IStr) -> Option<AllocationReference> {
        self.variables
            .iter()
            .rev()
            .find(|(name, aref)| *name == named)
            .map(|(_, aref)| *aref)
    }

    /// Returns a reference to a blank, new, allocation
    fn alloc_any(&mut self) -> AllocationReference {
        let id = self.allocations.indexed_insert(todo!());
        self.allocations[id].id = id;

        AllocationReference(id)
    }

    /// returns the ID of the root allocation that is referenced
    pub fn allocation_for(&mut self, ar: AllocationReference) -> usize {
        let owned = self.allocation_references.get(&ar.0).unwrap();
        let mut seen = HashSet::new();
        let mut cur = owned;

        loop {
            //let cycle = seen.insert(cur.id);
            match cur.inner {
                AllocationReferenceInner::Direct(d) => {
                    //break self.allocations.get_mut(&d).unwrap()
                    break d;
                }
                AllocationReferenceInner::Indirect(i) => {
                    if seen.contains(&i) {
                        panic!("Detected a reference cycle in allocation set!");
                    } else {
                        seen.insert(i);
                        cur = self.allocation_references.get(&i).unwrap();
                    }
                }
            }
        }
    }

    fn solve_move(&mut self, from: AllocationReference, into: AllocationReference) {}
}

/// An Allocation is a typed memory region
/// that can be referenced, assigned into,
/// modified, and read
/// They should not be semantically reused for different values,
/// and should be treated as pseudo SSA
///
/// They are basically temporary registers
pub struct Allocation {
    id: usize,

    name: Option<IStr>,
    note: Option<IStr>,

    allocation_type: TypeID,

    size: Option<usize>,

    source: Source,
}

pub struct FunctionCall {
    //to: CallableReference, TODO
    args: Vec<AllocationReference>,

    returns: AllocationReference,

    generics: Vec<GenericHandle>,
}

pub struct Construction {
    of: AbstractTypeReference,

    /// Each `field:value` pair provided
    inputs: HashMap<IStr, AllocationReference>,

    returns: AllocationReference,
}

pub struct UnresolvedMethodCall {
    named: IStr,

    args: Vec<AllocationReference>,

    returns: AllocationReference,
}

pub enum Source {
    /// If `!` or an analogue is provided,
    /// this does not have a source since
    /// it is unreachable. The type
    /// of this allocation must be inferred
    /// from downstream rather than top-to-bottom
    Never(),

    /// A Literal can be treated as an F(<builtin>) -> T,
    /// the output is inferred but with (some) constraints
    /// placed on it, such as "if the input parameter is an integer,
    /// the inferred type must be integral in nature".
    /// The input itself does have a type, but is casted
    /// into the output type and the cast is optimized away.
    Literal(),

    /// A Function has a set of inputs and a set of outputs,
    /// with generics linking them together. Through
    /// those parameters, and their constraints,
    /// Functions can help with inference both forward and backward,
    /// and are very helpful specifically *because* they
    /// are never overloaded, and thus provide
    /// information that is as helpful as construction
    /// and literals with single output types.
    Function(Arc<FunctionCall>),

    /// When we do Foo { a: b, c: d },
    /// We perform a Construction
    ///
    /// A Construction is *very* similar to a normal
    /// function, and the types are resolved in much the same way.
    ///
    /// A Constructor does not currently allow dynamic members
    /// to be directly populated, instead those are desugared
    /// into dot assignments after construction
    Construction(Arc<Construction>),

    /// Methods are separate from functions because
    /// the target method is, if looking at all base T,
    /// likely overloaded. Thus, until the
    /// base type can be sufficiently inferred to determine
    /// which method(s) are candidates for the call,
    /// this lookup is blocked.
    ///
    /// Once the method is resolved, it is turned into a Function(FunctionCall),
    /// with the target method as the callable
    MethodUnresolved(Arc<UnresolvedMethodCall>),
    //MethodResolved(Arc<ResolvedMethodCall>),
}

impl Allocation {
    /// Returns an allocation with an uninitialized ID
    /// and no constraints
    pub fn todo() {}
    /*pub fn any(ctx: &TypeContext) -> Self {
        Self {
            id: 0,
            name: None,
            note: None,

            allocation_type: ctx.,
            size: None,
            source: todo!(),
        }
    }*/
}

#[derive(Copy, Clone)]
pub struct AllocationReference(usize);

#[derive(Clone, Copy)]
pub struct OwnedAllocationReference {
    id: usize,

    inner: AllocationReferenceInner,
}

#[derive(Clone)]
pub struct TypeError {
    components: Vec<TypeID>,
    complaint: String,
}

#[derive(Clone, Copy)]
pub enum AllocationReferenceInner {
    /// A direct reference "owns" an
    /// allocation, and is used for binds
    ///
    /// The content of Direct is the id of an Allocation
    Direct(usize),

    /// An indirect reference holds the id
    /// of an AllocationReference, saying
    /// it talks about the same reference
    Indirect(usize),
}

/// The known type of a variable at a point in the code,
/// but symbolically accounting for generics
#[derive(Clone, Debug)]
pub struct SymbolicType {
    /// A vec of symbolic types within the context that are being resolved
    generics: Vec<TypeID>,

    /// The type that this allocation must provide at this usage site
    typeclass: Option<AbstractTypeReference>,
}

#[derive(Clone, Debug)]
pub struct ResolvedType {
    node: CtxID,
    generics: Vec<ResolvedType>,
}

/// No, this has nothing *directly* to do with HKTs,
/// it's just rust doesn't allow anonymous unions
#[derive(Clone, Debug)]
pub enum TypeType {
    /// This type has been unioned with another to
    /// create a new type, that this and the other ID both are the same as
    Refer(TypeID),

    /// A type that refers to a node and has all
    /// of its generics populated with also resolved types
    Resolved(Box<ResolvedType>),

    /// A type with *some* information known,
    /// either the
    Symbolic(Box<SymbolicType>),

    /// If you want to be all academic about it, this
    /// is a "T"
    Unknown(),
}

impl TypeType {}

pub struct Type {
    pub referees: Vec<TypeID>,
    pub current: TypeType,
}

pub struct TypeContext {
    types: AtomicVec<Type>,
}

impl TypeContext {
    pub fn unify(&mut self, a: TypeID, b: TypeID) -> Result<TypeID, TypeError> {
        let TypeID(root_1) = self.root(a);
        let TypeID(root_2) = self.root(b);

        if root_1 == root_2 {
            return Ok(TypeID(root_1)); // these types are already unioned, they have the same root
        }

        // we can safely do a get_two, since the types are not
        // the same

        //let merged = ar.current.union(&br.current);
        let merged = self.unify_simple(TypeID(root_1), TypeID(root_2))?;

        let nt = Type {
            referees: vec![TypeID(root_1), TypeID(root_2)],
            current: merged,
        };

        let (nti, _) = self.types.push(nt);

        //let (ar, br) = self.get_two(root_1, root_2);
        let ar = self.types.get_mut(root_1);
        ar.current = TypeType::Refer(TypeID(nti));

        let br = self.types.get_mut(root_2);
        br.current = TypeType::Refer(TypeID(nti));

        todo!()
    }

    pub fn new_alloc(&mut self) -> Allocation {
        todo!()
    }

    /*
    /// returns, *unordered*, mut refs to two Type vals by ID
    pub fn get_two(&mut self, a: usize, b: usize) -> (&mut Type, &mut Type) {
        let first = a.min(b);
        let second = a.max(b);

        let (vf, vs) = self.types.split_at_mut(second);
        return (vf[first], vs[0]);
    }
    */

    /*
    /// Takes two roots (non Refer types) and creates a new type with
    /// the combination of their information
    pub fn unify_roots(&mut self, a: usize, b: usize) -> Result<TypeType, TypeError> {
        if a == b {
            // already unified, same type
        } else {
            let unified = TypeType::Symbolic(());
            let ty =
            self.types.indexed_insert(unified);
        }
    }*/

    /// In most cases, simple direct substitution is directly possible
    ///
    /// So, we optimize this case, and try to build the narrowing method as
    /// a more expensive, less intuitive, fallback
    ///
    /// This is called with roots, so a and b must be distinct
    pub fn unify_simple(
        &mut self,
        TypeID(ia): TypeID,
        TypeID(ib): TypeID,
    ) -> Result<TypeType, TypeError> {
        let ta = self.types.get(ia);
        let tb = self.types.get(ib);

        match (&ta.current, &tb.current) {
            (TypeType::Unknown(), TypeType::Unknown()) => Ok(TypeType::Unknown()),
            (v, TypeType::Unknown()) | (TypeType::Unknown(), v) => Ok(v.clone()),
            (TypeType::Resolved(a), TypeType::Resolved(b)) => {
                // TODO: check that the two resolved types are the same type, if they've been
                // resolved but are different then the programmer made a typing error
                todo!()
            }
            (TypeType::Resolved(r), TypeType::Symbolic(s))
            | (TypeType::Symbolic(s), TypeType::Resolved(r)) => {
                // TODO: check that the symbolic constraints are compatible with the resolved
                // constraints
                todo!()
            }
            (TypeType::Symbolic(a), TypeType::Symbolic(b)) => {
                // TODO: merge the types, this is the substitution case
                if a.generics.len() != b.generics.len() {
                    Err(TypeError { components: vec![TypeID(ia), TypeID(ib)], complaint: String::from("types could not be unified because the cardinality of their generics was not compatible") })
                } else {
                    let unified_generics: Vec<(TypeID, TypeID)> = a
                        .generics
                        .iter()
                        .zip(b.generics.iter())
                        .map(|(a, b)| (a.clone(), b.clone()))
                        .collect();

                    for (ga, gb) in unified_generics {
                        let unioned = self.unify(ga.clone(), gb.clone());
                    }

                    todo!()
                }
            }
            (_, TypeType::Refer(_)) | (TypeType::Refer(_), _) => {
                unreachable!("values are roots, so can not be refer")
            }
        }
    }

    /// descends this type ID until the type in question
    /// no longer is a Refer(), so a "root" type has been found
    pub fn root(&self, TypeID(mut id): TypeID) -> TypeID {
        while let TypeType::Refer(TypeID(iid)) = self.types.get(id).current {
            id = iid;
        }

        TypeID(id)
    }

    pub fn register_type(&mut self, t: Type) {}
}

impl SymbolicType {
    pub fn unknown() -> Self {
        Self {
            generics: todo!(),
            typeclass: todo!(),
        }
    }

    pub fn union(&self, other: SymbolicType) -> Self {
        todo!()
    }

    /// At the moment, we don't support inheritance
    /// But we do support deref
    ///
    /// So this is actually somewhat complicated, as
    /// a variable of type T can come from a source of type *T or &T,
    /// and that coercion needs to be backpropagated (sort of) here
    ///
    /// What we do is we try to find a candidate base that could satisfy the
    /// other bases after some operation(s), and then try to resolve generics on that
    pub fn try_merge(&self, other: SymbolicType) -> Result<SymbolicType, CompilationError> {
        /*let mut bases: Vec<_> = other
            .facts
            .iter()
            .chain(self.facts.iter())
            .filter_map(|e| {
                if let Fact::Base(b) = e {
                    Some(b.clone())
                } else {
                    None
                }
            })
            .collect();

        for b in bases {}*/

        todo!()
    }
}

/*#[derive(PartialEq, Eq, Hash, Clone, Copy)]
enum TypeReferenceBacking {
    Real(AbsoluteTypeReference),
    Indirect(IndirectTypeReference),
}*/

/// An initialization is a branch point for type inference
///
/// When we construct, it takes a set of literals or other expressions
/// and ties them all into the type of a new object
pub struct Initialization {
    is_type: SymbolicType,

    values: Vec<(IStr, Value)>,
}

pub enum Value {
    /// Used when we don't have a literal,
    /// and we aren't composing a nested initialization
    Expression(AllocationReference),

    Initialization(Initialization),

    /// A list of bytes
    Literal(Vec<u8>),

    /// If a value is not provided we can simply zero the field
    /// Shouldn't actually expose syntactically, but used under the hood
    Zeroed(),

    /// Really shouldn't be used, and should almost never be exposed
    /// to the programmer except with intrinsics with a lot of warnings
    Uninit(),

    /// later use, can use for partial initialization similar to rust { ..., ..val } syntax
    Expanded(),
}
