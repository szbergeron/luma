use std::{
    collections::{HashMap, HashSet},
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

use smallvec::SmallVec;

use crate::{
    cst::GenericHandle,
    helper::{interner::IStr, Either, VecOps, CompilationError},
};

use super::{
    tree::{CtxID, NodeReference},
    types::{InstanceConstraint, FunctionDefinition},
};

struct TypeID(usize);

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
}

impl Quark {
    /// Convert tree form into serial-evaluated sea of nodes (to be translated down to LLVM later)
    fn build_from(root: FunctionDefinition) -> Quark {}

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
        let id = self.allocations.indexed_insert(Allocation::any());
        self.allocations[id].id = id;

        AllocationReference(id)
    }

    /// returns the ID of the root allocation that is referenced
    pub fn allocation_for(&mut self, ar: AllocationReference) -> usize {
        let owned = self.allocation_references.get(&ar.0).unwrap();
        let seen = HashSet::new();
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

struct FunctionCall {
    to: CallableReference,

    args: Vec<AllocationReference>,

    returns: AllocationReference,

    generics: Vec<GenericHandle>,
}

struct Construction {
    of: NodeReference,

    /// Each `field:value` pair provided
    inputs: HashMap<IStr, AllocationReference>,

    returns: AllocationReference,
}

struct UnresolvedMethodCall {
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
    pub fn any() -> Self {
        Self {
            id: 0,
            name: None,
            note: None,

            allocation_type: SymbolicType::unknown(),
            size: None,
            source: todo!(),
        }
    }
}

#[derive(Copy, Clone)]
struct AllocationReference(usize);

#[derive(Clone, Copy)]
struct OwnedAllocationReference {
    id: usize,

    inner: AllocationReferenceInner,
}

#[derive(Clone)]
pub struct TypeError {
    components: Vec<usize>,
    complaint: String,
}

#[derive(Clone, Copy)]
enum AllocationReferenceInner {
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
struct SymbolicType {
    /// A vec of symbolic types within the context that are being resolved
    generics: Vec<usize>,

    /// The type that this allocation must provide at this usage site
    typeclass: Option<NodeReference>,
}

struct ResolvedType {
    node: NodeReference,
    generics: Vec<ResolvedType>,
}

/// No, this has nothing *directly* to do with HKTs,
/// it's just rust doesn't allow anonymous unions
pub enum TypeType {
    /// This type has been unioned with another to
    /// create a new type, that this and the other ID both are the same as
    Refer(usize),

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
    pub referees: Vec<usize>,
    pub current: TypeType,
}

pub struct TypeContext {
    types: Vec<(usize, Type)>,
}

impl TypeContext {
    pub fn unify(&mut self, a: usize, b: usize) -> usize {
        let root_1 = self.root(a);
        let root_2 = self.root(b);

        if root_1 == root_2 {
            return; // these types are already unioned, they have the same root
        }

        // we can safely do a get_two, since the types are not
        // the same

        //let merged = ar.current.union(&br.current);
        let merged = self.unify_simple(root_1, root_2);

        let nt = Type {
            referees: vec![root_1, root_2],
            current: merged,
        };

        let nti = self.types.indexed_insert(nt);

        let (ar, br) = self.get_two(root_1, root_2);
        ar.current = TypeType::Refer(nti);
        br.current = TypeType::Refer(nti);
    }

    /// returns, *unordered*, mut refs to two Type vals by ID
    pub fn get_two(&mut self, a: usize, b: usize) -> (&mut Type, &mut Type) {
        let first = a.min(b);
        let second = a.max(b);

        let (vf, vs) = self.types.split_at_mut(second);
        return (vf[first], vs[0]);
    }

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
    pub fn unify_simple(&mut self, ia: usize, ib: usize) -> Result<TypeType, TypeError> {
        let ta: &Type = self.types[ia];
        let tb: &Type = self.types[ib];

        match (ta.current, tb.current) {
            (TypeType::Unknown(), TypeType::Unknown()) => Ok(TypeType::Unknown()),
            (v, TypeType::Unknown()) | (TypeType::Unknown(), v) => Ok(v),
            (TypeType::Resolved(a), TypeType::Resolved(b)) => {
                // TODO: check that the two resolved types are the same type, if they've been
                // resolved but are different then the programmer made a typing error
            }
            (TypeType::Resolved(r), TypeType::Symbolic(s))
            | (TypeType::Symbolic(s), TypeType::Resolved(r)) => {
                // TODO: check that the symbolic constraints are compatible with the resolved
                // constraints
            }
            (TypeType::Symbolic(a), TypeType::Symbolic(b)) => {
                // TODO: merge the types, this is the substitution case
                if a.generics.len() != b.generics.len() {
                    Err(TypeError { components: vec![ia, ib], complaint: String::from("types could not be unified because the cardinality of their generics was not compatible") })
                } else {
                    let unified_generics = a
                        .generics
                        .iter()
                        .zip(b.generics.iter())
                        .map(|(ga, gb)| {
                            let unioned = self.union(a, b);

                            unioned
                        })
                        .collect();
                }
            }
            (_, TypeType::Refer(_)) | (TypeType::Refer(_), _) => {
                unreachable!("values are roots, so can not be refer")
            }
        }
    }

    /// descends this type ID until the type in question
    /// no longer is a Refer(), so a "root" type has been found
    pub fn root(&self, mut id: usize) -> usize {
        while let TypeType::Refer(iid) = self.types[id].current {
            id = iid;
        }

        id
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

    pub fn union(&self, other: SymbolicType) -> Self {}

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
        let mut bases: Vec<_> = other
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

        for b in bases {}
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
