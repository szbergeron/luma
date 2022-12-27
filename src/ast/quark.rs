use std::{
    collections::{HashMap, HashSet},
    sync::atomic::{AtomicUsize, Ordering},
};

use smallvec::SmallVec;

use crate::{helper::{interner::IStr, VecOps, Either}, cst::GenericHandle};

use super::{tree::CtxID, types::InstanceConstraint};

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

    value: Linear,

    //allocations: HashMap<usize, Allocation>,
    allocations: Vec<Allocation>,

    allocation_references: HashMap<usize, OwnedAllocationReference>,

    variables: Vec<(IStr, AllocationReference)>,
    frames: Vec<usize>,
}

impl Quark {
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

    pub fn typecheck(&mut self) {
        let variables: Vec<(IStr, AllocationReference)>;

        let live_bindings = Vec::new(); // the set of bindings that are live in any way
                                        // local to the scope
        let bind_levels = Vec::new(); // Whenever we enter a new bind level, we push
                                      // the length of `live_bindings` into bind_levels.
                                      // When we execute an unbind, anything from the
                                      // current level is discarded by truncating live_bindings

        // A variable is different from a binding
        // in that variables can be bound multiple times
        //
        // Think of bindings as effectively an alias
        // that may or may not have a name. If something
        // is the last expression in a block, and the value
        // of a block is given a binding, then that
        // last expression in the block is actually
        // the same exact variable that the binding of the block
        // value refers to. Every variable reference (by ident)
        // to a given variable is actually a binding to that
        // variable itself, and if we do `let v = 5`,
        // first the literal 5 is basically "turned into a variable"
        // in that it gets pushed to stack/register and then v
        // aliases that variable
        //
        // (that isn't exactly true, technically the variable
        // behind v is created for the assign operation and then
        // 5 is put into that)
        //
        // This is how we can have `let v;` syntax.
        // We do check that a variable is assigned
        // before use by reachability checks, but
        // those accesses occur on the same variable
        // that v aliases so we can do inference on
        // the type of the variable!
        let variables = Vec::new();

        for operation in self.value.operations {
            use OperationInner::*;
            match operation.inner {
                // probably the most interesting branch, lets us do type
                // joining
                Move(m) => {
                    let MoveOperation { from, into } = m;

                    self.solve_move(from, into);
                }

                // calls create allocations as their result,
                // can be used if a call is identified to
                // perform inference
                Call(c) => {}

                // bindings can contain type constraints
                // that can (up to) fully solve the
                // type of a call or allocation
                Bind(b) => {
                    //
                    let BindOperation {
                        allocation_type,
                        named,
                        allocation,
                    } = b;

                    let allocation_id = self.allocation_for(b.allocation);

                    let allocation = self.allocations.get_mut(allocation_id).unwrap();

                    allocation.constrain(b.allocation_type, operation.id);
                }
                Reference(r) => {}
            }
        }

        loop {
            //
        }
    }
}

pub struct Variable {
    id: usize,
    writes: Vec<OperationReference>,
    reads: Vec<OperationReference>,
}

/// A Linear roughly corresponds to a function or method
///
/// There are a list of inputs and their associated contracted
/// types, as well as an output expected type
pub struct Linear {
    //is_a: CtxID,
    //with: Vec<Type>,
    operations: Vec<Operation>,

    /// This is constructed as a vecmap
    /// since we modify it (splicing)
    /// much more often than we need to read it in practice
    /// (and during real checking we'll construct
    /// a proper map after a threshold) I'd expect
    /// a "proper" map to be much slower for the
    /// almost always <100 element (often only 10)
    /// slices that this will contain. The
    /// values here are also so *dumb simple* (pairs of
    /// two usize) that this will branch
    /// predict almost perfectly up until
    /// the actual result, and will vectorize
    /// incredibly well
    ///
    /// The value at each index is the id
    /// of the operation at that index
    refs: Vec<usize>,

    inputs: Vec<(IStr, Type)>,

    output: Type,
}

impl Linear {
    pub fn insert(&mut self, operation: Operation) {
        self.refs.push(operation.id);
        self.operations.push(operation);
    }
    /*pub fn append(&mut self, other: Linear) {
        let prior = self.operations.len();

        self.operations.append(&mut other.operations);
        self.refs.append(&mut other.refs);


    }*/
    /*pub fn from_expression(expression: super::expressions::AnyExpression) -> Self {
    }*/

    /*
     * I could make this work but I'm
     * gonna come back later when I actually
     * have things working to consider optimizing lowering these
     *
    /// position says which element to insert before.
    /// If position is >= 0, it is the 0 indexed offset
    /// from the start of the operation vec at which to insert.
    ///
    /// If position is < 0, it is the offset from the end
    /// at which to insert the given segment with -1
    /// translating to appending the span to the very
    /// end of the list
    pub fn insert(&mut self, other: Linear, position: isize) {
        let position = if position > 0 {
            position
        } else {
            self.operations.len() as isize + position - 1
        };

        assert!(position <= self.operations.len() as isize);
        assert!(self.operations.len() == self.refs.len());
        assert!(other.operations.len() == other.refs.len());

        self.operations.reserve(other.operations.len());
        self.refs.reserve(other.operations.len());

        let (optr, osize, ocapacity) = self.operations.into_raw_parts();
        let (rptr, rsize, rcapacity) = self.refs.into_raw_parts();

        unsafe {
            // first shift down the old elements

            let offset = optr.offset(position);

            std::ptr::copy
        }

        //self.operations.extend_one
        todo!()
    }

    pub fn append(&mut self, other: Linear) {
        self.insert(other, -1)
    }*/
}

pub struct Operation {
    id: usize,

    /// a list of IDs of operations that can flow into this one
    comes_from: SmallVec<[usize; 2]>,

    /// any operation has a note that can be added
    /// that will be
    /// emitted with the node
    /// during encode and is visible during debugging
    note: IStr,
    inner: OperationInner,
}

impl Operation {
    /// Operations translate directly to syntactic LLVM basic blocks,
    /// though they don't really translate to logical basic blocks.
    /// Each one has a label, but they basically
    /// implicitly "fall through" to the next block
    pub fn generate_label(&self) -> IStr {
        todo!()
    }

    pub fn push_llvm(&self, next: IStr) {}

    pub fn call(&self, back_to: IStr) -> OperationResult {
        todo!()
    }

    pub fn new(inner: OperationInner, note: IStr) -> Self {
        static ID: AtomicUsize = AtomicUsize::new(1);

        Self {
            id: ID.fetch_add(1, Ordering::Relaxed),
            note,
            inner,
            comes_from: Default::default(),
        }
    }
}

pub enum OperationInner {
    Guard(GuardOperation),
    Modify(ModifyOperation),
    Call(CallOperation),
    Jump(JumpOperation),
    Return(ReturnOperation),
    Branch(BranchOperation),
    Bind(BindOperation),
    //Implement(ImplementOperation),
    Reference(VariableReferenceOperation),

    Noop(NoOperation),
    Never(NeverOperation),

    Move(MoveOperation),

    /// This could be modeled as a call, but
    /// it is significant enough to our
    /// resolution algorithm that it deserves
    /// a separate representation
    //Assign(AssignOperation),

    /// When a scope starts (so bindings inside of it should be limited by a corresponding scope
    /// end)
    BeginScope(),

    /// See BeginScope
    EndScope(),
}

pub struct OperationReference {
    id: usize,
    index: Option<usize>,
}



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

    allocation_type: SymbolicType,

    size: Option<usize>,

    /// When we create an allocation
    /// that isn't moved directly into, we use an Initialization.
    ///
    /// This is effectively a fully resolved type
    /// that comes from a struct or value literal
    constructed_from: Option<Initialization>,

    moves_into: Vec<AllocationReference>,
    moves_from: Vec<AllocationReference>,
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

            moves_into: Vec::new(),
            moves_from: Vec::new(),
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

pub enum OperationResult {
    /// If this block could not be resolved correctly,
    /// then Error() is returned. Error() substitutes for every other
    /// variant, and TODO: figure out if we can still compile
    /// even with type errors and simply panic
    Error(),

    /// This block yields no result, and
    /// purely produces side effects
    Empty(),

    /// This block yields a unified result
    /// in a single value (not destructured, can still be composite)
    Single(IStr),
}

pub struct MoveOperation {
    from: AllocationReference,
    into: AllocationReference,
}

pub struct AssignOperation {
    /// Need to change this into pattern syntax at
    /// some point for destructuring,
    /// for now just make it a variable or reference
    into: OperationReference,

    value: OperationReference,
}

/// Does nothing, but falls through
/// to the next operation just like
/// other (non-return) operations
///
/// Used for lowering loops
pub struct NoOperation {}

/// A neverop should never be reachable from
/// anywhere in the code (should not be a connected
/// leaf in any CFG).
pub struct NeverOperation {}

/// Any node connected to an ExitOperation
/// must provide a result value,
/// and that result value forms an exit from the
/// CFG. This node appears
/// at the end of functions only, and forms
/// the basis for implicit return
pub struct ExitOperation {}

pub struct CallOperation {
    base: OperationReference,

    named: IStr,

    type_args: Vec<SymbolicType>,

    result_type: Option<SymbolicType>,

    resolved_target: Option<CtxID>,
}

pub struct ReturnOperation {
    value: SymbolicType,
}

pub struct JumpOperation {
    to: OperationReference,
}

pub struct ModifyOperation {
    /// The target of a Modify,
    /// the value in `from` is added as an implementation
    /// to this object
    into: OperationReference,

    /// Must yield an expression of Implemetation
    /// type
    from: OperationReference,
}

pub struct ImplementOperation {}

pub struct GuardOperation {
    /// The guard target
    check: OperationReference,

    /// GuardOperation checks that the value in `check` is an `is`
    is: SymbolicType,

    then: OperationReference,

    otherwise: Option<OperationReference>,

    /// Not yet useful, will allow statically
    /// ensuring that a type propagates for a given usage for any possible path
    ///
    /// Basically allows for a guard inside an `if true` or on the basic path
    /// to force itself to pass, so if we're a closure called
    /// inside of a function that isn't offering generics but we can know that
    /// we're passing in a given type, that intermediate function can be monomorphised to
    /// pass additional information and not require a recheck of the type
    require_resolvable: bool,
}

pub struct BranchOperation {
    /// Use `eval` as the descriminant for the branch,
    /// it should either produce a bool (an i1) or
    /// a larger integer type (that may have more data from guards)
    /// that allows a "switch"
    eval: OperationReference,

    /// Take the result of `eval` as an integral
    /// type aliased `n` and branch to the `n`th
    /// entry in `targets` before converging at the end of
    /// this operation
    ///
    /// All entries in `targets` should
    /// be possible to narrow to a single type,
    /// with additional checks possible to warn
    /// about very loose divergences
    targets: SmallVec<[OperationReference; 2]>,
}

/// A name is optional here as
/// binding can occur during destructuring
/// but is different from
/// a destructure operation.
///
/// Think if someone does `let (a, b): (A, B);`,
/// the bind is done to an allocation of type (A, B)
/// even though that bind itself is unnamed
pub struct BindOperation {
    allocation_type: SymbolicType,

    named: Option<IStr>,

    allocation: AllocationReference,
    //level: usize,
}

/// Any bindings at a level equal to or greater than `level`
/// expire here
pub struct UnbindOperation {
    level: usize,
}

enum Type {
    Pointer(Box<Type>),
    Reference(Box<Type>),
    Generic(GenericConstraint),
    Value(SymbolicType),
    Dynamic(SymbolicType),
}

enum Fact {
    /// The index of the generic, as well as the type that we have info on it for
    Generic(usize, SymbolicType),

    /// A possible base type for this
    Base(SymbolicBase),
}

#[derive(Clone)]
enum SymbolicBase {
    Value(NodeID),
}

/// The known type of a variable at a point in the code,
/// but symbolically accounting for generics
struct SymbolicType {
    /// The actual underlying base "value type", which
    /// could even be Unit
    //value_type: TypeValue,

    //source_roots: Vec<>
    //source_roots: Vec<NodeID>,

    //generics: Vec<(GenericHandle, Vec<Either<>>)>,

    facts: Vec<Fact>,

    resolved: Option<ResolvedType>,

    /// When we construct a Type,
    /// we give it some TypeConstraints
    /// that can be used to help solve an actual type
    /// until either a solution type emerges,
    /// or an impossibly satisfied set of constraints
    /// emerges, or we reach limits to how hard we're
    /// allowed to try to resolve a given type
    bounds: InstanceConstraint,
}

impl SymbolicType {
    pub fn unknown() -> Self {
        Self {
            interfaces: Vec::new(),
            value_type: TypeValue::Unknown(),
            bounds: InstanceConstraint::unconstrained(),
        }
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
        let mut bases: Vec<_> = other.facts.iter().chain(self.facts.iter()).filter_map(|e| if let Fact::Base(b) = e { Some(b.clone()) } else { None }).collect();

        for b in bases {
        }
    }
}

struct InterfaceValue {
    /// is an instance of the Node referred to by the CtxID
    instance_of: CtxID,

    ///
    with_generics: Vec<SymbolicType>,

    /// If this interface was guarded against "fast",
    /// then the involved pointer is cached within
    /// a fat pointer to the actual value itself
    /// or as an aside to a value type
    ///
    /// It is double held because even for UnknownValue
    /// value types, we can allow mem2reg to
    /// better optimize the call, maybe
    /// even devirtualize/inline it if
    /// other constraints are met
    in_fat_ptr: bool,
}

enum TypeValue {
    /// A type that is (as of yet) completely
    /// unresolved, we don't even know if
    /// it's a reference type or not
    Unknown(),

    /// A type that we don't
    /// know the size of that is held behind a
    /// container such as & or Box, but
    /// that we may otherwise know the interfaces on
    UnknownReference(),

    /// A type with the given size in bytes
    /// even if we don't know what specific
    /// value type it is (sized constraints or an identity relationship)
    UnknownValue {
        size_bytes: usize,
    },

    Node(CtxID),
}

struct TypeConstraint {
    id: usize,

    assigns_into: Vec<AllocationReference>,

    assigns_from: Vec<AllocationReference>,

    mentions: Vec<CompleteMention>,

    offers: Vec<Offer>,

    subtype_of: Vec<TypeConstraintReference>,

    supertype_of: Vec<TypeConstraintReference>,

    callable: Vec<CallableSpecification>,
    //same_as: Vec<TypeConstraintReference>,

    //generics: Vec<GenericConstraint>,
}

/// An Offer is best thought of as a
/// single capability of an object,
/// as in it offers the `f()` operation,
/// or offers a field named `g`
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
struct Offer {
    named: IStr,

    /// If this is a callable, this is Some(_)
    /// even for () -> ? functions, and
    /// is only None if not a callable or is not called
    typed: TypeConstraintReference,
}

#[derive(PartialEq, Eq, Hash, Clone)]
struct CallableSpecification {
    arguments: Vec<TypeConstraintReference>,

    returns: TypeConstraintReference,
}

/// A Mention is basically a scoped name reference
#[derive(PartialEq, Eq, Hash, Clone)]
struct CompleteMention {
    elements: Vec<SingleMention>,
}

#[derive(PartialEq, Eq, Hash, Clone)]
struct SingleMention {
    named: IStr,
    generics: GenericConstraint,
}

#[derive(PartialEq, Eq, Hash, Clone)]
struct GenericConstraint {
    elements: Vec<Option<TypeConstraintReference>>,
}

struct TypeContext {
    indirects: Vec<AbsoluteTypeReference>,
    reals: Vec<TypeConstraint>,
}

/*#[derive(PartialEq, Eq, Hash, Clone, Copy)]
enum TypeReferenceBacking {
    Real(AbsoluteTypeReference),
    Indirect(IndirectTypeReference),
}*/

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
struct AbsoluteTypeReference(usize);

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
struct IndirectTypeReference(usize);

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
struct TypeConstraintReference(IndirectTypeReference);

impl TypeContext {
    /// When we know two types must be the same (it is a single
    /// variable within a segment), then we can declare they must be the same type
    pub fn same(
        &mut self,
        a: TypeConstraintReference,
        b: TypeConstraintReference,
    ) -> TypeConstraintReference {
        // combine and change their reference pointers
        let ta = self.entry(a);
        let tb = self.entry(b);

        let id = self.reals.len();

        let new = TypeConstraint {
            assigns_into: ta.assigns_into.clone().merged(tb.assigns_into.clone()),
            assigns_from: ta.assigns_from.clone().merged(tb.assigns_from.clone()),
            mentions: ta.mentions.clone().merged(tb.mentions.clone()),
            offers: ta.offers.clone().merged(tb.offers.clone()),
            subtype_of: ta.subtype_of.clone().merged(tb.subtype_of.clone()),
            supertype_of: ta.supertype_of.clone().merged(tb.supertype_of.clone()),
            callable: ta.callable.merged(tb.callable.clone()),
            id,
        };

        let real_id = self.reals.len();
        let indirect_id = self.indirects.len();

        self.reals.push(new);

        self.indirects
            .push(AbsoluteTypeReference(real_id));

        TypeConstraintReference(IndirectTypeReference(indirect_id))
    }

    /// When a type "offers" some capability, we add it here
    pub fn offers(&mut self, a: TypeConstraintReference, offers: Offer) -> TypeConstraintReference {
        todo!()
    }

    /// Says that a must be a subtype or sametype as b
    pub fn subtype(
        &mut self,
        a: TypeConstraintReference,
        b: TypeConstraintReference,
    ) -> (TypeConstraintReference, TypeConstraintReference) {
        //self.entry_mut(a).subtype_of.push(self.indirect(self.chase(b)));

        todo!()
    }

    pub fn callable(
        &mut self,
        a: TypeConstraintReference,
        call: CallableSpecification,
    ) -> TypeConstraintReference {
        self.entry_mut(a).callable.push(call);

        todo!("pump solve here")
    }

    fn entry_mut(&mut self, r: TypeConstraintReference) -> &mut TypeConstraint {
        let absolute = self.chase(r);

        let val = self
            .reals
            .get_mut(absolute.0)
            .expect("malformed absolute ref from chase");

        val
    }

    fn entry(&mut self, r: TypeConstraintReference) -> &TypeConstraint {
        self.entry_mut(r)
    }

    fn chase(&mut self, mut r: TypeConstraintReference) -> &mut AbsoluteTypeReference {
        let mut seen = HashSet::new();

        while let None = seen.get(&r.0) {
            seen.insert(r.0);

            let trb = self
                .indirects
                .get_mut(r.0)
                .expect("chase was given an invalid or orphan type constraint reference");

            match trb {
                TypeReferenceBacking::Real(r) => return r,
                TypeReferenceBacking::Indirect(i) => {
                    r = *i;
                }
            }
        }

        panic!("BUG: type constraints are mutually indirect")
    }
}
