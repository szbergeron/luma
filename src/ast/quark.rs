use std::{sync::atomic::{AtomicUsize, Ordering}, collections::{HashMap, HashSet}};

use smallvec::SmallVec;

use crate::helper::{interner::IStr, VecOps};

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
    type_args: Vec<Type>,

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
        self.variables.iter().rev().find(|(name, aref)| *name == named).map(|(_, aref)| *aref)
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
                    break d
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

    fn solve_move(&mut self, from: AllocationReference, into: AllocationReference) {
    }

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
                Call(c) => {
                }

                // bindings can contain type constraints
                // that can (up to) fully solve the
                // type of a call or allocation
                Bind(b) => {
                    //
                    let BindOperation { allocation_type, named, allocation } = b;

                    let allocation_id = self.allocation_for(b.allocation);

                    let allocation = self.allocations.get_mut(allocation_id).unwrap();

                    allocation.constrain(b.allocation_type, operation.id);
                }
                Reference(r) => {
                }
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

    pub fn push_llvm(&self, next: IStr) {
    }

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
    Implement(ImplementOperation),
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

    allocation_type: Type,

    size: Option<usize>,

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

            allocation_type: Type::unknown(),
            size: None,

            moves_into: Vec::new(),
            moves_from: Vec::new(),
        }
    }
}

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
pub struct NoOperation {
}

/// A neverop should never be reachable from
/// anywhere in the code (should not be a connected
/// leaf in any CFG).
pub struct NeverOperation {
}

/// Any node connected to an ExitOperation
/// must provide a result value,
/// and that result value forms an exit from the
/// CFG. This node appears
/// at the end of functions only, and forms
/// the basis for implicit return
pub struct ExitOperation {
}

pub struct CallOperation {
    base: OperationReference,

    named: IStr,

    type_args: Vec<Type>,

    result_type: Option<Type>,

    resolved_target: Option<CtxID>,
}

pub struct ReturnOperation {
    value: Type,
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

pub struct ImplementOperation {
}

pub struct GuardOperation {
    /// The guard target
    check: OperationReference,

    /// GuardOperation checks that the value in `check` is an `is`
    is: Type,

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
    allocation_type: Type,

    named: Option<IStr>,

    allocation: AllocationReference,

    //level: usize,
}

/// Any bindings at a level equal to or greater than `level`
/// expire here
pub struct UnbindOperation {
    level: usize,
}

/// The known type of a variable at a point in the code
struct Type {
    /// The known interfaces that exist on this type
    /// that the programmer can actually use without having to guard
    interfaces: Vec<InterfaceValue>,

    /// The actual underlying base "value type", which
    /// could even be Unit
    value_type: TypeValue,

    /// When we construct a Type,
    /// we give it some TypeConstraints
    /// that can be used to help solve an actual type
    /// until either a solution type emerges,
    /// or an impossibly satisfied set of constraints
    /// emerges, or we reach limits to how hard we're
    /// allowed to try to resolve a given type
    bounds: InstanceConstraint,
}

impl Type {
    pub fn unknown() -> Self {
        Self {
            interfaces: Vec::new(),
            value_type: TypeValue::Unknown(),
            bounds: InstanceConstraint::unconstrained(),
        }
    }
}

struct InterfaceValue {
    /// is an instance of the Node referred to by the CtxID
    instance_of: CtxID,

    ///
    with_generics: Vec<Type>,

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
    UnknownValue{ size_bytes: usize },

    Node()
}
