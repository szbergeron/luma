use crate::helper::interner::IStr;

use super::tree::CtxID;

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
struct Quark {
    //
}

struct SSA {
    is_a: CtxID,
    with: Vec<Type>,

    operations: Vec<Operation>,
}

struct Operation {
    id: usize,
    inner: OperationInner,
}

impl Operation {
    pub fn generate_label(&self) -> IStr {
        todo!()
    }
}

enum OperationInner {
    Guard(GuardOperation),
    Call(CallOperation),
    Jump(JumpOperation),
    Return(ReturnOperation),
    Branch(BranchOperation),
    Bind(BindOperation),
}

struct OperationReference {
    id: usize,
    index: Option<usize>,
}

struct BindOperation {
    from: OperationReference,

    has_type: Type,

    named: Option<IStr>,
}

/// The known type of a variable at a point in the code
struct Type {
    /// The known interfaces that exist on this type
    /// that the programmer can actually use without having to guard
    interfaces: Vec<InterfaceValue>,

    /// The actual underlying base "value type", which
    /// could even be Unit
    value_type: TypeValue,
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
    /// A type that we don't
    /// know the size of that is held behind a
    /// container such as & or Box, but
    /// that we may otherwise know the interfaces on
    UnknownReference(),
    UnknownValue{ size_bytes: usize },
    Node()
}
