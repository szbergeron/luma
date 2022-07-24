use std::collections::HashMap;

use super::{types::TypeConstraint, expressions::AnyExpression};


/// A binding has no name, it is only ever aliased as a variable
///
/// Multiple variables can theoretically have the same binding
struct Binding {
    has_type: TypeConstraint,
    mutable: bool,

    initialization: AnyExpression,
}

/// Lays out a set of bindings that must be evaluated in order
struct Sequential {
}

/// Basically a phi node with N sources
struct Combine {
}

/// Events happen "in serial" semantically.
///
/// If they can be shown to be possible to reorder without visible side effects,
/// then they may be--or they may be run in parallel in very specific cases
enum Event {
    Call(FunctionInstanceID, Vec<Binding>),
}

/// Conceptually similar to a monomorphization,
/// except it happens implicitly and is simultaneously more
/// complicated and less helpful
struct FunctionInstance {
    //
}

struct FunctionTemplate {
    //
}

struct Quark {
    functions: Vec<FunctionTemplate>,

    instances: HashMap<usize, FunctionInstance>,

    //
}
