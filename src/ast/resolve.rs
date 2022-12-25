use crate::cst::GenericHandle;

/// only makes sense when looking at InstanceSet
struct Single {
    of: Option<NodeReference>,
    generics: Vec<(GenericHandle, Option<Instance>)>,
}

/// An Instance describes the type of a variable
///
/// An Instance has Singles which are components
/// that contribute to resolving its type.
/// Each Single puts some constraint
/// on an Instance, 
struct Instance {
    constraints: Vec<Single>,
}
