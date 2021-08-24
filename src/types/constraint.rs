/// Refers to a specific, resolved, *specialized* type.
/// This could be a value type, or could itself be a trait.
///
/// In the context of an impl guard, this
/// is used for constraining what it will actually be applied to.
/// Although what it stores is very similar to a TraitDescription,
/// it is used not as an "expanding" element but instead actually provides the guarantees
/// that are to be provided within that context
pub struct TypeDescription {
}

/// Refers to a specific, resolved, *specialized* trait.
///
/// This is a trait with the generics fully specified so that any matching
/// specializations for this type can also be pulled in
pub struct TraitDescription {
}
