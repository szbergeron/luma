pub struct StructType {
}

pub enum ValueType {
}


/// A type is something that has default values for related methods,
/// can contain data inline
pub struct TypeOld {
}

/// A type is just a schema for data, a constraint for how it can be represented
/// Types have fields and methods, all under the same umbrella, but relocatable
/// when actually lowering
pub struct Type {
    tag: Tag
}

/// A Tag uniquely describes a specific type.
/// It is used to know which slot to insert into,
/// it has a scalar reduction that can be used to
/// efficiently search for it at runtime.
///
/// A tag can include the path to a type, along with the
/// generic arguments involved in that.
///
/// An import of a type with certain generic arguments
/// is given a tag even if it isn't directly used,
/// and this all allows default implementations for types as well
pub struct Tag {
    generic_args: Vec<Tag>,

    value: Option<Literal>,

    terminal: CtxID,

    id: usize,
}
