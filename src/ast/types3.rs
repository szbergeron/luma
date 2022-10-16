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
    //generic_args: Vec<Tag>,

    value: Option<Literal>,

    //terminal: CtxID,

    id: usize,
}

/// A held type is different from a Node in the sense that
/// it is the *static type of a particular variable*.
///
/// It includes both the primary held type (the one implicitly
/// rooted for dynamic member selection) as well as the
/// outer type chain that can also be referred to
pub struct HeldType {
    //
}

pub struct FieldMember {
    named: IStr,
    typed: CtxID,
    initialization: Option<Initialization>,
}

// An ESpan roughly corresponds to an expression, except it *does* know things like where
// a variable came from, and allows doing incremental deduction of types
//
// I don't want to have to make an SAT solver so here we are
pub struct ESpan {
    depends_on: Vec<ExpressionID>,
    depends_for: Vec<ExpressionID>,
}
