use std::{sync::atomic::{AtomicUsize, Ordering}, collections::HashMap};

use smallvec::SmallVec;

use crate::{helper::interner::IStr, types::CtxID};

/// Generics notes:
///
/// An implementation is applied to an object at construction
/// iff there exists an import or definition of the trait
/// for that implementation and either the implementation
/// of that trait for the stated object exists within the
/// module where the trait is sourced from or such an
/// implementation exists within the current module

/// A TypeRoot describes ...
enum TypeRootInner {
    Value(ValueType),
    Virtual(VirtualType),

    /// Modules are still types, but they have no methods or "fields",
    /// They only have related types and functions--no vtables
    ///
    /// This can also be used for "non instantiable" types on their own,
    /// such as the never type. They can exist as return types,
    /// but have no valid instance value and can not be represented
    /// as any sequence of bits or be cast to from any other type
    Uninstantiable(),
    //Specialized(SpecializedType),
    //Generic(GenericHandle),
}

struct TypeRoot {
    generic_params: Vec<GenericHandle>,

    inner: TypeRootInner,

    space: TypeSpace,
}

/// A TypeSpace basically holds the "module like" properties
/// of a type, such as associated types and associated functions
struct TypeSpace {
    members: Vec<Symbol>
}

struct VirtualType {
    fields: Vec<FieldMember>,

    methods: Vec<!>,
}

struct ValueType {
    name: IStr,
    
    core: ValueTypeCore,
}

enum ValueTypeCore {
    Struct(StructType),
    Enum(!),
}

struct StructType {
    fields: Vec<FieldMember>,
}

/// A Module describes the set of "related objects" within 
struct Module {
}

enum Symbol {
    Type(TypeRoot),
    Function(Function),
}

trait AsModLike {
    fn as_mod_like(&self) -> &dyn ModLike;
}

impl<T> ModLike for T where T: AsModLike {
    fn name(&self) -> IStr {
        self.as_mod_like().name()
    }

    fn generics(&self) -> Vec<(IStr, TypeConstraint)> {
        self.as_mod_like().generics()
    }

    fn children(&self) -> Vec<(IStr, &Symbol)> {
        self.as_mod_like().children()
    }
}

trait ModLike {
    /// Returns the "symbol name" of a given TypeLike
    ///
    /// For example, `struct Foo {}` in source should yield
    /// a value from name of `Foo`
    fn name(&self) -> IStr;

    fn generics(&self) -> Vec<(IStr, TypeConstraint)>;

    /// For a struct, children is all fields and methods and related functions
    ///
    /// For a module, children is all types and functions including re-exports
    fn children(&self) -> Vec<(IStr, &Symbol)>;
}

/// The "T" in Something<T>
///
/// A GenericHandle aims to be a key type
/// that can map down to an actual type during
/// usage and implementation solving
///
/// If a handle is reused, then simply clone the existing one
/// that you wish to refer to (don't use new!)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct GenericHandle {
    name: IStr,
    id: usize,
}

impl GenericHandle {
    pub fn new(name: IStr) -> GenericHandle {
        static id: AtomicUsize = AtomicUsize::new(1);

        GenericHandle { name, id: id.fetch_add(1, Ordering::SeqCst)}
    }
}

/// Represents a fully resolved type, with all generics filled in
/// (or at least a required subset of them)
struct TypeInstiation {
    base_type: TypeRoot,
    generic_args: HashMap<GenericHandle, TypeInstiation>,
    //generic_arguments: Vec<(TypeInstance, GenericHandle)>,
}

/// An Instance represents not an actual object but more a "type of thing",
/// where the "thing" could be some object that we're calling a method on, but also could be just a
/// type or an interface that we're calling an associated function on. An "Instance" is something
/// that can be solved for, and is effectively an IR representation that backs a TypeReference.
struct Instance {
    narrowings: TypeConstraint,
}

impl Instance {
    /// Tries to collapse an instance from its constraints
    /// down to an "actual type"
    ///
    /// TODO: figure out if this should return an error
    /// if no solution type exists, or if further narrowing is
    /// a better way of dealing with that
    fn describe(&self) -> Option<TypeInstiation> {
        match self.narrowings {
            TypeConstraint::Complete(ti) => Some(ti),
            _other => None,
        }
    }
}

pub enum TypeConstraint {
    /// A "has" type constraint states that the type must "have"
    /// a symbol with the given name with the given Instance,
    /// for example "v.foo(bar)" is described by `Instance(v)`
    /// having a Has constraint with symbol `foo` and
    /// type `(Instance(bar)) -> *`.
    ///
    /// This is not implemented yet since we haven't explored
    /// type inference (only simple deduction so far)
    Has(!),

    /// An "of" type constraint states that the type must
    /// accept a compatible generic argument list to the
    /// one provided
    Of(!),

    /// An "in" type constraint states that the target type
    /// must be exported (either aliased or declared) within a specific module
    ///
    /// Refers to a module context that has been fully resolved by name
    In(CtxID),

    /// A "named" type constraint states that the target type
    /// has a base name matching the provided string
    ///
    /// TODO: consider adding a "generic redirect" here
    /// that can provide for GATs later on or for
    /// templated outer generics as at least a "poor man's GATs"
    Named(IStr),

    /// If a deduction/reduction can be made that narrows to
    /// a single, fully defined, type (such as accessing a field
    /// on a known type struct of a known type member), then
    /// a "complete" TypeConstraint can be constructed that
    /// need not specify any more complicated relationships
    Complete(TypeInstiation),

    /// If multiple constraints are put on a single object
    /// then Multiple can encompass them.
    ///
    /// This works for a variable that is constrained in two or more places,
    /// or for example with generics and specialization
    Multiple(Vec<TypeConstraint>),

    /// If an instance has no guiding information "yet" (for example,
    /// a simple generic argument or a wildcard during assignment),
    /// then Unconstrained can be used as a placeholder
    /// and folded into Multiple later on
    Unconstrained(),
}

struct Implementation {
    for_type: Instance, 
}

/// A RootType describes
/// either an actual trait, or an actual struct,
/// or some other similar type.
///
/// Even though these types are later "augmented" with
/// guards and impls, those types are not RootTypes but
/// are instead AugmentedTypes
///
/// The actual methods, descriptions, fields, field types,
/// are all somewhere in a RootType. Impl blocks *can* add
/// additional methods and such, but they can't fundamentally
/// change the structure of a type (generics, name, etc)
enum RootType {
    Value(ValueType),
    Virtual(VirtualType),
}

enum ComplexType {
    Generic(GenericType),
}

#[derive(Debug, Clone)]
/// A fieldmember can be either a method or a sized field.
/// For value types within the inherent impl context, fields exist within
/// the structural record and methods are fully devirtualized.
///
/// For virtual types, fieldmembers exist as entries within the virttype vtable whether
/// they are methods or variables.
pub struct FieldMember {
    /// Every attribute of a type is named (even if not uniquely).
    pub name: IStr,

    /// Every field is uniquely described by the pair of name and type
    pub ftype: TypeReference,

    /// Any field may have a provided default value for a member.
    ///
    /// Struct inherent implementations of functions demand default falues,
    /// but virtual types do not. Every other (known) instance of FieldMember does not require
    /// any default literal value
    ///
    /// A default value is not really a "value", but is instead implicitly a callable with the
    /// contents wrapped in a parameterless closure. These closures have static constants all
    /// visible to them, as well as typical literals and all imported constants.
    pub default: Callable<()>,
}

/// A VirtualType is an interface or other non-structured type. It can be inherently
/// implemented or runtime composed (and used through a CompositeType).
#[derive(Debug, Clone)]
pub struct VirtualType {
    members: Vec<FieldMember>,
}

#[derive(Debug, Clone)]
pub struct StructType {
    members: Vec<FieldMember>,
}

/*pub trait PrimitiveType: std::fmt::Debug + Clone {
}*/

#[derive(Debug, Clone)]
pub enum PrimitiveType {
    //
}

/// If a type is an enum, struct, or primitive, then it is a ValueType.
#[derive(Debug, Clone)]
pub enum ValueType {
    //Callable(CallableType),
    Struct(StructType),
    Enum(!),
    Primitive(PrimitiveType),
}

/// A composed type is one that only exists at usage sites, and
/// embodies types of the form "T + U". This is not used for types that inherit
/// or have inherent implementations of traits, as those are natively part of the ValueType or
/// VirtualType. This class of type only exists because of the runtime type composition
/// features of LumaLang.
#[derive(Debug, Clone)]
pub struct ComposedType {
    member_types: Vec<TypeReference>,
}
///
/// A RealType is a fully resolved type with all generics resolved.
pub enum RealType {
    Value(ValueType),
    Virtual(VirtualType),
}

pub struct TypeAlias {
    unimplemented: !,
}

pub enum Type {
    Composed(ComposedType),
    Real(RealType),
    Alias(TypeAlias),
}

trait TypeLike {
    fn members(&self) -> Vec<>
}

struct Callable {
}
