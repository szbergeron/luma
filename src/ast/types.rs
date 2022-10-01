//use crate::types::FunctionDeclaration;
//
mod old {
    use crate::{cst::SyntaxTypeReference, helper::interner::IStr};

    use crate::cst;

    use crate::cst::{CstNode, IntoCstNode, NodeInfo};

    pub mod types {
        /// A TypeReference represents the sort of thing that goes directly after declaring a variable,
        /// or for a function parameter. At parse time and AST construction, it is unresolved and
        /// the only information it has is the canonicalized type string and the scope that it attempts
        /// to reference. It is resolved (and modified using interior mutability) to the type that it
        /// resolves to during a compiler pass, which holds a reference to a constraint, and possibly
        /// an actual TypeID or TypeHandle
        #[derive(Debug)]
        pub struct TypeReference {
            resolution: std::sync::RwLock<TypeResolution>,
            syntax: crate::cst::SyntaxTypeReference,
        }

        impl Clone for TypeReference {
            fn clone(&self) -> Self {
                Self {
                    syntax: self.syntax.clone(),
                    resolution: std::sync::RwLock::new(self.resolution.read().unwrap().clone()),
                }
            }
        }

        #[derive(Debug, Clone)]
        pub enum TypeResolution {
            Unresolved(),
            Resolved(std::sync::Arc<TypeConstraint>),
        }

        /// Every type reference is effectively a constraint on what that variable can be.
        #[derive(Debug, Clone)]
        pub enum TypeConstraint {
            /// A "narrowing" constraint is one where a partial reference is given.
            /// This could be in the case where a variable is being assigned from a function and we
            /// want to hold it as some type, or where we are taking a literal type and
            /// provide a "u128" constraint to the variable it's going in to which backpropagates
            /// and changes what type the literal appears as
            ///
            /// It also means that if we have a variable of type T, but it comes from a source of T+U,
            /// that variable itself is only T rather than being T+U from the type system perspective.
            ///
            /// Accessing U again requires a narrowing conversion with some (T) -> Maybe<T+U>
            Widening(),

            /// A composite constraint is one that inherits the constraints of its uses. This
            /// corresponds to "wildcard" or other non-constraints, such as implicit constraints within
            /// an expression. If we have a function that returns T+U, save it into a _ variable, and
            /// then only pass that variable into a (U) -> ?, then _ is resolved to U. If we try to
            /// pass a _ variable from a T+U function into a (V) -> ?, then the composite is
            /// unresolvable and the compiler throws an error
            Composite(),

            /// A source constraint is an absolute constraint. This appears in cases like the
            /// post-generic-resolution signature of a function. This is also used for global
            /// constants.
            Source(),
        }

        use crate::{cst::ExpressionWrapper, helper::interner::IStr};

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
            pub default: BasicClosure,
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
            //Callable(CallableType), // TODO
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

        #[derive(Debug, Clone)]
        pub struct BasicClosure {
            signature: ValueType, // always a CallableType

            arguments: Vec<IStr>,

            code: Box<ExpressionWrapper>,
        }

        /*pub trait Type {
            fn fields(&self) -> Vec<&FieldMember>;

            fn methods(&self) -> Vec<&FieldMember>;
        }*/

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
    }

    #[derive(Debug, Clone)]
    pub struct DefinitionBody {
        fields: Vec<FieldMember>,
        //methods: Vec<FunctionDeclaration>,
        //aliases: Vec<AliasDeclaration>,
    }

    #[derive(Debug, Clone)]
    pub enum ImplementationItem {
        Function(cst::FunctionDefinition),
        Field(FieldMember),
    }

    #[derive(Debug, Clone)]
    pub struct ImplementationBody {
        pub node_info: NodeInfo,
        pub items: Vec<ImplementationItem>,
    }

    #[derive(Debug, Clone)]
    pub struct Implementation {
        pub node_info: NodeInfo,

        pub body: ImplementationBody,

        pub impl_of: Option<SyntaxTypeReference>,

        pub impl_for: SyntaxTypeReference,
    }

    impl CstNode for Implementation {
        fn node_info(&self) -> NodeInfo {
            self.node_info
        }
    }

    #[derive(Debug, Clone)]
    pub struct FieldMember {
        //pub attributes: MemberAttributes,
        pub name: IStr,

        pub ftype: SyntaxTypeReference,
    }

    impl CstNode for FieldMember {
        fn node_info(&self) -> NodeInfo {
            todo!()
        }
    }

    /// These act like structs or records in any other language,
    /// Member functions here (once implemented) are devirtualized by default
    ///
    /// This is a product type
    #[derive(Debug, Clone)]
    pub struct StructDefinition {
        pub name: IStr,
        pub node_info: NodeInfo,
        pub fields: Vec<FieldMember>,
    }

    /// Not yet implemented, but will allow for typical pattern matching
    /// later.
    ///
    /// This is a sum type
    #[derive(Debug, Clone)]
    pub struct EnumDefinition {
        no: !,
    }

    /// These are used for either record or enum types, and allow adding
    /// immutable, replaceable, vtables to types
    #[derive(Debug, Clone)]
    pub struct TraitDefinition {
        pub node_info: NodeInfo,
        pub attrs: cst::MemberAttributes,
        //generic_params: Vec<IStr>,
        pub implements_type: Option<SyntaxTypeReference>,
        pub for_type: SyntaxTypeReference,
        pub body: DefinitionBody,
    }

    /// Allows specifying a type schema for virtual record types
    #[derive(Debug, Clone)]
    pub struct RecordVirtualSpecification {}

    impl CstNode for RecordVirtualSpecification {
        fn node_info(&self) -> NodeInfo {
            todo!()
        }
    }

    impl CstNode for StructDefinition {
        fn node_info(&self) -> NodeInfo {
            self.node_info
        }

        /*fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
            writeln!(f, "{} struct {} {{", indent(depth), self.name.resolve());
            for field in self.fields {
            }
            writeln!(f, "{}}}", indent(depth));
        }*/
    }

    impl CstNode for EnumDefinition {
        fn node_info(&self) -> NodeInfo {
            todo!()
        }
    }

    impl CstNode for TraitDefinition {
        fn node_info(&self) -> NodeInfo {
            self.node_info
        }
    }

    #[derive(Debug, Clone)]
    pub enum TypeDefinition {
        Struct(StructDefinition),
        Enum(EnumDefinition),
        Specification(RecordVirtualSpecification),
        Implementation(TraitDefinition),
    }

    impl CstNode for TypeDefinition {
        fn node_info(&self) -> NodeInfo {
            match self {
                Self::Struct(s) => s.node_info(),
                Self::Enum(s) => s.node_info(),
                Self::Specification(s) => s.node_info(),
                Self::Implementation(s) => s.node_info(),
            }
        }
    }

    impl IntoCstNode for TypeDefinition {
        fn as_node(&self) -> &dyn CstNode {
            match self {
                Self::Struct(e) => e,
                Self::Enum(e) => e,
                Self::Specification(e) => e,
                Self::Implementation(e) => e,
            }
        }
    }
}

use std::mem::swap;

//use crate::cst::{self, TypeReference}::{self, TypeReference};
use crate::cst::{self, NodeInfo};
use crate::helper::interner::IStr;
use dashmap::DashMap;
use smallvec::SmallVec;

use crate::ast::tree::CtxID;

#[derive(Debug)]
pub struct FieldMember {
    pub name: IStr,
    pub has_type: Option<InstanceConstraint>,
    pub initialization: Option<Initialization>,
}

#[derive(Debug)]
pub struct StructuralDataDefinition {
    pub fields: Vec<FieldMember>,
}

#[derive(Debug)]
pub struct CallableDefinition {
    info: cst::NodeInfo,
    name: IStr,

    parameters: Vec<(IStr, InstanceConstraint)>,
    return_type: InstanceConstraint,

    implementation: cst::ExpressionWrapper,
}

impl CallableDefinition {
    pub fn from_cst(f: cst::FunctionDefinition) -> Self {
        let cst::FunctionDefinition {
            info,
            public,
            name,
            body,
            return_type,
            params,
        } = f;

        let return_type = InstanceConstraint::from_cst(return_type);

        let parameters = params.into_iter().map(|(name, tr)| {
            (name, InstanceConstraint::from_cst(tr))
        }).collect();

        Self {
            info,
            name,
            implementation: body.into_ast(),
            return_type,
            parameters,
        }
    }
}

/// Implementations are not a variant within NodeUnion because they don't actually represent
/// any referenceable symbol. Instead, an implementation (with constraints) is "applied"
/// as it exists to the matching children of any node that has "imported" it (which
/// implicitly includes the context in which it is defined)
///
/// Eventually, implementations might be a named property that can be imported and act as a regular
/// node.
#[derive(Debug)]
pub struct Implementation {
    of_type: InstanceConstraint,
    for_type: InstanceConstraint,

    implementation: !,
}

/// An Instance is a (partially | fully) resolved
/// representation of "some type".
///
/// Types can usually have instances, modules and functions
/// aren't really types and thus can't really have "instances",
/// though a function is/can be held as an "instance" of a type.
///
/// An Instance is basically best thought of as the type
/// information for a variable or expression
#[derive(Debug)]
pub struct Instance {
    narrowings: InstanceConstraint,
}

/// Represents a fully resolved type, with all generics filled in
/// (or at least a required subset of them)
#[derive(Debug, Clone)]
pub struct TypeInstiation {
    instantiates: CtxID,
    generic_arguments: DashMap<cst::GenericHandle, TypeInstiation>,
}

#[derive(Debug, Clone)]
pub struct InstanceConstraint(cst::NodeInfo, InstanceConstraintInner);

#[derive(Debug, Clone)]
pub enum InstanceConstraintInner {
    /// A "has" type constraint states that the type must "have"
    /// a symbol with the given name with the given Instance,
    /// for example "v.foo(bar)" is described by `Instance(v)`
    /// having a Has constraint with symbol `foo` and
    /// type `(Instance(bar)) -> *`.
    ///
    /// This is not implemented yet since we haven't explored
    /// type inference (only simple deduction so far)
    Has(IStr, Box<InstanceConstraint>),

    /// An "of" type constraint states that the type must
    /// accept a compatible generic argument list to the
    /// one provided
    Of(Vec<Option<InstanceConstraint>>),

    /// An "in" type constraint states that the target type
    /// must be exported (either aliased or declared) within a specific module
    ///
    /// Refers to a module context that has been fully resolved by name
    In(CtxID),

    /// Since modules are types,
    /// we accomplish scoping by a "backwards list"
    /// of how we represent a given type.
    /// This also lets us very early on detect
    /// if someone is trying to use two types
    /// by the same name from different modules,
    /// or find the earliest module mismatch when
    /// trying to match two types
    //Within(Box<TypeConstraint>),

    /// This is the inverse of the former `Within(_)` variant,
    /// basically saying that the resolution of this
    /// type must contain a type that satisfies
    /// this constraint as a related type.
    ///
    /// This allows implementing modules as types nicely,
    /// where while it makes resolving the inner
    /// types directly a bit harder (as they are leaves
    /// instead of roots), it means for the usual case
    /// where imports are more limited than references
    /// and imports are more clearly defined, we can
    /// give the user more precise diagnostics saying
    /// things like "you tried to use B with two different (wrong)
    /// sets of generics, but we know that B is inside A so we
    /// can just tell you what the correct generics are"
    Containing(Box<InstanceConstraint>),

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
    Multiple(Vec<InstanceConstraint>),

    /// If an instance has no guiding information "yet" (for example,
    /// a simple generic argument or a wildcard during assignment),
    /// then Unconstrained can be used as a placeholder
    /// and folded into Multiple later on
    Unconstrained(),
}

impl InstanceConstraint {
    pub fn add(self, other: InstanceConstraint) -> Self {
        use InstanceConstraintInner as TCI;

        match (self, other) {
            (Self(so, TCI::Multiple(mut sc)), Self(oo, TCI::Multiple(mut oc))) => {
                // need to join them, we convert origin to a builtin since the leaves already know
                // origins

                sc.append(&mut oc);

                Self(cst::NodeInfo::Builtin, TCI::Multiple(sc))
            }
            (Self(_, TCI::Unconstrained()), Self(origin, constraint))
            | (Self(origin, constraint), Self(_, TCI::Unconstrained())) => Self(origin, constraint),
            (Self(so, TCI::Multiple(mut sc)), other) | (other, Self(so, TCI::Multiple(mut sc))) => {
                sc.push(other);

                Self(so, TCI::Multiple(sc))
            }
            (other1, other2) => Self(cst::NodeInfo::Builtin, TCI::Multiple(vec![other1, other2])),
        }
    }

    pub fn remove(&mut self) -> Self {
        let mut swapped = Self(cst::NodeInfo::Builtin, InstanceConstraintInner::Unconstrained());
        swap(self, &mut swapped);

        swapped
    }

    pub fn with_generics(
        info: cst::NodeInfo,
        name: IStr,
        generics: Vec<cst::SyntaxTypeReference>,
    ) -> Self {
        // TODO: wildcard generic args
        let gens = generics.into_iter().map(|tr| Some(Self::from_cst(tr))).collect();

        use InstanceConstraintInner as TCI;

        let of = TCI::Of(gens);
        let of = Self(info, of);

        let named = Self(info, TCI::Named(name));

        Self(info, TCI::Multiple(vec![named, of]))
    }

    pub fn unconstrained() -> Self {
        Self(NodeInfo::Builtin, InstanceConstraintInner::Unconstrained())
    }

    pub fn from_cst(r: cst::SyntaxTypeReference) -> Self {
        let cst::SyntaxTypeReference {
            info,
            ctx,
            name,
            type_args,
        } = r;

        let mut root = box Self::with_generics(info, name, type_args);

        for entry in ctx.scope.iter().rev() {
            root = box Self(
                cst::NodeInfo::Builtin,
                InstanceConstraintInner::Multiple(vec![
                    Self(
                        cst::NodeInfo::Builtin,
                        InstanceConstraintInner::Containing(root),
                    ),
                    Self(cst::NodeInfo::Builtin, InstanceConstraintInner::Named(*entry)),
                ]),
            );
        }

        *root
    }
}
