//use crate::types::FunctionDeclaration;
//

use std::mem::swap;

//use crate::cst::{self, TypeReference}::{self, TypeReference};
use crate::{helper::interner::IStr, cst::{ExpressionWrapper, ScopedName}};
use dashmap::DashMap;

//use crate::mir::tree::CtxID;


//use crate::cst::expressions::{selExpressionID, ExpressionContext};
use crate::cst;

use super::tree::CtxID;

/// A TypeReference encloses an entire constraint,
/// including any `where` clauses or `+` composition
/// Each `+` is represented by an entry in `bases`
#[derive(Debug)]
pub struct TypeReference {
    bases: Vec<TypeBase>,
}

#[derive(Debug)]
pub enum TypeBase {
    /// 
    Generic(GenericType),

    Resolved(ResolvedType),

    UnResolved(UnResolvedType),
}

#[derive(Debug)]
struct GenericType {
    /// Gennerics aren't yet plumbed through
    ni: !,
}

#[derive(Debug)]
struct ResolvedType {
    base: CtxID,

    /// May or may not yet be directly resolved
    generics: Vec<TypeReference>,
}

#[derive(Debug)]
struct UnResolvedType {
    named: ScopedName,

    generics: Vec<TypeReference>,
}

/*#[derive(Debug)]
pub struct TypeBase {
    /// 
    is_generic: bool,

    refers: NodeReference,

    generics: Vec<
}*/

#[derive(Debug)]
pub enum NodeReference {
    Unresolved(ScopedName),
    Resolved(CtxID),
}

#[derive(Debug)]
pub struct FieldMember {
    pub name: IStr,
    pub has_type: Option<TypeReference>,
    pub initialization: Option<cst::expressions::ExpressionWrapper>,
}

#[derive(Debug)]
pub struct StructuralDataDefinition {
    pub fields: Vec<FieldMember>,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    info: cst::NodeInfo,
    name: IStr,

    parameters: Vec<(IStr, InstanceConstraint)>,
    return_type: InstanceConstraint,

    implementation: cst::expressions::ExpressionWrapper,
}

impl FunctionDefinition {
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
            implementation: todo!(),
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
        Self(cst::NodeInfo::Builtin, InstanceConstraintInner::Unconstrained())
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
