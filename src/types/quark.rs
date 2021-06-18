/**
 * Quark: lowering engine to take mid-repr type representation
 * and lower to prolog for querying
 **/

//use super::types::GlobalTypeID;
use super::{GlobalTypeID, GlobalSymbolID};
use crate::helper::interner::StringSymbol;

/// A Quark represents a query context, with an opaque implementation
/// for how queries are computed
pub struct Quark {
}

pub struct QuarkDeclID(usize);

pub struct Query {
    //
}

impl Query {
    pub fn filter_name(self, name: StringSymbol) -> Self {
        let b: Vec<_> = Vec::new();
        b.push(5);
        unimplemented!()
    }
}

/// An BasicConstraint (BCst) represents a fully specified
/// reference to a distinct, non-generic, type
/// This could be something such as `i32` or `SomeStruct`,
/// but is not the same as a dyn trait reference
pub struct BasicConstraint {
}

/// A TraitConstraint (TCst) represents a reference to 
/// a dyn trait in some context, and
/// may or may not be generic.
pub struct TraitConstraint {
}

/// A GenericConstraint (GCst) represents a reference
/// where generic parameters are expected to be 
/// present, whether they be directly provided or
/// only provided as wildcards
///
/// If a GCst has had all parameters provided as
/// BasicConstraints then it is considered "fully qualified" (FQ),
/// and alternatively if all parameters provided are
/// either BCst or FQ GCsts, then the GCst itself is then also FQ
pub struct GenericConstraint {
    //
}

/// An UnConstraint (UCst) is a wildcard constraint,
/// where no generic members are provided and
/// no explicit type or trait name is mentioned
///
/// It is allowed to resolve to any valid type and is discarded
/// from consideration by the inference engine as a constraint
pub struct UnConstraint {
}

pub enum NameConstraint {
    Basic(BasicConstraint),
    Trait(TraitConstraint),
    Wildcard(UnConstraint),
}

impl Into<Constraint> for NameConstraint {
    fn into(self) -> Constraint {
        match self {
            Self::Basic(b) => Constraint::Basic(b),
            Self::Trait(t) => Constraint::Trait(t),
            Self::Wildcard(w) => Constraint::Wildcard(w),
        }
    }
}

pub enum Constraint {
    Wildcard(UnConstraint),
    Generic(GenericConstraint),
    Trait(TraitConstraint),
    Basic(BasicConstraint),
}

impl Into<Option<NameConstraint>> for Constraint {
    fn into(self) -> Option<NameConstraint> {
        match self {
            Self::Basic(b) => Some(NameConstraint::Basic(b)),
            Self::Trait(t) => Some(NameConstraint::Trait(t)),
            Self::Wildcard(w) => Some(NameConstraint::Wildcard(w)),
            Self::Generic(_) => None,
        }
    }
}

//pub struct {}

impl Quark {
    pub fn declare_trait(&mut self, name: StringSymbol) -> QuarkDeclID {
        unimplemented!()
    }

    pub fn add_to_trait(&mut self, p_trait: QuarkDeclID, trait_member_name: StringSymbol, trait_member_id: GlobalTypeID) -> () {
    }
}
