/**
 * Quark: lowering engine to take mid-repr type representation
 * and eventually, lower to prolog for querying
 *
 * Current implementation to act as stub for type propagation
 * from fully specified usage sites instead
 **/
//use super::types::GlobalTypeID;
use super::{GlobalCtxNode, GlobalFunctionID, GlobalTypeID};
use crate::helper::interner::IStr;

pub struct QuarkDeclID(usize);

pub struct Query {
    //
}

impl Query {
    pub fn filter_name(self, _name: IStr) -> Self {
        let mut b: Vec<_> = Vec::new();
        b.push(5);
        unimplemented!()
    }
}

pub struct Fact {}

/// An BasicConstraint (BCst) represents a fully specified
/// reference to a distinct, non-generic, type
/// This could be something such as `i32` or `SomeStruct`,
/// but is not the same as a dyn trait reference
pub struct BasicConstraint {
    pub name: IStr,
}

/// A TraitConstraint (TCst) represents a reference to
/// a dyn trait in some context, and
/// may or may not be generic.
pub struct TraitConstraint {
    pub name: IStr,
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
    // TODO: potentially limit primary to be
    // of some NameConstraint type that omits
    // any generic type itself
    pub primary: Box<Constraint>,
    pub params: Vec<Constraint>,
}

/// An UnConstraint (UCst) is a wildcard constraint,
/// where no generic members are provided and
/// no explicit type or trait name is mentioned
///
/// It is allowed to resolve to any valid type and is discarded
/// from consideration by the inference engine as a constraint
pub struct UnConstraint {}

/*pub enum NameConstraint {
    Basic(BasicConstraint),
    Trait(TraitConstraint),
    Wildcard(UnConstraint),
}*/

/*impl Into<Constraint> for NameConstraint {
    fn into(self) -> Constraint {
        match self {
            Self::Basic(b) => Constraint::Basic(b),
            Self::Trait(t) => Constraint::Trait(t),
            Self::Wildcard(w) => Constraint::Wildcard(w),
        }
    }
}*/

pub enum Constraint {
    Wildcard(UnConstraint),
    Generic(GenericConstraint),
    Trait(TraitConstraint),
    Basic(BasicConstraint),
}

/*impl Into<Option<NameConstraint>> for Constraint {
    fn into(self) -> Option<NameConstraint> {
        match self {
            Self::Basic(b) => Some(NameConstraint::Basic(b)),
            Self::Trait(t) => Some(NameConstraint::Trait(t)),
            Self::Wildcard(w) => Some(NameConstraint::Wildcard(w)),
            Self::Generic(_) => None,
        }
    }
}*/

//pub struct {}

/*pub struct QueryResult {
    yes: bool,
}*/

/// A Quark represents a query context, with an opaque implementation
/// for how queries are computed
pub struct Quark {
    within: &'static GlobalCtxNode,
}

impl Quark {
    pub fn declare_trait(&mut self, _name: IStr) -> QuarkDeclID {
        unimplemented!()
    }

    pub fn add_to_trait(
        &mut self,
        _p_trait: QuarkDeclID,
        _trait_member_name: IStr,
        _trait_member_id: GlobalTypeID,
    ) -> () {
    }

    /// If a coercion from the argument type to the target type exists,
    /// a function that accomplishes the conversion is provided as a Some(_) return value
    ///
    /// If no conversion exists, or any conversion is not resolvable, then None is returned
    pub fn coerces(
        &self,
        argument: GlobalTypeID,
        target: GlobalTypeID,
    ) -> Option<GlobalFunctionID> {
        if argument != target {
            // TODO: don't currently handle upcasting/traitcasting of params
            None
        } else {
            unimplemented!()
        }
    }

    /// Unsafe contract: requires that `direct` be valid for at least as long as self, but
    /// not necessarily for the full 'static lifetime
    pub unsafe fn new_within(direct: &'static GlobalCtxNode) -> Quark {
        Quark { within: direct }
    }

    /*/// Mutates the `within` member to align with the provided ref.
    /// This is unsafe as the given ref is actually not required to be 'static,
    /// and instead is only required to live as long as 'self
    pub unsafe fn set_within(&mut self, r: &'static GlobalCtxNode) {
        self.within = r;
    }*/

    /*pub fn new_within(within: Weak<GlobalCtxNode>, global: Weak<GlobalCtxNode>) -> Quark {
        Quark {
            _within: within,
            _global: global,
        }
    }*/

    /*fn within_ref(&self) -> &GlobalCtxNode {
        unsafe {
            //&*self.within
        }
    }

    fn global_ref(&self) -> &GlobalCtxNode {
        unsafe {
            &*self.global
        }
    }*/

    //pub fn
}
