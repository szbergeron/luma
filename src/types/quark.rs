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

pub struct PrologDeclID(usize);

impl Quark {
    pub fn declare_trait(&mut self, name: StringSymbol) -> PrologDeclID {
        unimplemented!()
    }

    pub fn add_to_trait(&mut self, p_trait: PrologDeclID, trait_member_name: StringSymbol, trait_member_id: GlobalTypeID) -> () {
    }
}
