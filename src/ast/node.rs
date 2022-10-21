use crate::helper::interner::IStr;

struct GlobalContext {
    //
}

impl GlobalContext {
    //ops:
    //  "I need tag _ on node _"
    //  "I provide tag _ on node _"
}

pub struct VarID(usize);

/// Stores (shard, inner), so we can look up
/// the shard that contains the interned schema
/// as well as the schema itself
pub struct TypeSchemaID(usize, usize);

pub enum Fact {
    HasSchema(VarID, TypeSchemaID),
    HasField(VarID, IStr, TypeSchemaID),
    MustAssign(TypeSchemaID, TypeSchemaID),
}

/// The "gives cryptic advice" kind not the "80% lawyers" kind
struct Oracle {
}

impl Oracle {
    /// This is used when we declare a variable with a given name,
    /// we get back a unique handle
    ///
    /// This requires the caller to handle shadowing itself
    pub fn declare(&self, name: IStr) -> VarID {
        todo!()
    }

    pub fn bind(&self, exp: ()) -> VarID {
        todo!()
    }

    pub fn assigns(&self, b1: VarID, b2: VarID) {
        todo!()
    }

    pub fn require(&self, binding: VarID, to_be: TypeConstraint) {
        todo!()
    }

    pub fn tag_on(&self, on: VarID, tag: VarID) -> VarID {
        todo!()
    }

    /// We say "this type has this field, but we don't know what type it is"
    /// The oracle will tell us if it finds out
    pub fn has_field(&self, on: VarID, field: IStr) {
        todo!()
    }

    /// If we know a full schema we can say that some var
    /// has it, this happens when we do like an assignment
    pub fn has_schema(&self, var: VarID, schema: TypeSchemaID) {
        todo!()
    }

    /// We can ask the oracle if it has any wisdom for us,
    /// and it will give us an unordered list of things it
    /// has been able to figure out
    ///
    /// We can then apply those Facts and potentially tell it
    /// more things
    ///
    /// If we have no more facts, and everything else has reached a resting
    /// state, and we have anything unresolved, then that means
    /// there is an unresolvable question somewhere
    pub fn ask(&self) -> Vec<Fact> {
        todo!()
    }

    /// Once the current solve context has done everything it
    /// can with the current leased facts, it can block on a future set of facts
    ///
    /// If the returned set of *new* facts is empty we may have an error,
    /// so use the `stalled()` wait to inform the oracle that
    /// we are unable to do anything useful
    ///
    /// If the state of all contexts converges to solved() and stalled(), the oracle exits
    ///
    /// On oracle exit, the contexts are told that they have gotten all the
    /// information they are going to get, so they are free to emit errors or continue to
    /// final output
    pub fn block(&self) -> Vec<Fact> {
        todo!()
    }

    pub fn stalled(&self) -> Result<Vec<Fact>, EmptyReason> {
        todo!()
    }

    pub fn solved(&self) -> Result<(), EmptyReason> {
        todo!()
    }
}

/*
 * fn corge<T>(f: T) -> Baz<T> {
 *     Baz<T> { d: "g" }
 * }
 *
 * fn foo(a: Bar) -> Baz<i32> {
 *     let c;
 *
 *     c = Baz<i64> { d: "e" };
 *
 *     for b in 1..5 {
 *         c[['qux']] = b + 3; // b + 3, since b is i32 and 3 is i32 expr is i32, so we say 'qux'
 *                             // on type(c) must be i32
 *         with h as c[['qux']] {
 *             c[['qux']] = "foo" // we can't get here to check the "foo" as a potential structure
 *                                // for c without already making an assumption for h, but we can
 *                                // sorta ignore that since we're still talking about c here.
 *                                // this allows broader checking, but if a tag is recursive things
 *                                // can't be fully checked in one go
 *                                // we use this to segment solve phases and give functionally
 *                                // deterministic solves on error
 *         }
 *     }
 *
 *     !
 * }
 */
