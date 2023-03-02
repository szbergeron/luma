use crate::{helper::interner::IStr, ast::tree::CtxID, compile::per_module::Earpiece};

struct GlobalContext {
    //
}

impl GlobalContext {
    //ops:
    //  "I need tag _ on node _"
    //  "I provide tag _ on node _"
}

#[derive(Clone, Copy)]
pub struct VarID(usize);

/// Stores (shard, inner), so we can look up
/// the shard that contains the interned schema
/// as well as the schema itself
#[derive(Clone, Copy)]
pub struct TypeSchemaID(usize, usize);

pub enum Fact {
    HasSchema(VarID, TypeSchemaID),
    HasField(VarID, IStr, TypeSchemaID),
    MustAssign(TypeSchemaID, TypeSchemaID),
}

/// The "gives cryptic advice" kind not the "80% lawyers" kind
pub struct Oracle {
    units: Vec<usize>,
}

impl Oracle {
    pub fn for_node(node_id: CtxID, earpiece: Earpiece) {
    }

    pub fn thread(self) {
    }

    /// Tell the Oracle the full set of units to wait for
    pub fn init(&mut self, units: Vec<usize>) {
        todo!()
    }

    pub fn add_fact(&mut self, fact: Fact) {}

    /// The "single thread" that steps the phases
    pub async fn solver(&self) {}

    /// This is used when we declare a variable with a given name,
    /// we get back a unique handle
    ///
    /// This requires the caller to handle shadowing itself
    pub async fn declare(&self, unit: usize, name: IStr) -> VarID {
        todo!()
    }

    pub async fn bind(&self, unit: usize, exp: ()) -> VarID {
        todo!()
    }

    /// b1 is assigned into from b2
    pub async fn assigns(&self, unit: usize, b1: VarID, b2: VarID) {
        todo!()
    }

    /*pub async fn require(&self, unit: usize, binding: VarID, to_be: TypeConstraint) {
        todo!()
    }*/

    pub async fn tag_on(&self, unit: usize, on: VarID, tag: VarID) -> VarID {
        todo!()
    }

    /// We say "this type has this field, but we don't know what type it is"
    /// The oracle will tell us if it finds out
    pub async fn has_field(&self, unit: usize, on: VarID, field: IStr) {
        todo!()
    }

    pub async fn has_method(&self, unit: usize, on: VarID, args: Vec<VarID>) {}

    /// If we know a full schema we can say that some var
    /// has it, this happens when we do like an assignment
    pub async fn has_schema(&self, unit: usize, var: VarID, schema: TypeSchemaID) {
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
    pub async fn ask(&self, unit: usize) -> Vec<Fact> {
        todo!()
    }

    /*pub async fn query(&self, unit: usize, query: Query) -> Result<Fact, ()> {
        todo!()
    }*/

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
    pub async fn block(&self, unit: usize) -> Vec<Fact> {
        todo!()
    }

    pub async fn stalled(&self, unit: usize) -> Result<Vec<Fact>, EmptyReason> {
        todo!()
    }

    /// We should only be woken back up on exit, hang here as our
    /// stable state indicating we've given the information we can
    /// and need no more
    pub async fn solved(&self, unit: usize) -> Result<(), EmptyReason> {
        todo!()
    }
}

pub enum EmptyReason {}

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
 *         with c[['qux']] as h {
 *             c[['qux']] = "foo" // we can't get here to check the "foo" as a potential structure
 *                                // for c without already making an assumption for h, but we can
 *                                // sorta ignore that since we're still talking about c here.
 *                                // this allows broader checking, but if a tag is recursive things
 *                                // can't be fully checked in one go
 *                                // we use this to segment solve phases and give functionally
 *                                // deterministic solves on error
 *              if h == "hello" {
 *                  // this is incomparable, but we need to have resolved tag type first
 *              }
 *
 *              c[['qux']] = c[['qux']] + 2 // this is incomputable in a single pass,
 *                                          // requires another stepping to eval,
 *                                          // wouldn't cause a type resolution issue but an
 *                                          // improper assignment at this location instead
 *         }
 *     }
 *
 *     !
 * }
 */
