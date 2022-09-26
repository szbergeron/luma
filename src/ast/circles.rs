use super::tree::CtxID;

/// When we have two interfaces that coexist in an object treated as dyn,
/// then we need to have all objects with either interface have space for both
///
/// Circles is what we use to compute these parallel existances and output object layouts
///
/// It's a fancy set union thing, no more
///
/// Actually it's a bit more
///
/// Say we have traits like Printable and Constructable
/// that a bunch of things implement (even if not all)
///
/// We want to order those first, and then so long
/// as no one does like a: Printable <- Bar {},
/// then we don't actually need all Printable things
/// to have room for Bar
pub struct Circles {
}

impl Circles {
    pub fn coexists(&self, a: CtxID, b: CtxID, because: InterfaceID) {
    }

    pub fn layout(&self, obj: CtxID) -> Layout {
        panic!()
    }
}

/// Specifies the layout of objects of a given type
pub struct Layout {
    //
}
