use super::quark::Linear;

/// Deduction method takes a Linear and resolves types by doing straightforward assignment back
/// propagation
///
/// Constructors must be able to be fully constrained from forward stepping from their import,
/// so the base of a call or expression must be something we can resolve directly to a module or item
///
///
///

impl Quark {
    pub fn solve(l: &mut Linear) {
        //
    }

    pub fn chase(a: AllocationReference) {}
}
