/// This file contains algorithms for deciding whether a given function, impl block, or type
/// should be imported or tied to another import

/// trait implementations for objects are "sticky". If a trait has been implemented for
/// a type class anywhere within the current Evaluation Unit (EU), then if the trait is in scope
/// the implementation is too
///
/// If an EU is pulled in alongside the current as a library, and has implementations
/// of some trait for a type, then if that trait is imported the implementation
/// of that trait for that type is also visible/usable.
///
/// Notice that this flows "downstream" from types or traits.
/// We will enforce the orphan rule for implementation blocks once the code to do so is
/// ready, but for now the rule implicitly applies (an object passed "upstream" that implements a
/// trait downstream will not have that trait visible)
