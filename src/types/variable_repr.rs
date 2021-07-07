use crate::helper::interner::StringSymbol;

/**
 * Spitballing here, trying to figure out how
 * I want to handle tracing variable references 
 * and types through a program.
 *
 * Ultimately, being able to infer types through function calls
 * and similar boundaries would be useful, but doing this
 * requires solving an arbitrary tree based on type 
 * transformations and may get computationally unreasonable 
 * even for a fact-based constraint solver.
 *
 * Currently I think I'll slice the data first along bindings,
 * tracking individual bindings and doing first layer *forward*
 * stepping inference (can only solve on a return bound
 * for a single function depth)
 */

/**
 * As another tidbit of typesystem info,
 * the & type is actually a generic,
 * so &T becomes &<T>, and can be monomorphised from there
 */

pub struct Generic {
}

#[allow(dead_code)]
pub struct Variable {
    name: StringSymbol,
    
    //instance_params: Vec<

    uses: Vec<VariableUsage>,

}

pub struct VariableUsage {
}
