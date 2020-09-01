use super::base::TypeID;

/**
 * Solver has the following operations:
 *
 * register(Type Handle)
 *   This operation allows adding a new reference to the registry.
 *   A handle ID is returned that allows efficiently referring to
 *   some type that the registry knows of and can concurrently access.
 *
 * request(Type Handle, Symbol Name, Symbol Type: TypeHandle) -> [Symbols]
 *   This operation tells the solver to attempt to find some symbol by the name
 *   and type specified defined for the indicated type. This returns
 *   a set of matching symbols that can then be selected from.
 *
 *   An example usage would be method calls or method chains,
 *   where you wish to infer which method is being used.
 *
 * define(Type Handle, [Symbols]) -> Definition Handle
 *   This operation allows adding a definition to some type,
 *   such as with an `impl` block.
 *
 * constrain(Source: Type Handle, Constraint: Type Handle) -> Option<Type Handle>
 *   This operation allows using a destination type that the source type must widen to.
 *   This allows for inferring type of partially specified variables.
 *
 * propose(Source: Type Handle
 **/

/**
 * let f = ContainerType::new(); // generic over some contained T
 * let g: ContainerType<dyn SomeTrait> = f; // constrains T to be a type that impl SomeTrait
 * g.insert(5); // constrains T to be something that allows implicit From<integer>
 *
 * in this example, the type of `f` is the primary type being inferred. All
 * type definitions in scope are considered. The type is constrained in two places to be
 * `ContainerType<T>`, which allows the method `insert` to be considered. The `ContainerType`
 * in scope here defines insert as `pub meth insert<U>(self, item: T) where T: From<U>`
 *
 * Note that all items implicitly implement `From<T>` where T is themselves.
 *
 * Here, we have the following facts:
 *   T is From<integer>
 *   T is dyn SomeTrait
 *
 * This means that we must select from our pool of type definitions
 *
 * The pool of definitions is set up as a mapping from prepositions to facts.
 *
 * These are of the form `T where T is { U, V, ... } is also { W, X, ... }`.
 * This can be rephrased as a mapping from { U + V + ... } to { W + X + ... }.
 * If the set of traits from the first set is a subset of the traits that some T 
 * implements, then T by definition also implements all traits from the second set.
 *
 * If a rule applies to some T, then for each trait in the dependency set (W, X, ...)
 * a fact is introduced of `T is W`. Once the closure is complete, the set of
 * atomic types (structs, enums, primitives) is filtered at each level of the 
 * type. This means that for each type of the form T<U, V, ...> each of T, U, and V
 * is filtered based on the provided facts. If the set of possible types is of cardinality 1,
 * then inference succeeds and that type is used. If the set is of cardinality greater than 1,
 * an error is yielded telling the user to select from the found set and specify as
 * an annotation. If cardinality is zero, a different error is given stating that no compatible
 * type was found. If the found type is itself generic and the type parameters can 
 * not be inferred based on the given facts, another error is given saying that a T must be
 * specified.
 *
 * Critically to the decidability of this approach, if there is more than one possible
 * inferred type, no generic parameters of those types are to be explored. This does
 * prevent some types of inference, but this serves to bound computation as well.
 * If this were not the case, the solver could get lost in a recursive loop
 * exploring Vec<Vec<Vec<Vec<...>>>> for implementations of required traits.
 * This does mean that this case requires type annotations:
 *
 * let f: Trait1 = Vec::new();
 *
 * when the only implementation of Trait1
 * exists for Vec<Vec<Vec<integer>>>, since exploration
 * stops after the first generic.
 *
 * Types are defined primarily by their contained symbols. Symbols do not
 * help for initial type inference (otherwise misspelling a method or function
 * would cause a type error, which would be a strange user experience) but are 
 * used for when the results of methods or functions are being inferred.
 *
 * Once a type has been inferred and fully constrained for some variable,
 * inference can use methods and fields defined on that type to check other variables.
 **/

pub struct Rule {
    requires: std::collections::HashSet<TypeID>,
    provides: std::collections::HashSet<TypeID>,
}

pub struct Registry {
    types: std::collections::HashMap<TypeID, Box<dyn Type>>,
}

pub struct Context {
    registry: Registry,
}

impl Context {
    pub fn register(&mut self, tr: Box<dyn Type>) -> TypeID {
    }
}
