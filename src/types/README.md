// notes
Basic:
    Trait([(Element, VarianceAnnotation)], Nullable(SuperTrait: Trait)): Element:
        any Trait is defined both by a unique identity and by the symbols it contains.

        Traits are possible to subtype, and allow marking associated symbols as invariant or covariant

    Element:
        An element can be a trait, a struct, a primitive type, a function, an enum, or any other symbol that can be placed inside a namespace or trait

        Elements 

Constraints:
    FullNegative(Trait):
        Constrains an object to implementing none of the trait specified

    FullPositive(Trait):
        Constrains an object to implementing all of the trait specified

    PartialPositive(Trait, [ElementRef]): 
        Constrains an object to implementing at least the referenced elements of the referenced trait

    PartialNegative(Trait, [ElementRef]):
        Constrains an object to implementing none of the referenced elements of the referenced trait

Compositions:
    Subset(Trait, [symbols]):
        Constructs a trait that takes 






Basic:
    Space: Element:
        A space is analogous to a namespace, but appears in several different contexts.
        A space is separate from any other space, including identically defined spaces.had

    Element:
        An element describes a single symbol within some space


Types themselves are divided into two categories:
    Value Types:
        Value types speak to the structure of the data or the data itself, examples would be structs, specific sized integers, enums, or other such types.

        Value types are always invariant for constraints, so no value type may be assigned into any *different* value type.

    Trait Types:
        Trait types speak to the abilities an object is capable of, and put no restriction on the structure of the data used for 


//All accesses are done through associated vtables, with each object being 

each object is laid out, when by value, as a value and a pointer. When a modification to a by-value object occurs, the behavior that results from the modification varies. 

Objects follow one of two types:
    Fixed:
        Fixed objects are those that are held "by value", and are unboxed and on the stack/in registers.
        Fixed objects have no dynamic dispatch capability, and their type is not mutable and not run-time polymorphic.

    Dynamic:
        Dynamic objects are objects that are solely owned as boxed values, and are always located on the heap.
        Dynamic objects allow for dynamic dispatch, trait promotion/demotion, and run-time polymorphism.

Fixed objects may be copied and promoted to dynamic objects losslessly.
However, going from a dynamic object to a fixed object is less straightforward.
A fixed object may be initialized from a dynamic object where that dynamic object allows
for run-time coercion to a value type of the specified type. The ability to convert in this way
must be defined within the type that the dynamic object is known as, or the dynamic object
must first be confirmed through trait introspection to allow this conversion.


Syntax:

References:
    Type references follow this form:
        TypeReference = Identifier | Tuple | GenericSpecifier | Wildcard

        GenericSpecifier = Identifier < TypeList >

        TypeList = TypeReference | TypeReference , TypeList | <null derives>

        Tuple = ( TypeList )

        Wildcard = _

    Type references will hopefully allow for value specification (constexpr or value arguments) in the future

    Type references are intended to be used both for providers (function return type) and for constraints,
    and are the primary tool by which type inference is accomplished
