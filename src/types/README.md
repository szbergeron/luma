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
        A space is separate from any other space, including identically defined spaces.

    Element:
        An element describes a single symbol within some space
