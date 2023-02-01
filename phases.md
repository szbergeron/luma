1. CST:

CST is (in some form) carried into AST, but is in the first form
not actually aware of module structure, those links haven't been made

When we hit AST, we have created a fully linked up tree where the contents of functions
are still themselves in CST form, but where types know which modules their fields are from,
use statements have been resolved, function param/return types have been found and referred to

MIR (Middle IR) is the name of the former AST representation, and is what is actually used for local type inference,
and is used as the source material for resolving dynamic fields on types

MIR is read by Quark/Oracle and is not modified directly, but the type of every expr is annotated if possible

Now, every type has ossified and MIR provides all the context needed for lowering, so...

LIR (Lower IR) is a bit above LLVM IR, currently it's nearly 1:1. LIR is expected to be when things such as Inlining/OOLing for dyn fields can happen,
and allows for transformations on monomorphised types. LIR is fully monomorphised from the start, but because of back-annotations
we can do things like combining and devirtualization at this stage (when only one dyn member is ever assigned, or for taking
methods/callables that are currently put in vtables and changing them to direct calls)
