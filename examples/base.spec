// puts everything in foo.rs inside a foo namespace within the current module (named base)
source "foo.rs" as foo;

// takes everything in main.rs and puts it in the current module (named base)
source "main.rs";

// basically acts as an include statement for the spec file
spec "other.spec";

data "icon.png" as icon;
