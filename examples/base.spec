// puts everything in foo.rs inside a foo namespace within the current module (named base)
// source "foo.rsh" as foo;

// takes everything in main.rs and puts it in the current module (named base)
// source "main.rsh" as main;

// check
// source "modules.rsh" as modules;

// check
source "./modification.rsh" as modification;

// basically acts as an include statement for the spec file
// spec "other.spec";

// data "icon.png" as icon;
