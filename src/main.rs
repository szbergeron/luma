//#![feature(generic_associated_types)]
//#[macro_use] extern crate lalrpop_util;
//#![feature(associated_type_defaults)]
//#![feature(option_expect_none)]
#![feature(
    once_cell,
    new_uninit,
    iter_intersperse,
    vec_into_raw_parts,
    layout_for_ptr,
    ptr_metadata,
    inherent_associated_types,
    try_trait_v2,
    try_trait_v2_residual,
    min_specialization,
    type_changing_struct_update,
    let_chains,
    more_qualified_paths,
    if_let_guard,
    const_heap,
    const_mut_refs,
    box_syntax,
    never_type
)]

#![allow(irrefutable_let_patterns)]
#![allow(dead_code)]
#![allow(incomplete_features)]

#[allow(unused_imports)]
#[macro_use]
extern crate static_assertions;

#[macro_use]
extern crate lazy_static;

//#[macro_use]
//extern crate ouroboros;

//#[macro_use]
//extern crate enum_display_derive;

//lalrpop_mod!(pub rsh);

//#[macro_use]
//extern crate lazy_static;

use std::env;

pub mod ast;
pub mod cst;
pub mod avec;
pub mod build_expr;
pub mod check;
pub mod compile;
pub mod encode;
pub mod helper;
pub mod lalrpop_lexer;
pub mod lex;
pub mod mid_repr;
pub mod parse;
pub mod traits;
pub mod types;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("Args: {:?}", args);
    let sliced: Vec<&str> = args.iter().map(|s| &s[..]).collect();

    let sliced: &[&str] = &sliced[1..];
    println!("stripped args: {:?}", sliced);

    compile::launch(sliced);
}
