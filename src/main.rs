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
    never_type,
    backtrace_frames,
    unboxed_closures,
    fn_traits,
    array_chunks,
    assert_matches,
    exclusive_range_pattern,
    generic_const_exprs,
    const_trait_impl,
    box_patterns
)]
#![allow(irrefutable_let_patterns)]
#![allow(dead_code)]
#![allow(incomplete_features)]
#![allow(unused_variables)]
#![allow(unreachable_code)]

#[allow(unused_imports)]
#[macro_use]
extern crate static_assertions;

#[macro_use]
extern crate lazy_static;

use console_subscriber;

//#[macro_use]
//extern crate ouroboros;

//#[macro_use]
//extern crate enum_display_derive;

//lalrpop_mod!(pub rsh);

//#[macro_use]
//extern crate lazy_static;

use std::env;

pub mod mir; // just until we fix lowering, reduce error count
pub mod lir;
pub mod ast;
pub mod avec;
pub mod build_expr;
pub mod check;
pub mod compile;
pub mod cst;
pub mod encode;
pub mod helper;
pub mod lalrpop_lexer;
pub mod lex;
pub mod llvm;
pub mod lowered;
//pub mod mid_repr;
pub mod parse;
pub mod traits;
pub mod types;
//pub mod bump_interner;

fn main() {
    console_subscriber::init();
    let args: Vec<String> = env::args().collect();
    println!("Args: {:?}", args);
    let sliced: Vec<&str> = args.iter().map(|s| &s[..]).collect();

    let sliced: &[&str] = &sliced[1..];
    println!("stripped args: {:?}", sliced);

    compile::launch(sliced);
}
