//#![feature(generic_associated_types)]
//#[macro_use] extern crate lalrpop_util;
//#![feature(associated_type_defaults)]
//#![feature(option_expect_none)]
#![feature(arc_new_cyclic, once_cell, new_uninit, bool_to_option, iter_intersperse, vec_into_raw_parts, layout_for_ptr, ptr_metadata, const_generics_defaults, inherent_associated_types, try_trait_v2)]

#[allow(unused_imports)]
#[macro_use]
extern crate static_assertions;

#[macro_use]
extern crate lazy_static;

//lalrpop_mod!(pub rsh);

//#[macro_use]
//extern crate lazy_static;

use std::env;

pub mod ast;
pub mod helper;
pub mod lalrpop_lexer;
pub mod lex;
pub mod parse;
pub mod traits;
pub mod encode;
pub mod build_expr;
pub mod check;
pub mod compile;
pub mod mid_repr;
pub mod types;
pub mod avec;

fn main() {

    let args: Vec<String> = env::args().collect();
    println!("Args: {:?}", args);
    let sliced: Vec<&str> = args.iter().map(|s| &s[..]).collect();

    let sliced: &[&str] = &sliced[1..];
    println!("stripped args: {:?}", sliced);

    compile::launch(sliced);
}
