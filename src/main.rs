//#![feature(generic_associated_types)]
//#[macro_use] extern crate lalrpop_util;
//#![feature(associated_type_defaults)]
#![feature(option_expect_none)]
#![feature(arc_new_cyclic)]

#[allow(unused_imports)]
#[macro_use]
extern crate static_assertions;

#[macro_use]
extern crate lazy_static;

//lalrpop_mod!(pub rsh);

//#[macro_use]
//extern crate lazy_static;

use std::env;
use tokio::runtime::*;
//use std::fs;
//use logos::Logos;

//mod lib;
pub mod ast;
pub mod helper;
pub mod lalrpop_lexer;
pub mod lex;
pub mod parse;
pub mod traits;
pub mod encode;
//pub mod parse_expr;
pub mod build_expr;
pub mod check;
pub mod compile;
pub mod mid_repr;
pub mod types;
//pub mod parse_helper;

//#[macro_use] extern crate lalrpop_util;
//lalrpop_mod!(pub grammar);

//pub type StringSymbol = lasso::LargeSpur;
//#[tokio::main]
fn main() {
    unsafe {
        crate::helper::interner::init_interner();
    }

    let args: Vec<String> = env::args().collect();
    println!("Args: {:?}", args);
    let sliced: Vec<&str> = args.iter().map(|s| &s[..]).collect();
    //sliced.remove(0);

    let sliced: &[&str] = &sliced[1..];
    println!("stripped args: {:?}", sliced);

    compile::launch(sliced);

    /*let files = &args[1..];
    for file in files {
        //let contents = fs::read_to_string(file).expect("Couldn't read source code file");
        //compile::compile(&contents[..]); // maybe create compilationcontext to pass here?
    }*/
    //
    //let mut lex = Token::lexer(
}

/*fn main_inner() {
}*/
