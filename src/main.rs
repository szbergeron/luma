#![feature(generic_associated_types)]
//#[macro_use] extern crate lalrpop_util;

//lalrpop_mod!(pub rsh);

use std::env;
use std::fs;
use logos::Logos;

//mod lib;
pub mod lex;
pub mod helper;
pub mod parse;
pub mod ast;
pub mod lalrpop_lexer;
pub mod parse_expr;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("Args: {:?}", args);
    let files = &args[1..];
    for file in files {
        let contents = fs::read_to_string(file).expect("Couldn't read source code file");
        parse::compile(&contents[..]); // maybe create compilationcontext to pass here?
    }
    //
    //let mut lex = Token::lexer(
}
