use crate::lex::{TokenWrapper, ParseResultError};

use crate::parse::*;



//use std::collections::HashSet;

impl<'lexer> Parser<'lexer> {

}

pub struct RunConditional {
    pub run_if: Option<TokenWrapper>,
}
