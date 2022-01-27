use smallvec::SmallVec;

use crate::helper::lex_wrap::LookaheadHandle;
use crate::lex::Token;

use crate::helper::lex_wrap::ParseResultError;
use crate::helper::lex_wrap::TokenWrapper;
use crate::parse::*;

use super::parse_tools::LexerStreamHandle;
use super::parse_tools::ParseValueGuard;
//use std::collections::HashSet;

pub trait ResultHint {
    fn hint(self, hint: &'static str) -> Self;
}

impl<T> ResultHint for Result<T, ParseResultError> {
    fn hint(self, hint: &'static str) -> Self {
        self.map_err(|pre| ParseResultError::ErrorWithHint {
            hint,
            original: Box::new(pre),
        })
    }
}

impl<'lexer> Parser<'lexer> {

}

pub struct RunConditional {
    pub run_if: Option<TokenWrapper>,
}
