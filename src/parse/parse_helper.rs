use crate::lex::{TokenWrapper, ParseResultError};

use crate::parse::*;



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
