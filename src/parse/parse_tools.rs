use crate::helper::lex_wrap::{TokenWrapper, LookaheadHandle};

use super::Parser;

// During any recursive "rule", if we encounter
// a local parsing error we want to know what to synchronize to.
//
// A linked stack is formed at each stage, with each
// rule pushing a list of "next" items that it can try
// to consume. 
pub struct RuleContext {
}

#[derive(Clone, Copy)]
pub struct LexerStreamHandle {
    index: usize,
    id: usize,
}

pub struct ParseValueGuard<'tokens, ParseValue> {
    value: ParseValue,
    handle: LookaheadHandle<'tokens>,
}

impl<'tokens, T> ParseValueGuard<'tokens, T> {
    pub fn success<E>(value: T, handle: LookaheadHandle<'tokens>) -> Result<ParseValueGuard<'tokens, T>, E> {
        Ok(ParseValueGuard { value, handle })
    }

    pub fn open(self) -> (LookaheadHandle<'tokens>, T) {
        (self.handle, self.value)
    }
}

impl<'lexer> Parser<'lexer> {
}

pub type ParseResult<'t, T, E> = Result<ParseValueGuard<'t, T>, E>;
