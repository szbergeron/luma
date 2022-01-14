use crate::helper::lex_wrap::TokenWrapper;

use super::Parser;

pub struct LexerStreamHandle {
    index: usize,
    id: usize,
}

impl LexerStreamHandle {
    pub fn split(&self) -> Self {
        Self { id: self.id + 1, index: self.index }
    }
}

pub struct ParseValueGuard<ParseValue> {
    value: ParseValue,
    handle: LexerStreamHandle,
}

impl<T> ParseValueGuard<T> {
    pub fn success<E>(value: T, handle: LexerStreamHandle) -> Result<ParseValueGuard<T>, E> {
        Ok(ParseValueGuard { value, handle })
    }

    pub fn extract(self, token: LexerStreamHandle) -> (LexerStreamHandle, T) {
        if token.index > self.handle.index {
            panic!("Extract was given a 'newer' token, this is a compiler bug");
        }

        (self.handle, self.value)
    }
}

impl<'lexer> Parser<'lexer> {
}

pub type ParseResult<T, E> = Result<ParseValueGuard<T>, E>;
