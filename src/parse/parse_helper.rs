use crate::lex::Token;

use crate::helper::lex_wrap::TokenWrapper;
use crate::helper::lex_wrap::ParseResultError;
use std::collections::HashSet;
use crate::parse::*;

impl<'b, 'a> Parser<'b, 'a> {
    pub fn eat_through(&mut self, toks: Vec<Token>) {
        let s: HashSet<Token> = toks.into_iter().collect();

        while let Ok(tw) = self.lex.next() {
            if s.contains(&tw.token) {
                break;
            } else {
                continue;
            }
        }
    }

    pub fn eat_to(&mut self, toks: Vec<Token>) {
        let s: HashSet<Token> = toks.into_iter().collect();

        while let Ok(tw) = self.lex.la(0) {
            if s.contains(&tw.token) {
                break;
            } else {
                self.lex.advance();
                continue;
            }
        }
    }

    pub fn eat_match(&mut self, t: Token) -> Option<TokenWrapper<'a>> {
        self.expect(t).ok()
        //expect(t).map_or(|t| Some(t), None)
    }

    pub fn eat_match_in(&mut self, t: &[Token]) -> Option<TokenWrapper<'a>> {
        if let Ok(tw) = self.lex.la(0) {
            if t.contains(&tw.token) {
                self.lex.advance();

                Some(tw)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn eat_if<F, T>(&mut self, f: F) -> Option<(T, TokenWrapper<'a>)>
        where F: FnOnce(TokenWrapper<'a>) -> Option<T>
    {
        match self.lex.la(0) {
            Ok(tw) => {
                let result_f = f(tw);
                let result = match result_f {
                    Some(r) => Some((r, tw)),
                    None => None,
                };
                if result.is_some() {
                    self.lex.advance();
                }

                result
            },
            Err(_) => None
        }
    }

    pub fn expect(&mut self, t: Token) -> Result<TokenWrapper<'a>, ParseResultError<'a>> {
        if let Ok(tw) = self.lex.next() {
            match tw.token {
                tt if tt == t => Ok(tw),
                _ => {
                    self.lex.backtrack();

                    Err(ParseResultError::UnexpectedToken(tw, vec![t]))
                },
            }
        } else {
            Err(ParseResultError::EndOfFile)
        }
    }
}


pub struct RunConditional<'a> {
    pub run_if: Option<TokenWrapper<'a>>,
}
