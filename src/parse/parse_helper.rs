use smallvec::SmallVec;

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
        self.map_err(|pre| { ParseResultError::ErrorWithHint { hint, original: Box::new(pre) } })
    }
}

impl<'lexer> Parser<'lexer> {
    /// Mark what tokens could feasibly come after some recursive parse state call that the current
    /// parse state wants to capture and handle.
    pub fn sync_next(&mut self, next: &[Token]) -> SyncSliceHandle {
        let start_len = self.next.len();

        self.next.extend(next.iter());

        SyncSliceHandle { start: start_len }
    }

    /// Remove the current recovery frame from the recovery stack,
    /// pass the handle that was provided by sync_next to remove the correct frame
    pub fn unsync(&mut self, handle: SyncSliceHandle) -> Result<(), ParseResultError> {
        if self.next.len() < handle.start {
            // maybe just < rather than <=?
            Err(ParseResultError::InternalParseIssue)
        } else {
            self.next.truncate(handle.start);
            Ok(())
        }
    }

    /// Eat up to, return whether any synchronization action was required
    ///
    /// The synchronization algorithm tries to avoid dropping as much spurious input as possible,
    /// and instead assumes the user has generally not completed typing.
    ///
    /// It will only drop an input if the `next` set does not contain the provided token.
    ///
    /// If the `next` set *does* contain the provided token, then it will
    /// remove any entries in the set more recent than that entry, and
    /// signal to restart parsing in the state that matches that entry by
    /// having unsync(...) only return Ok once that recovery scope is reached
    pub fn synchronize(&mut self) -> bool {
        let mut r = false;
        loop {
            if let Ok(tok) = self.lex.la(0) {
                if let Some(index) = self.next.iter().rposition(|ntok| *ntok == tok.token) {
                    self.next.truncate(index + 1);
                    return r;
                } else {
                    self.lex.advance();
                    r = true;
                }
            } else {
                // EOF
                self.next.clear(); // maintain average capacity by not reallocating
                break;
            }
        }

        return r;
    }

    /// If the current lookahead is the token passed, consume the token and
    /// return its metadata. Otherwise, do nothing and return None
    ///
    /// fast-cased version of eat_match_in for when only one token would be possible
    pub fn eat_match(&mut self, next: LexerStreamHandle, t: Token) -> Option<ParseValueGuard<TokenWrapper>> {
        self.eat_match_in(next, [t])
    }

    /// If the current lookahead is within the [Token] slice passed, consume the token and
    /// return its metadata. Otherwise, do nothing and return None
    pub fn eat_match_in<const LEN: usize>(&mut self, next: LexerStreamHandle, t: [Token; LEN]) -> Option<ParseValueGuard<TokenWrapper>> {
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

    /// Consumes a token if the passed closure returns Some(T),
    /// returning a tuple of the returned T and the token (+metadata) that was consumed
    pub fn eat_if<F, T>(&mut self, f: F) -> Option<(T, TokenWrapper)>
    where
        F: FnOnce(TokenWrapper) -> Option<T>,
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
            }
            Err(_) => None,
        }
    }

    /// Reports an error if next token is not within the [expected] slice
    /// Does not consume the expected token, should be used as an error-reporting
    /// form of eat_to
    pub fn expect_next_in(&mut self, expected: &[Token]) -> Result<(), ParseResultError> {
        let first = self.lex.la(0);
        let sync = self.sync_next(expected);
        let corrected = self.synchronize();

        if corrected {
            println!("corrective action had to be taken");
            if let Ok(tw) = first {
                self.report_err(ParseResultError::UnexpectedToken(
                    tw,
                    expected.to_vec(),
                    None,
                    //expected.iter().cloned().collect(),
                ));
            } else {
                self.report_err(ParseResultError::EndOfFile);
            }
        }

        match self.unsync(sync) {
            Err(e) => {
                // found a synchronization point for a sync point in a superscope, so the current
                // scope's sync point is not usable. Parent scope will not get their expect_next,
                // so return err
                Err(e)
            }
            Ok(_) => {
                // found a synchronization point that allows for parse recovery, so return to
                // parent scope an Ok result
                Ok(())
            }
        }
    }

    /// Will return a failing result if parsing fails, but will not attempt to reallign input or
    /// independently dispatch any error notifications, and will not consume any erroneous input
    pub fn soft_expect(
        &mut self,
        expected: Token,
    ) -> Result<TokenWrapper, ParseResultError> {
        if let Ok(tw) = self.lex.la(0) {
            if tw.token == expected {
                self.lex.advance();
                Ok(tw)
            } else {
                Err(ParseResultError::UnexpectedToken(tw, vec![expected], None))
            }
        } else {
            Err(ParseResultError::EndOfFile)
        }
    }

    /// Behavior similar to soft_expect, but will attempt to reallign input stream to get to the
    /// token
    pub fn hard_expect(
        &mut self,
        expected: Token,
    ) -> Result<TokenWrapper, ParseResultError> {
        self.expect_next_in(&[expected])?;

        if let Ok(tw) = self.lex.la(0) {
            if tw.token == expected {
                self.lex.advance();
                Ok(tw)
            } else {
                Err(ParseResultError::UnexpectedToken(tw, vec![expected], None))
            }
        } else {
            Err(ParseResultError::EndOfFile)
        }
    }

    pub fn eat_match_string<const LEN: usize>(&mut self, expected: [Token; LEN]) -> Result<SmallVec<[TokenWrapper; LEN]>, ParseResultError> {
        let old_idx = self.lex.index();

        let mut sv: SmallVec<[TokenWrapper; LEN]> = SmallVec::new();
        for i in 0..LEN {
            let m = self.eat_match(expected[i]);
            match m {
                Some(tw) => {
                    sv.push(tw);
                }
                None => {
                    self.lex.seek_to(old_idx);
                    return match self.lex.la(0) {
                        Err(e) => Err(e),
                        Ok(tw) => Err(ParseResultError::UnexpectedToken(tw, expected.into(), None))
                    }
                }
            }
        }

        Ok(sv)
    }
}

pub struct RunConditional {
    pub run_if: Option<TokenWrapper>,
}
