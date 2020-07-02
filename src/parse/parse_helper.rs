use crate::lex::Token;

use crate::helper::lex_wrap::ParseResultError;
use crate::helper::lex_wrap::TokenWrapper;
use crate::parse::*;
use std::collections::HashSet;

impl<'input, 'lexer> Parser<'input, 'lexer> {
    pub fn sync_next(
        &mut self,
        next: &[Token],
        ) -> SyncSliceHandle
    {
        let start_len = self.next.len();
        
        self.next.extend(next.iter());

        let end_len = self.next.len();

        println!("sync next called, sync stack is {:?}", self.next);

        SyncSliceHandle { start: start_len, end: end_len }
    }

    pub fn unsync(
        &mut self,
        handle: SyncSliceHandle,
    ) -> Result<(), ParseResultError<'input>> {
        if self.next.len() != handle.end {
            //panic!("wrong handle or unmatched sync handle passed");
            Err(ParseResultError::InternalParseIssue)
        } else {
            self.next.truncate(handle.start);
            Ok(())
        }
    }

    /// eat up to, return whether any synchronization action was required
    pub fn synchronize(
        &mut self
    ) -> bool {
        println!("synchronizing! Current error derivations list is {:?}", self.next);
        println!("looping in sync");

        let mut r = false;
        loop {
            if let Ok(tok) = self.lex.la(0) {
                if let Some(index) = self.next.iter().rposition(|ntok| *ntok == tok.token) {
                    println!("truncates the return list! Found a token: {:?}", tok.token);
                    self.next.truncate(index + 1);
                    return r;
                } else {
                    println!("advances lexer over erronious token {:?}", tok.token);
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

    /*pub fn eat_through(&mut self, toks: Vec<Token>) {
        let s: HashSet<Token> = toks.into_iter().collect();

        while let Ok(tw) = self.lex.next() {
            if s.contains(&tw.token) {
                break;
            } else {
                continue;
            }
        }
    }*/

    /*pub fn eat_to(&mut self, toks: Vec<Token>) {
        let s: HashSet<Token> = toks.into_iter().collect();

        while let Ok(tw) = self.lex.la(0) {
            if s.contains(&tw.token) {
                break;
            } else {
                self.lex.advance();
                continue;
            }
        }
    }*/

    pub fn eat_match(&mut self, t: Token) -> Option<TokenWrapper<'input>> {
        self.eat_match_in(&[t])
        //expect(t).map_or(|t| Some(t), None)
    }

    pub fn eat_match_in(&mut self, t: &[Token]) -> Option<TokenWrapper<'input>> {
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

    pub fn eat_if<F, T>(&mut self, f: F) -> Option<(T, TokenWrapper<'input>)>
    where
        F: FnOnce(TokenWrapper<'input>) -> Option<T>,
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

    pub fn expect(&mut self, expected: Token) -> Result<TokenWrapper<'input>, ParseResultError<'input>> {
        /*self.sync_next(&[t]);
        self.synchronize();
        self.unsync(*/
        if let Ok(tw) = self.lex.la(0) {
            let r = match tw.token {
                token if token == expected => {
                    self.lex.advance();

                    Ok(tw)
                }
                other => {
                    println!("reported an error, token was not of expected type expected {:?} but found {:?}", expected, tw.token);
                    self.report_err(ParseResultError::UnexpectedToken(tw, vec![expected]));

                    // skip over any erronious tokens that don't exist in sync set
                    let sync = self.sync_next(&[expected]);
                    self.synchronize();

                    // if token was meant for an above sync point then don't eat token
                    self.unsync(sync)?;

                    // can only get here if sync stack 
                    if let Ok(tw) = self.lex.la(0) {
                        if tw.token == expected {
                            self.lex.advance();
                            Ok(tw)
                        } else {
                            panic!("Unexpected state: unsynced to current scope, but next token was not expected token");
                        }
                    } else {
                        Err(ParseResultError::EndOfFile)
                    }
                }
            };
            /*let sync = self.sync_next(&[t]);
            self.synchronize();
            self.unsync(sync)?;*/

            /*let r = match tw.token {
                tt if tt == t => Ok(tw),
                _ => {
                    println!("expected a {} but found a {}, so failing", );
                    self.lex.backtrack();

                    self.synchronize();
                    Err(ParseResultError::UnexpectedToken(tw, vec![t]))
                }
            };

            r

            r*/

            r
        } else {
            // no point in synchronizing, EOF already
            Err(ParseResultError::EndOfFile)
        }
    }
}

pub struct RunConditional<'input> {
    pub run_if: Option<TokenWrapper<'input>>,
}
