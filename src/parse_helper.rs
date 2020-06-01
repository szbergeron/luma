use crate::ast;
use crate::lex::Token;

use crate::helper::lex_wrap::TokenWrapper;
use crate::helper::lex_wrap::ParseResultError;
use crate::helper::lex_wrap::LookaheadStream;
use std::collections::HashSet;

pub fn eat_through<'a>(la: &mut LookaheadStream<'a>, toks: Vec<Token>) {
    let s: HashSet<Token> = toks.into_iter().collect();

    while let Ok(tw) = la.next() {
        if s.contains(&tw.token) {
            break;
        } else {
            continue;
        }
    }
}

pub fn eat_if_matches<'a>(la: &mut LookaheadStream<'a>, t: Token) -> Option<TokenWrapper<'a>> {
    expect(la, t).ok()
    //expect(t).map_or(|t| Some(t), None)
}

pub fn eat_if<'a, F, T>(la: &mut LookaheadStream<'a>, f: F) -> Option<(T, TokenWrapper<'a>)>
    where F: FnOnce(TokenWrapper<'a>) -> Option<T>
{
    match la.la(0) {
        Ok(tw) => {
            let result_f = f(tw);
            let result = match result_f {
                Some(r) => Some((r, tw)),
                None => None,
            };
            if result.is_some() {
                la.advance();
            }

            result
        },
        Err(_) => None
    }
    //expect(la, t).ok()
    //expect(t).map_or(|t| Some(t), None)
}

pub struct RunConditional<'a> {
    pub run_if: Option<TokenWrapper<'a>>,
}

/*pub struct RunResult<'a, Output> {
    parses: Option<TokenWrapper<'a>>,
    result: Option<Output>,
}

impl<'a, Output> RunResult<'a, Output> {
    pub fn then<F>(&self, func: F) -> RunResult<Output> where F: FnOnce(TokenWrapper<'a>) -> Output {
        match self.parses {
            Some(tw) => RunResult { result: Some(func(tw)), parses: self.parses },
            None => RunResult { result: self.result, parses: self.parses },
        }
    }

    pub fn otherwise<F>(&self, func: F) -> RunResult<Output> where F: FnOnce() -> () {
        match self.parses {
            Some(tw) => RunResult { result: self.result, parses: self.parses },
            None => RunResult { result: 
    }
}*/ // this was getting to be probably not useful, likely only need simple then case anyway

/*impl<'a> RunConditional<'a> {
    pub fn then<F, T>(&self, func: F) -> Option<T> where F: FnOnce(TokenWrapper<'a>) -> T {
        if self.run_if.is_some() {
            Some(func(self.run_if))
        } else {
            None
        }
    }
}

fn if_token<'a>(la: &mut LookaheadStream<'a>, t: Token) -> RunConditional<'a> {
    RunConditional { run_if: eat_if(la, t) }
}*/

pub fn expect<'a>(la: &mut LookaheadStream<'a>, t: Token) -> Result<TokenWrapper<'a>, ParseResultError<'a>> {
    println!("Expect asked for: {:?}", t);
    if let Ok(tw) = la.next() {
        match tw.token {
            tt if tt == t => Ok(tw),
            _ => {
                la.backtrack();

                Err(ParseResultError::UnexpectedToken(tw))
            },
        }
    } else {
        Err(ParseResultError::EndOfFile)
    }
}
