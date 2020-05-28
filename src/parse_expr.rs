use crate::ast;
use crate::lex::Token;
use crate::helper::lex_wrap::Wrapper;
use crate::helper::lex_wrap::TokenWrapper;
use crate::helper::lex_wrap::ParseResultError;
use crate::helper::lex_wrap::LookaheadStream;
use std::collections::HashSet;

use crate::parse::*;

use crate::parse_helper::*;

type ExpressionResult<'a> = Result<Box<ast::Expression<'a>>, ParseResultError<'a>>;

/*pub fn variable_access<'a>(la: &mut LookaheadStream<'a>) -> ExpressionResult<'a> {
}

pub fn atomic_expression<'a>(la: &mut LookaheadStream<'a>) -> ExpressionResult<'a> {
}*/

/*pub fn parse_expr<'a>(la: &mut LookaheadStream<'a>) -> ExpressionResult<'a> {
}*/

pub fn parse_expr<'a>(la: &mut LookaheadStream<'a>) -> ExpressionResult<'a> {
    let mut lhs = atomic_expression(la)?;

    while let Ok(tw) = la.next() {
        match tw.token {
            Token::Dot => {
                let access = object_access(la)?;
                match access {
                    //Field(name, span) => ast::Expression::
                }
            }
        }
    }

    panic!()
}

enum ObjectAccess<'a> {
    Field(&'a str),
    Method(&'a str, Vec<Box<ast::Expression<'a>>>),
}

pub fn object_access<'a>(la: &mut LookaheadStream<'a>) -> Result<ObjectAccess<'a>, ParseResultError<'a>> {
    panic!()
}

pub fn atomic_expression<'a>(la: &mut LookaheadStream<'a>) -> ExpressionResult<'a> {
    if let Ok(tw) = la.next() {
        match tw.token {
            Token::Identifier => {
                Ok(Box::new(ast::Expression::identifier(tw.slice)))
            },
            Token::UnknownIntegerLiteral => {
                Ok(Box::new(ast::Expression::int_literal(tw.slice)))
            },
            Token::LParen => {
                let inner = parse_expr(la)?;
                expect(la, Token::RParen)?;

                Ok(inner)
            },
            _ => {
                Err(ParseResultError::UnexpectedToken(tw))
            }
        }
    } else {
        Err(ParseResultError::EndOfFile)
    }
}

pub struct LALRPopLexWrapper<'a, 'b> {
    pub la: &'b mut LookaheadStream<'a>,
    pub end_with: Vec<Token>, // use array instead of set as n will almost never be above 3, and often will be just 1
}

impl<'a, 'b> LALRPopLexWrapper<'a, 'b> {
    pub fn new(la: &'b mut LookaheadStream<'a>, end_with: Vec<Token>) -> LALRPopLexWrapper<'a, 'b> {
        LALRPopLexWrapper {
            la, end_with
        }
    }
}

impl<'a, 'b> Iterator for LALRPopLexWrapper<'a, 'b> {
    type Item = Result<(usize, LALRPopToken<'a>, usize), ParseResultError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Ok(tw) = self.la.la(0) {
            return match tw.token {
                Token::Lambda => {
                    let c = closure(&mut self.la);
                    match c {
                        Ok(c) => {
                            let start = c.start;
                            let end = c.end;
                            Some(Ok((start, LALRPopToken::Closure(c), end)))
                        },
                        Err(e) => {
                            Some(Err(e))
                        }
                    }
                },
                other => {
                    println!("LALRPopLexWrapper got other of {:?}", other);
                    if self.end_with.contains(&other) {
                        println!("LALRPopLexWrapper returns None with other {:?}", other);
                        None
                    } else {
                        println!("LALRPopLexWrapper returns Some with ends_with {:?} and other {:?}", self.end_with, other);
                        self.la.next();
                        //Some((0, to_lp_token(tw), 0))
                        Some(Self::to_lp_token(tw))
                    }
                }
            }
        }

        None
    }
}

impl<'a, 'b> LALRPopLexWrapper<'a, 'b> {
    fn to_lp_token(tw: TokenWrapper<'a>) -> Result<(usize, LALRPopToken<'a>, usize), ParseResultError<'a>> {
        let start = tw.start;
        let end = tw.end;
        let lpt = match tw.token {
            Token::Public => LALRPopToken::Public,
            Token::If => LALRPopToken::If,
            Token::As => LALRPopToken::As,
            Token::Else => LALRPopToken::Else,
            Token::For => LALRPopToken::For,
            Token::While => LALRPopToken::While,
            Token::Semicolon => LALRPopToken::Semicolon,
            Token::RBrace => LALRPopToken::RBrace,
            Token::LBrace => LALRPopToken::LBrace,
            Token::RBracket => LALRPopToken::RBracket,
            Token::LBracket => LALRPopToken::LBracket,
            Token::RParen => LALRPopToken::RParen,
            Token::LParen => LALRPopToken::LParen,
            Token::Asterisk => LALRPopToken::Asterisk,
            Token::FSlash => LALRPopToken::FSlash,
            Token::Dash => LALRPopToken::Dash,
            Token::Plus => LALRPopToken::Plus,
            Token::Equals => LALRPopToken::Equals,
            Token::CmpEqual => LALRPopToken::CmpEqual,
            Token::CmpLessThan => LALRPopToken::CmpLessThan,
            Token::CmpGreaterThan => LALRPopToken::CmpGreaterThan,
            Token::CmpLessThanOrEqual => LALRPopToken::CmpLessThanOrEqual,
            Token::CmpGreaterThanOrEqual => LALRPopToken::CmpGreaterThanOrEqual,
            Token::QueryAssign => LALRPopToken::QueryAssign,
            Token::Bang => LALRPopToken::Bang,
            Token::Pipe => LALRPopToken::Pipe,
            Token::Dot => LALRPopToken::Dot,
            Token::Identifier => LALRPopToken::Identifier(tw.slice),
            Token::UnknownIntegerLiteral => LALRPopToken::UnknownIntegerLiteral(tw.slice),
            _ => return Err(ParseResultError::UnexpectedToken(tw)),
        };

        Ok((start, lpt, end))
    }
}

#[derive(Debug, Clone)]
pub enum LALRPopToken<'a> {
    Public,
    If,
    As,
    Else,
    For,
    While,
    Semicolon,
    RBrace,
    LBrace,
    RBracket,
    LBracket,
    RParen,
    LParen,
    Asterisk,
    FSlash,
    Dash,
    Plus,
    Equals,
    CmpEqual,
    CmpLessThan,
    CmpGreaterThan,
    CmpLessThanOrEqual,
    CmpGreaterThanOrEqual,
    QueryAssign,
    Bang,
    Pipe,
    Dot,
    Identifier(&'a str),
    UnknownIntegerLiteral(&'a str),
    Closure(ast::Closure<'a>),
}


/*pub enum Node<'a> {
    Terminal(Token),
    NonTerminal(&'a str),
}

pub struct Rule<'a> {
    from: &'a str,
    expands: Vec<Node<'a>>,
    on_recognize: Option<Box<dyn Fn(&mut LookaheadStream<'a>) -> Box<dyn ast::Expression<'a>>>>,
}

pub struct LRParser {
}

impl LRParser {
    pub fn new() -> LRParser {
        LRParser {}
    }

    //pub fn rule(
}*/
// am going to probably do pratt parsing instead,
// since trying to maintain an inline parser generator is
// going to be a massive headache if I actually do this
//
// what follows is a recursive ascent parser for this
