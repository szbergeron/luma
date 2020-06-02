use crate::ast;
use crate::lex::Token;

use crate::helper::lex_wrap::TokenWrapper;
use crate::helper::lex_wrap::ParseResultError;
use crate::helper::lex_wrap::LookaheadStream;
use std::collections::HashSet;

use crate::parse::*;

use crate::parse_helper::*;

use ast::expressions::*;
use ast::base::*;

type ExpressionResult<'a> = Result<Box<ast::ExpressionWrapper<'a>>, ParseResultError<'a>>;

/*pub fn variable_access<'a>(la: &mut LookaheadStream<'a>) -> ExpressionResult<'a> {
}

pub fn atomic_expression<'a>(la: &mut LookaheadStream<'a>) -> ExpressionResult<'a> {
}*/

/*pub fn parse_expr<'a>(la: &mut LookaheadStream<'a>) -> ExpressionResult<'a> {
}*/

pub fn parse_expr<'a>(la: &mut LookaheadStream<'a>) -> ExpressionResult<'a> {
    println!("parse expr called");
    //let mut lhs = access_expression(la)?;
    let r = parse_expr_inner(la, 0, 1);

    println!("Parse_expr produces {:?}", r);

    r

    /*while let Ok(tw) = la.la(0) {
        match tw.token {
            /*Token::Dot => {
                let access = object_access(la, lhs)?;
                match access {
                    //Field(name, span) => ast::Expression::
                    _ => todo!(),
                }
            }*/
            _ => todo!(),
        }
    }*/
}

pub fn parse_if_then_else<'a>(la: &mut LookaheadStream<'a>) -> ExpressionResult<'a> {
    let start = expect(la, Token::If)?.start;
    let if_exp = parse_expr(la)?;
    let then_exp = parse_expr(la)?;
    let (else_exp, end) = if eat_if_matches(la, Token::Else).is_some() {
        let exp = parse_expr(la)?;
        let end = exp.as_node().end().expect("successfully parsed else had no end");

        (exp, end)

    } else {
        let exp = BlockExpression::new_expr(ast::NodeInfo::Builtin, Vec::new());
        let end = then_exp.as_node().end().expect("then had no end?");

        (exp, end)

    };

    let node_info = ast::NodeInfo::from_indices(true, start, end);

    Ok(IfThenElseExpression::new_expr(node_info, if_exp, then_exp, else_exp))
}

pub fn syntactic_block<'a>(lexer: &mut LookaheadStream<'a>) -> ExpressionResult<'a> {
    expect(lexer, Token::LBrace)?;
    let mut declarations: Vec<Result<Box<ast::ExpressionWrapper<'a>>, ParseResultError<'a>>> = Vec::new();
    let start = lexer.la(0).map_or(0, |tw| tw.start);

    let mut failed = false;

    loop {
        match lexer.la(0)?.token {
            Token::RBrace => {
                break;
            },
            Token::Semicolon => {
                // empty
            },
            _ => {
                let e = parse_expr(lexer);

                let final_exp = e.map(|exp| {
                    match eat_if_matches(lexer, Token::Semicolon) {
                        Some(semi) => {
                            let start = exp.as_node().start().map_or(0, |v| v);
                            let end = semi.end;
                            let node_info = ast::NodeInfo::from_indices(true, start, end);
                            ast::StatementExpression::new_expr(node_info, exp)
                        },
                        None => exp,
                    }
                }).map_err(|err| {
                    failed = true;
                    eat_to(lexer, vec![Token::Semicolon, Token::RBrace]);
                    err
                });

                declarations.push(final_exp);

            },
        }
    }

    let end = lexer.la(-1).map_or(start, |tw| tw.start);

    let node_info = ast::NodeInfo::from_indices(failed, start, end);

    expect(lexer, Token::RBrace)?;

    Ok(ast::BlockExpression::new_expr(node_info, declarations))
}

pub fn parse_expr_inner<'a>(la: &mut LookaheadStream<'a>, min_bp: u32, level: usize) -> ExpressionResult<'a> {
    let t1 = la.la(0)?;
    println!("{}parse_expr_inner called, current lookahead token is {:?}, {}", indent(level), t1.token, t1.slice);
    let mut lhs = match t1.token {
        Token::LParen => {
            expect(la, Token::LParen)?;
            let r = parse_expr_inner(la, 0, level + 1);
            expect(la, Token::RParen)?;

            r?
        },
        Token::LBrace => {
            let r = syntactic_block(la);

            r?
        },
        Token::If => {
            let r = parse_if_then_else(la);

            r?
        },
        t if prefix_binding_power(t).is_some() => {
            la.advance();

            let bp = prefix_binding_power(t).expect("bp should already be Some from match guard");
            let rhs = parse_expr_inner(la, bp, level + 1)?;
            let start = t1.start;
            let end = rhs.as_node().end().expect("parsed rhs has no end?");
            let node_info = ast::NodeInfo::from_indices(true, start, end);

            build_unary(node_info, t, rhs)
        },
        //
        _other => {
            access_expression(la)?
        },
    };

    println!("{}parse_expr_inner got lhs of {:?}", indent(level), lhs);
    println!();

    loop {
        //let operator = la.la(0)?;
        let operator = eat_if(la, |t| infix_binding_power(t.token));
        println!("{}consumes operator {:?}", indent(level), operator);
        //if let Some(bp) = post
        if let Some(((l_bp, r_bp), tw)) = operator {
            if l_bp < min_bp {
                la.backtrack();
                println!("{}binding power too weak, breaks", indent(level));
                break;
            } else {
                println!("{}asking for an rhs", indent(level));
                let rhs = parse_expr_inner(la, r_bp, level + 1)?;
                let start = lhs.as_node().start().expect("parsed lhs has no start?");
                let end = rhs.as_node().end().expect("parsed rhs has no end?");
                let node_info = ast::NodeInfo::from_indices(true, start, end);
                lhs = build_binary(node_info, tw.token, lhs, rhs);
                continue;
            }
        } else {
            break;
        }
    }

    Ok(lhs)
}

fn build_unary<'a>(node_info: NodeInfo, t: Token, lhs: Box<ast::ExpressionWrapper<'a>>)
    -> Box<ast::ExpressionWrapper<'a>> {

    match t {
        Token::And | Token::Asterisk | Token::Dash | Token::Bang => {
            UnaryOperationExpression::new_expr(node_info, t, lhs)
        },
        _ => {
            println!("got unexpected token {:?}", t);
            panic!("Programming error: no way to build binary expression from given token");
        }
    }
}

fn build_binary<'a>(node_info: NodeInfo, t: Token, lhs: Box<ast::ExpressionWrapper<'a>>, rhs: Box<ast::ExpressionWrapper<'a>>)
    -> Box<ast::ExpressionWrapper<'a>> {


    match t {
        Token::Plus | Token::Dash | Token::Asterisk | Token::FSlash => {
            BinaryOperationExpression::new_expr(node_info, t, lhs, rhs)
        },
        Token::As => {
            CastExpression::new_expr(node_info, lhs, rhs)
        },
        Token::Equals => {
            AssignmentExpression::new_expr(node_info, lhs, rhs)
        },
        _ => {
            println!("got unexpected token {:?}", t);
            panic!("Programming error: no way to build binary expression from given token");
        },
    }
}

pub enum DotAccess<'a> {
    Field(&'a str),
    Method(&'a str, Vec<Box<ast::ExpressionWrapper<'a>>>),
}

use ast::IntoAstNode;

pub fn object_access<'a>(la: &mut LookaheadStream<'a>, innerexp: Box<ast::ExpressionWrapper<'a>>) -> ExpressionResult<'a> {
    let _dot = expect(la, Token::Dot)?;

    let name = expect(la, Token::Identifier)?;

    match eat_if_matches(la, Token::LParen) {
        Some(_) => {
            todo!()
        },
        None => {
            let start = innerexp.as_node().start().expect("object_access was given an improperly parsed innerexp");
            let end = name.end;
            let field = name.slice;
            let node_info = ast::NodeInfo::from_indices(true, start, end);
            //Ok(DotAccess::Field(name.slice))
            //Ok(Box::new(ast::FieldAccess { node_info, field, on: innerexp }))
            Ok(ast::FieldAccess::new_expr(node_info, field, innerexp))
        }
    }
}

pub fn access_expression<'a>(la: &mut LookaheadStream<'a>) -> ExpressionResult<'a> {
    let mut base = atomic_expression(la);
    //println!("atomic expr produces base of {:?}", base);
    //println!();
    let mut base = base?;
    /*loop {
        
    }*/
    while let Ok(tw) = la.la(0) {
        match tw.token {
            Token::Dot => {
                let access = object_access(la, base)?;
                base = access;
            },
            Token::LBracket => {
                todo!("array access not yet implemented")
            }
            Token::QuestionMark => {
                todo!("err short circuit not yet implemented")
            }
            _ => {
                break;
            }
        }
    }

    /*println!();
    println!("access expression crated from {:?}", base);
    println!();*/

    Ok(base)
}

/*pub fn additive_expression<'a>(la: &mut LookaheadStream<'a>) -> ExpressionResult<'a> {
    //let mut lhs = 
    let mut t = 5;
    let mut to = 6;

    todo!()
}

pub fn assignment_expression<'a>(la: &mut LookaheadStream<'a>) -> ExpressionResult<'a> {
    let mut lhs = access_expression(la)?;

    while let Ok(tw) = la.la(0) {
        match tw.token {
            Token::Equals => {
                la.advance();
                let rhs = assignment_expression(la)?;
                let start = lhs.as_node().start().expect("assignment_expression found malformed lhs that passed Err bubbling");
                let end = rhs.as_node().end().expect("assignment_expression found malformed rhs that passed Err bubbling");
                let node_info = ast::NodeInfo::from_indices(true, start, end);
                let node = ast::AssignmentExpression::new_expr(node_info, lhs, rhs);
                lhs = node;
            },
            Token::Asterisk | Token::FSlash | Token::Plus | Token::Dash => {
            },
            _ => {
                break; // parsed to something that doesn't make sense as part of an expression
            }
        }
    }

    Ok(lhs)
}*/

pub fn atomic_expression<'a>(la: &mut LookaheadStream<'a>) -> ExpressionResult<'a> {
    if let Ok(tw) = la.next() {
        match tw.token {
            Token::Identifier => {
                Ok(Box::new(ast::ExpressionWrapper::identifier_expression(tw)))
            },
            Token::UnknownIntegerLiteral => {
                Ok(Box::new(ast::ExpressionWrapper::integer_literal_expression(tw)))
            },
            Token::LParen => {
                let inner = parse_expr(la)?;
                expect(la, Token::RParen)?;

                Ok(inner)
            },
            // Token::If, Token::Match,
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

pub fn prefix_binding_power(t: Token) -> Option<u32> {
    match t {
        Token::Plus
            | Token::Dash
            | Token::Asterisk
            | Token::Bang
            | Token::And
            => Some(100),

        _ => None,
    }
}

pub fn infix_binding_power(t: Token) -> Option<(u32, u32)> {
    match t {
        Token::As
            => Some((1, 300)),

        Token::Equals
            => Some((200, 2)),

        Token::LogicalOr
            => Some((3, 4)),

        Token::LogicalAnd
            => Some((5, 6)),

        Token::CmpEqual
            | Token::CmpLessThan
            | Token::CmpGreaterThan
            | Token::CmpLessThanOrEqual
            | Token::CmpGreaterThanOrEqual
            | Token::CmpNotEqual
            => Some((7, 8)),

        Token::Pipe
            => Some((9, 10)),

        Token::Caret
            => Some((11, 12)),

        Token::And
            => Some((13, 14)),

        Token::ShiftLeft
            | Token::ShiftRight
            => Some((15, 16)),

        Token::Plus
            | Token::Dash
            => Some((17, 18)),

        Token::Asterisk
            | Token::FSlash
            | Token::Modulo
            => Some((19, 20)),

        _ => None,
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

#[derive(Debug)]
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
