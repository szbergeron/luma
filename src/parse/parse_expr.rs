use crate::ast;
use crate::lex::Token;

use crate::helper::lex_wrap::TokenWrapper;
use crate::helper::lex_wrap::ParseResultError;
use crate::helper::lex_wrap::LookaheadStream;
use crate::helper::EitherAnd;
use std::collections::HashSet;

use crate::parse::*;

//use crate::parse_helper::*;

use ast::expressions::*;
use ast::base::*;
use ast::outer::*;

type ExpressionResult<'a> = Result<Box<ast::ExpressionWrapper<'a>>, ParseResultError<'a>>;

/*pub fn variable_access<'a>(la: &mut LookaheadStream<'a>) -> ExpressionResult<'a> {
}

pub fn atomic_expression<'a>(la: &mut LookaheadStream<'a>) -> ExpressionResult<'a> {
}*/

/*pub fn parse_expr<'a>(la: &mut LookaheadStream<'a>) -> ExpressionResult<'a> {
}*/

impl<'a, 'b> Parser<'a, 'b> {
    pub fn parse_expr(&mut self) -> ExpressionResult<'a> {
        //println!("parse expr called");
        //let mut lhs = access_expression(la)?;
        let r = self.parse_expr_inner(0, 1);

        //println!("Parse_expr produces {:?}", r);

        r
    }

    pub fn parse_pattern(&mut self) -> Result<Pattern<'a>, ParseResultError<'a>> {
        // can be a single literal or tuple, and each tuple is a set of expressions
        //println!("parsing a pattern");

        let lp = self.expect(Token::LParen);

        let start = lp?.start;

        let mut expressions = Vec::new();

        while let Ok(expr) = self.parse_expr() {
            expressions.push(expr);

            match self.eat_match(Token::Comma) {
                Some(_comma) => continue,
                None => break,
            }
        }

        let end = self.expect(Token::RParen)?.end;

        let node_info = NodeInfo::from_indices(true, start, end);

        Ok(Pattern { node_info, expressions })

        //Ok(Pattern::new_expr(node_info, exprs))
    }

    pub fn parse_array_literal(&mut self) -> ExpressionResult<'a> {
        todo!()
    }

    pub fn scoped_name(&mut self) -> Box<ScopedName<'a>> {
        //println!("parsing a scoped name");
        let mut r = Box::new(ScopedName { scope: Vec::new(), silent: true, node_info: NodeInfo::Builtin });

        let mut start = None;
        let mut end = None;

        match self.eat_match(Token::DoubleColon) {
            Some(dc) => {
                //println!("scoped_name ate a doublecolon");
                r.scope.push("global");
                r.silent = false;
                start = Some(dc.start);
                end = Some(dc.end);
            },
            None => r.scope.push("here"),
        }

        while let Some(id) = self.eat_match(Token::Identifier) {
            //println!("scoped_name eats an id: {}", id.slice);
            r.scope.push(id.slice);
            r.silent = false;

            start = Some(start.unwrap_or(id.start));
            end = Some(id.end);

            match self.eat_match(Token::DoubleColon) {
                None => break,
                Some(dc) => {
                    end = Some(dc.end);
                    continue;
                },
            }
        }

        match start {
            Some(start) => {
                // can't get start without also getting end
                let end = end.unwrap_or(start);
                r.node_info = NodeInfo::from_indices(true, start, end);
            },
            None => {},
        }

        r
    }

    pub fn atomic_expression(&mut self) -> ExpressionResult<'a> {
        if let Ok(tw) = self.lex.next() {
            match tw.token {
                Token::UnknownIntegerLiteral => {
                    Ok(ast::ExpressionWrapper::literal_expression(tw))
                },
                Token::StringLiteral => {
                    Ok(ast::ExpressionWrapper::literal_expression(tw))
                },
                Token::Underscore => {
                    Ok(ast::ExpressionWrapper::wildcard(tw))
                },
                Token::LParen | Token::DoubleColon | Token::Identifier => {
                    self.lex.backtrack();
                    self.parse_access_base(None)
                },
                _ => {
                    Err(ParseResultError::UnexpectedToken(tw,
                        vec![
                            Token::UnknownIntegerLiteral,
                            Token::StringLiteral,
                            Token::Underscore,
                            Token::LParen,
                            Token::DoubleColon,
                            Token::Identifier]))
                }
            }
        } else {
            self.err(ParseResultError::EndOfFile)
        }
    }

    pub fn parse_access_trailing(&mut self, on: Box<ExpressionWrapper<'a>>) -> ExpressionResult<'a> {
        let p = self.parse_pattern()?;
        let start = on.as_node().start().unwrap_or(0);

        let node_info = NodeInfo::from_indices(true, start, p.end().unwrap_or(start));

        let scope = ScopedName {
            silent: true,
            scope: Vec::new(),
            node_info: NodeInfo::Builtin,
        };

        let ae = AccessExpression {
            node_info,
            pattern: Some(p),
            on: Some(on),
            scope: Box::new(scope),
        };

        Ok(Box::new(ExpressionWrapper::Access(ae)))
    }

    pub fn parse_access_base(&mut self, on: Option<Box<ExpressionWrapper<'a>>>) -> ExpressionResult<'a> {
        //println!("parse_access called with lookahead {:?}", self.lex.la(0));
        /*
         * Follows pattern:
         *     Namespace1::NamespaceN::Access &| (Pattern) . Repeat_Chain
         *
         *     which translates to
         *
         *     scoped_name Pattern? (.parse_chain)?
         *
         *     this has a special requirement that the rule as a whole is not null deriving,
         *
         *     so either the scoped_name.scope has a nonzero length or the pattern exists
         */

        // first access has no specified "self" unless it is an object itself.

        let mut either: EitherAnd<Span, Span> = EitherAnd::Neither;

        let base = self.scoped_name();

        match base.node_info.as_parsed() {
            Some(pni) => {
                either = either.with_a(pni.span);
            },
            _ => {},
        }

        let b_pattern = match base.silent {
            true => {
                Some(self.parse_pattern()?)
            },
            false => {
                self.parse_pattern().ok()
            },
        };

        match &b_pattern {
            Some(p) => {
                match p.node_info().as_parsed() {
                    Some(pni) => {
                        either = either.with_b(pni.span);
                    },
                    _ => {},
                }
            },
            _ => {},
        }

        // invisible invariant: either base or b_pattern has to be Some, or both

        let (start, end) = match either {
            EitherAnd::Neither => panic!("Somehow got neither a pattern nor a base"),
            EitherAnd::A(a) => (a.start, a.end),
            EitherAnd::B(b) => (b.start, b.end),
            EitherAnd::Both(a, b) => (a.start, b.end),
        };

        let node_info = NodeInfo::from_indices(true, start, end);

        let ae = AccessExpression {
            node_info,
            on,
            scope: base,
            pattern: b_pattern,
        };

        Ok(Box::new(ExpressionWrapper::Access(ae)))
    }

    pub fn parse_if_then_else(&mut self) -> ExpressionResult<'a> {
        let start = self.expect(Token::If)?.start;
        let if_exp = self.parse_expr()?;
        let then_exp = self.parse_expr()?;
        let (else_exp, end) = if self.eat_match(Token::Else).is_some() {
            let exp = self.parse_expr()?;
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

    pub fn syntactic_block(&mut self) -> ExpressionResult<'a> {
        self.expect(Token::LBrace)?;
        let mut declarations: Vec<Result<Box<ast::ExpressionWrapper<'a>>, ParseResultError<'a>>> = Vec::new();
        let start = self.lex.la(0).map_or(0, |tw| tw.start);

        let mut failed = false;

        loop {
            match self.lex.la(0)?.token {
                Token::RBrace => {
                    break;
                },
                Token::Semicolon => {
                    self.lex.advance();
                    // empty
                },
                Token::Let => {
                    let r = self.variable_declaration();

                    let exp = r
                        .map(|vd| Box::new(ast::ExpressionWrapper::LetExpression(vd)))
                        .map_err(|e| { 
                            self.report_err(e.clone());

                            failed = true;
                            e
                        });

                    declarations.push(exp);
                },
                _ => {
                    let e = self.parse_expr();

                    let final_exp = e.map(|exp| {
                        match self.eat_match(Token::Semicolon) {
                            Some(semi) => {
                                let start = exp.as_node().start().map_or(0, |v| v);
                                let end = semi.end;
                                let node_info = ast::NodeInfo::from_indices(true, start, end);
                                ast::StatementExpression::new_expr(node_info, exp)
                            },
                            None => exp,
                        }
                    }).map_err(|err| {
                        self.report_err(err.clone());

                        failed = true;
                        self.eat_to(vec![Token::Semicolon, Token::RBrace]);
                        err
                    });

                    declarations.push(final_exp);

                },
            }
        }

        let end = self.lex.la(-1).map_or(start, |tw| tw.start);

        println!("Parsed a syntactic block, errors is {}", failed);

        let node_info = ast::NodeInfo::from_indices(!failed, start, end);


        self.expect(Token::RBrace)?;

        Ok(ast::BlockExpression::new_expr(node_info, declarations))
    }

    pub fn parse_expr_inner(&mut self, min_bp: u32, level: usize) -> ExpressionResult<'a> {
        let t1 = self.lex.la(0)?;
        //println!("{}parse_expr_inner called, current lookahead token is {:?}, {}", indent(level), t1.token, t1.slice);
        let mut lhs = match t1.token {
            Token::LBracket => {
                let r = self.parse_array_literal();

                r?
            },
            Token::LBrace => {
                let r = self.syntactic_block();

                r?
            },
            Token::If => {
                let r = self.parse_if_then_else();

                r?
            },
            t if prefix_binding_power(t).is_some() => {
                self.lex.advance();

                let bp = prefix_binding_power(t).expect("bp should already be Some from match guard");
                let rhs = self.parse_expr_inner(bp, level + 1)?;
                let start = t1.start;
                let end = rhs.as_node().end().expect("parsed rhs has no end?");
                let node_info = ast::NodeInfo::from_indices(true, start, end);

                self.build_unary(node_info, t, rhs)
            },
            //
            _other => {
                self.atomic_expression()?
            },
        };

        //println!("{}parse_expr_inner got lhs of {:?}", indent(level), lhs);
        //println!();

        loop {
            let operator = self.eat_if(|t| infix_binding_power(t.token));
            //println!("{}consumes operator {:?}", indent(level), operator);
            if let Some(((l_bp, r_bp), tw)) = operator {
                if l_bp < min_bp {
                    self.lex.backtrack();
                    //println!("{}binding power too weak, breaks", indent(level));
                    break;
                } else {
                    //println!("{}asking for an rhs", indent(level));
                    let rhs = self.parse_expr_inner(r_bp, level + 1)?;
                    let start = lhs.as_node().start().expect("parsed lhs has no start?");
                    let end = rhs.as_node().end().expect("parsed rhs has no end?");
                    let node_info = ast::NodeInfo::from_indices(true, start, end);
                    lhs = self.build_binary(node_info, tw.token, lhs, rhs)?;
                    continue;
                }
            } else if let Some(tw) = self.eat_match_in(&[Token::LParen, Token::LBracket, Token::Identifier, Token::DoubleColon]) {
                self.lex.backtrack(); // want to know it's there, but not consume it
                // not a binary operator per-se, could be chained access?

                lhs = self.parse_access_trailing(lhs)?;

                continue;
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    fn build_unary(&mut self, node_info: NodeInfo, t: Token, lhs: Box<ast::ExpressionWrapper<'a>>)
        -> Box<ast::ExpressionWrapper<'a>> {

        match t {
            Token::And | Token::Asterisk | Token::Dash | Token::Bang => {
                UnaryOperationExpression::new_expr(node_info, t, lhs)
            },
            _ => {
                println!("got unexpected token {:?}", t);
                panic!("Programming error: no way to build unary expression from given token");
            }
        }
    }

    fn build_binary(&mut self, node_info: NodeInfo, t: Token, lhs: Box<ast::ExpressionWrapper<'a>>, rhs: Box<ast::ExpressionWrapper<'a>>)
        //-> Box<ast::ExpressionWrapper<'a>> {
        -> ExpressionResult<'a> {


        match t {
            Token::Plus | Token::Dash | Token::Asterisk | Token::FSlash => {
                Ok(BinaryOperationExpression::new_expr(node_info, t, lhs, rhs))
            },
            Token::As => {
                Ok(CastExpression::new_expr(node_info, lhs, rhs))
            },
            Token::Equals => {
                Ok(AssignmentExpression::new_expr(node_info, lhs, rhs))
            },
            Token::Dot => {
                //println!("lhs and rhs are: {:?} DOT {:?}", lhs, rhs);
                let rhs = match *rhs {
                    ExpressionWrapper::Access(mut ae) => {
                        ae.on = Some(lhs);

                        Box::new(ExpressionWrapper::Access(ae))
                    },
                    _ => {
                        let err = ParseResultError::SemanticIssue(
                        "Can not perform a dot-access with non-access RHS.
                        RHS must be of the form (ScopedIdentifier &| Pattern).
                        Offending RHS occurs at span", rhs.as_node().start().unwrap_or(0), rhs.as_node().end().unwrap_or(0));

                        //self.errors.push(err);

                        //return Err(err);

                        return self.err(err);
                    }
                };

                Ok(rhs)
            },
            _ => {
                println!("got unexpected token {:?}", t);
                panic!("Programming error: no way to build binary expression from given token");
            },
        }
    }
}


/*pub fn parse_slice<'a>(la: &mut LookaheadStream<'a>, on: Option<Box<ExpressionWrapper<'a>>>) -> 
    Result<EitherAnd<ExpressionWrapper<'a>, ExpressionWrapper<'a>>, ParseResultError<'a>> {
    todo!()
}*/


// this rule can be null deriving, but can also have a syntax error, so we use a Result<Option, _>
/*pub fn parse_tuple_literal<'a>(la: &mut LookaheadStream<'a>) -> Result<Option<Pattern<'a>>>, ParseResultError<'a>> {
}*/

// scopedname can null derive so no parse error can occur



pub enum DotAccess<'a> {
    Field(&'a str),
    Method(&'a str, Vec<Box<ast::ExpressionWrapper<'a>>>),
}

use ast::IntoAstNode;

/*pub fn object_access<'a>(la: &mut LookaheadStream<'a>, innerexp: Box<ast::ExpressionWrapper<'a>>) -> ExpressionResult<'a> {
    let _dot = expect(la, Token::Dot)?;

    let name = expect(la, Token::Identifier)?;

    match eat_match(la, Token::LParen) {
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
}*/

/*pub fn access_expression<'a>(la: &mut LookaheadStream<'a>) -> ExpressionResult<'a> {
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
}*/

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

        Token::Dot 
            => Some((250, 250)),

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
