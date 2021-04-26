use crate::ast;
use crate::lex::Token;

use crate::helper::lex_wrap::{CodeLocation, ParseResultError};
use crate::helper::EitherAnd;

use crate::parse::*;


//use crate::parse_helper::*;

use ast::base::*;
use ast::expressions::*;
use ast::outer::*;

//type LetRes

type ExpressionResult =
    Result<Box<ast::ExpressionWrapper>, ParseResultError>;

impl<'lexer> Parser<'lexer> {
    pub fn parse_expr(&mut self) -> ExpressionResult {
        let r = self.parse_expr_inner(0, 1);

        r
    }

    /// Parses the recursively defined first part of a let statement,
    ///
    /// let <parse_binding()> = <expression>;
    ///
    /// includes support for parsing type specifiers within
    pub fn parse_binding(&mut self) -> Result<Box<LetComponent>, ParseResultError> {
        // want to find if this is going to be a destructuring operation
        self.expect_next_in(&[Token::LParen, Token::Identifier])?;

        let mut end;
        let start;

        // parse body/binding part of let statement
        let content = match self.lex.la(0)?.token {
            Token::LParen => {
                let mut elements: Vec<LetComponent> = Vec::new();
                //todo!("Pattern assignment not yet implemented")
                start = self.hard_expect(Token::LParen)?.start; // consume start lparen

                while let Ok(expr) = self.parse_binding() {
                    elements.push(*expr);

                    match self.eat_match(Token::Comma) {
                        None => break,

                        // consequence of this is that trailing comma is discarded
                        // arity of tuple is not increased in this case, but
                        // these semantics make sense to me
                        //
                        //
                        // TODO: revisit if trailing comma within tuple should
                        // signify increased arity, or if type should
                        // be identical to tuple with current specified elements
                        //
                        // possible way of using this would be "capture
                        // all specified parts of tuple, discard remaining"
                        Some(_comma) => continue,
                    }
                }

                end = self.hard_expect(Token::RParen)?.end; // if user opened, must close

                // see if a type exists, user may specify type anywhere during parsing, but
                // must be possible to resolve types directly for every binding 
                // without needing to infer
                Either::A(elements)
            },
            Token::Identifier => {
                let variable = self.hard_expect(Token::Identifier)?;
                let variable_name = variable.slice;

                start = variable.start;
                end = variable.end;

                Either::B(variable_name)
            }
            _ => {
                unreachable!("expect_next_in forwarded a bad token")
            }
        };

        // if there's a colon, we assume that a type specifier must directly follow it
        //if let Some(_colon) = self.eat_match(Token::
        let specified_type = match self.eat_match(Token::Colon) {
            Some(_colon) => {
                let r = self.parse_type_specifier()?;
                r.end().map(|loc| end = loc);
                Some(r)
            },
            None => None,
        };

        let node_info = NodeInfo::from_indices(start, end);

        match content {
            Either::A(tuple_elements) => {
                Ok(Box::new(LetComponent::Tuple(LetComponentTuple { elements: tuple_elements, node_info, type_specifier: specified_type })))
            },
            Either::B(name) => {
                Ok(Box::new(LetComponent::Identifier(LetComponentIdentifier { identifier_string: name, node_info, type_specifier: specified_type })))
            },
        }
    }

    /// syntax: let <binding>: <type> = <expr>
    pub fn parse_let(&mut self) -> ExpressionResult {
        let start = self.hard_expect(Token::Let)?.start;

        let binding = self.parse_binding()?;

        let _eq = self.hard_expect(Token::Equals)?;

        let expr = self.parse_expr()?;

        // TODO: eval if this could ever be None, potentially force expr to have a
        // CodeLocation::Parsed
        let end = expr.as_node().end().unwrap_or(CodeLocation::Builtin);
        
        let lexpr = LetExpression {
            node_info: NodeInfo::from_indices(start, end),
            primary_component: binding,
            expression: expr };

        //todo!("Let expression parsing not yet complete")
        Ok(Box::new(ExpressionWrapper::LetExpression(lexpr)))
    }

    // follows typespecifier? pattern
    // where:
    //     typespecifier: <typelist>
    //     pattern: (expressionlist)
    pub fn parse_pattern(&mut self) -> Result<Pattern, ParseResultError> {
        // can be a single literal or tuple, and each tuple is a set of expressions

        let lp = self.soft_expect(Token::LParen);

        let start = lp?.start;

        let mut expressions = Vec::new();

        while let Ok(expr) = self.parse_expr() {
            expressions.push(expr);

            match self.eat_match(Token::Comma) {
                Some(_comma) => continue,
                None => break,
            }
        }

        let end = self.hard_expect(Token::RParen)?.end;

        let node_info = NodeInfo::from_indices(start, end);

        Ok(Pattern {
            node_info,
            expressions,
        })
    }

    pub fn parse_type_specifier(
        &mut self,
    ) -> Result<Box<ast::TypeReference>, ParseResultError> {
        // these can start with a ( for a typle, a & for a reference, or a str, [<] for a named and
        // optionally parameterized type

        let scope = self.parse_scoped_name();

        /*if !scope.silent {
            self.hard_expect
        }*/
        // TODO: eval if should require a trailing :: during scope lookup

        let final_id = scope
            .scope
            .last()
            .expect("ScopedNameReference had no final id, not even implicit");
        let final_id = *final_id;
        let mut tr = TypeReference::new(*scope, final_id);

        let continuation = [Token::LParen, Token::Ampersand];
        //self.eat_match_in(&continuation)?;
        //println!("Trying to eat a type_spec with la {:?}", self.lex.la(0));
        match self.eat_match_in(&continuation) {
            Some(tw) => {
                match tw.token {
                    Token::Ampersand => {
                        //println!("adding a & to a ctx that had {:?}", tr.ctx.scope);
                        tr.ctx.scope.push(intern("&"));

                        let inner = self.parse_type_specifier()?;
                        tr.type_args.push(inner);
                    }
                    Token::LParen => {
                        tr.ctx.scope.push(intern("Tuple"));

                        //println!("la is {:?}", self.lex.la(0));

                        #[allow(irrefutable_let_patterns)]
                        while let tri = self.parse_type_specifier()? {
                            tr.type_args.push(tri);
                            match self.eat_match(Token::Comma) {
                                None => break,
                                Some(_comma) => continue,
                            }
                        }

                        self.hard_expect(Token::RParen)?;
                        //todo!()
                    }
                    _ => {
                        self.err(ParseResultError::UnexpectedToken(
                            tw,
                            continuation.to_vec(),
                            Some(
                                "Was trying to parse a type specifier, but didn't find a valid
                            start to such an expression",
                            ),
                        ))?;
                    }
                };

                //r

                //Ok(Box::new(tr))
            }
            None => {
                // no continuation to the type, parsing can be finished at this point
            }
        }

        println!("Returns with tr {:?}", tr);

        Ok(Box::new(tr))
    }

    /*pub fn parse_type_specifier(&mut self) -> Vec<ast::TypeReference<'input>> {
        let mut r = Vec::new();

        if let Some(_) = self.eat_match(Token::CmpLessThan) {
            let _ = self.hard_expect(Token::CmpGreaterThan);
        }

        r
    }*/

    pub fn parse_array_literal(&mut self) -> ExpressionResult {
        todo!()
    }

    pub fn parse_scoped_name(&mut self) -> Box<ScopedNameReference> {
        let mut r = Box::new(ScopedNameReference {
            scope: Vec::new(),
            silent: true,
            node_info: NodeInfo::Builtin,
        });

        let mut start = None;
        let mut end = None;

        match self.eat_match(Token::DoubleColon) {
            Some(dc) => {
                r.scope.push(intern("global"));
                r.scope.extend(self.scope.as_slice()); // TODO: revisit `global` prepending
                r.silent = false;
                start = Some(dc.start);
                end = Some(dc.end);
            }
            None => {
                eprintln!("need to address `local` vars within {}: {}", std::file!(), std::line!());
                r.scope.push(intern("local"));
            }
        }

        while let Some(id) = self.eat_match(Token::Identifier) {
            r.scope.push(id.slice);
            r.silent = false;

            start = Some(start.unwrap_or(id.start));
            end = Some(id.end);

            match self.eat_match(Token::DoubleColon) {
                None => break,
                Some(dc) => {
                    end = Some(dc.end);
                    continue;
                }
            }
        }

        match start {
            Some(start) => {
                // can't get start without also getting end
                let end = end.unwrap_or(start);
                r.node_info = NodeInfo::from_indices(start, end);
            }
            None => {}
        }

        r
    }

    pub fn atomic_expression(&mut self) -> ExpressionResult {
        if let Ok(tw) = self.lex.next() {
            match tw.token {
                Token::UnknownIntegerLiteral => Ok(ast::ExpressionWrapper::literal_expression(tw)),
                Token::StringLiteral => Ok(ast::ExpressionWrapper::literal_expression(tw)),
                Token::Underscore => Ok(ast::ExpressionWrapper::wildcard(tw)),
                Token::LParen | Token::DoubleColon | Token::Identifier => {
                    self.lex.backtrack();
                    self.parse_access_base(None)
                }
                _ => {
                    self.lex.backtrack();
                    Err(ParseResultError::UnexpectedToken(
                        tw,
                        vec![
                            Token::UnknownIntegerLiteral,
                            Token::StringLiteral,
                            Token::Underscore,
                            Token::LParen,
                            Token::DoubleColon,
                            Token::Identifier,
                        ],
                        None,
                    ))
                }
            }
        } else {
            self.err(ParseResultError::EndOfFile)
        }
    }

    pub fn parse_access_trailing(
        &mut self,
        on: Box<ExpressionWrapper>,
    ) -> ExpressionResult {
        let p = self.parse_pattern()?;
        let start = on.as_node().start().unwrap_or(CodeLocation::Builtin);

        let node_info = NodeInfo::from_indices(start, p.end().unwrap_or(start));

        let scope = ScopedNameReference {
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

    pub fn parse_access_base(
        &mut self,
        on: Option<Box<ExpressionWrapper>>,
    ) -> ExpressionResult {
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

        let base = self.parse_scoped_name();

        match base.node_info.as_parsed() {
            Some(pni) => {
                either = either.with_a(pni.span);
            }
            _ => {}
        }

        let b_pattern = match base.silent {
            true => Some(self.parse_pattern()?),
            false => self.parse_pattern().ok(),
        };

        match &b_pattern {
            Some(p) => match p.node_info().as_parsed() {
                Some(pni) => {
                    either = either.with_b(pni.span);
                }
                _ => {}
            },
            _ => {}
        }

        // invisible invariant: either base or b_pattern has to be Some, or both
        let (start, end) = match either {
            EitherAnd::Neither => panic!("Somehow got neither a pattern nor a base"),
            EitherAnd::A(a) => (a.start, a.end),
            EitherAnd::B(b) => (b.start, b.end),
            EitherAnd::Both(a, b) => (a.start, b.end),
        };

        let node_info = NodeInfo::from_indices(start, end);

        let ae = AccessExpression {
            node_info,
            on,
            scope: base,
            pattern: b_pattern,
        };

        Ok(Box::new(ExpressionWrapper::Access(ae)))
    }

    pub fn parse_if_then_else(&mut self) -> ExpressionResult {
        let start = self.hard_expect(Token::If)?.start;
        let if_exp = self.parse_expr()?;
        let then_exp = self.parse_expr()?;
        let (else_exp, end) = if self.eat_match(Token::Else).is_some() {
            let exp = self.parse_expr()?;
            let end = exp
                .as_node()
                .end()
                .expect("successfully parsed else had no end");

            (exp, end)
        } else {
            let exp = BlockExpression::new_expr(ast::NodeInfo::Builtin, Vec::new());
            let end = then_exp.as_node().end().expect("then had no end?");

            (exp, end)
        };

        let node_info = ast::NodeInfo::from_indices(start, end);

        Ok(IfThenElseExpression::new_expr(
            node_info, if_exp, then_exp, else_exp,
        ))
    }

    pub fn parse_while(&mut self) -> ExpressionResult {
        let start = self.hard_expect(Token::While)?.start;
        let while_exp = self.parse_expr()?;
        let do_exp = self.parse_expr()?;

        let node_info = ast::NodeInfo::from_indices(start, do_exp.as_node().end().unwrap_or(start));

        Ok(WhileExpression::new_expr(node_info, while_exp, do_exp))
    }

    pub fn syntactic_block(&mut self) -> ExpressionResult {
        self.hard_expect(Token::LBrace)?;
        let mut declarations: Vec<
            Result<Box<ast::ExpressionWrapper>, ParseResultError>,
        > = Vec::new();
        let start = self.lex.la(0).map_or(CodeLocation::Builtin, |tw| tw.start);

        //let mut failed = false;

        loop {
            match self.lex.la(0)?.token {
                Token::RBrace => {
                    break;
                }
                Token::Semicolon => {
                    self.lex.advance();
                    // empty
                }
                /*Token::Let => {
                    let r = self.variable_declaration();

                    let exp = r
                        .map(|vd| Box::new(ast::ExpressionWrapper::LetExpression(vd)))
                        .map_err(|e| {
                            self.report_err(e.clone());

                            failed = true;
                            e
                        });

                    declarations.push(exp);
                }*/
                _ => {
                    let sync = self.sync_next(&[Token::Semicolon]);

                    let e = self.parse_expr();

                    let final_exp = e
                        .map(|exp| match self.eat_match(Token::Semicolon) {
                            Some(semi) => {
                                let start =
                                    exp.as_node().start().map_or(CodeLocation::Builtin, |v| v);
                                let end = semi.end;
                                let node_info = ast::NodeInfo::from_indices(start, end);
                                ast::StatementExpression::new_expr(node_info, exp)
                            }
                            None => exp,
                        })
                        .map_err(|err| {
                            self.report_err(err.clone());

                            let _ = self.expect_next_in(&[Token::Semicolon, Token::RBrace]); // need to eat up to recovery point

                            //failed = true;
                            //self.eat_to(vec![Token::Semicolon, Token::RBrace]);
                            err
                        });

                    self.unsync(sync)?;

                    //self.expect(Token::Semicolon)?; // TODO: eval if this is required

                    declarations.push(final_exp);
                }
            }
        }

        let end = self.lex.la(-1).map_or(start, |tw| tw.start);

        let node_info = ast::NodeInfo::from_indices(start, end);

        self.hard_expect(Token::RBrace)?;

        Ok(ast::BlockExpression::new_expr(node_info, declarations))
    }

    pub fn parse_expr_inner(&mut self, min_bp: u32, level: usize) -> ExpressionResult {
        let t1 = self.lex.la(0)?;
        let mut lhs = match t1.token {
            Token::LBracket => {
                let r = self.parse_array_literal();

                r?
            }
            Token::LBrace => {
                let r = self.syntactic_block();

                r?
            }
            Token::If => {
                let r = self.parse_if_then_else();

                r?
            }
            Token::While => {
                let r = self.parse_while();

                r?
            }
            Token::Let => {
                let r = self.parse_let();

                r?
            }
            /*Token::Let => {
                let pattern = self.parse_atomic(0, level);

            }*/
            /*Token::Let => {
                let r = self.variable_declaration();

                r?
            },*/
            t if prefix_binding_power(t).is_some() => {
                self.lex.advance();

                let bp =
                    prefix_binding_power(t).expect("bp should already be Some from match guard");
                let rhs = self.parse_expr_inner(bp, level + 1)?;
                let start = t1.start;
                let end = rhs.as_node().end().expect("parsed rhs has no end?");
                let node_info = ast::NodeInfo::from_indices(start, end);

                self.build_unary(node_info, t, rhs)
            }
            //
            _other => self.atomic_expression()?,
        };

        loop {
            if let Some(_colon) = self.eat_match(Token::Colon) {
                todo!("Type constraints not implemented yet")
            } else if let Some(_as) = self.eat_match(Token::As) {
                let typeref: Box<ast::TypeReference> = self.parse_type_specifier()?;
                let start = lhs
                    .as_node()
                    .start()
                    .expect("parsed lhs did not have a start");
                let end = typeref.end().expect("parsed typeref did not have an end");
                let node_info = NodeInfo::from_indices(start, end);
                lhs = ast::CastExpression::new_expr(node_info, lhs, typeref);
                continue;
            } else if let Some(((l_bp, r_bp), tw)) = self.eat_if(|t| infix_binding_power(t.token)) {
                //if let Some(((l_bp, r_bp), tw)) = operator {
                if l_bp < min_bp {
                    self.lex.backtrack();
                    break;
                } else {
                    let rhs = self.parse_expr_inner(r_bp, level + 1)?;
                    let start = lhs.as_node().start().expect("parsed lhs has no start?");
                    let end = rhs.as_node().end().expect("parsed rhs has no end?");
                    let node_info = ast::NodeInfo::from_indices(start, end);
                    lhs = self.build_binary(node_info, tw.token, lhs, rhs)?;
                    continue;
                }
            } else if let Some(_tw) = self.eat_match_in(&[
                Token::LParen,
                Token::LBracket,
                Token::Identifier,
                Token::DoubleColon,
            ]) {
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

    fn build_unary(
        &mut self,
        node_info: NodeInfo,
        t: Token,
        lhs: Box<ast::ExpressionWrapper>,
    ) -> Box<ast::ExpressionWrapper> {
        match t {
            Token::Ampersand | Token::Asterisk | Token::Dash | Token::Bang => {
                UnaryOperationExpression::new_expr(node_info, t, lhs)
            }
            //Token::Let => LetExpression::new_expr(node_info, lhs),
            Token::Return => ReturnExpression::new_expr(node_info, lhs),
            _ => {
                println!("got unexpected token {:?}", t);
                panic!("Programming error: no way to build unary expression from given token");
            }
        }
    }

    fn build_binary(
        &mut self,
        node_info: NodeInfo,
        t: Token,
        lhs: Box<ast::ExpressionWrapper>,
        rhs: Box<ast::ExpressionWrapper>,
    ) -> ExpressionResult {
        match t {
            Token::Plus | Token::Dash | Token::Asterisk | Token::FSlash => {
                Ok(BinaryOperationExpression::new_expr(node_info, t, lhs, rhs))
            }
            //Token::As => Ok(CastExpression::new_expr(node_info, lhs, rhs)),
            Token::Equals => Ok(AssignmentExpression::new_expr(node_info, lhs, rhs)),
            Token::Dot => {
                let rhs = match *rhs {
                    ExpressionWrapper::Access(mut ae) => {
                        ae.on = Some(lhs);

                        Box::new(ExpressionWrapper::Access(ae))
                    }
                    _ => {
                        let err = ParseResultError::SemanticIssue(
                            "Can not perform a dot-access with non-access RHS.
                        RHS must be of the form (ScopedIdentifier &| Pattern).
                        Offending RHS occurs at span",
                            rhs.as_node().start().unwrap_or(CodeLocation::Builtin),
                            rhs.as_node().end().unwrap_or(CodeLocation::Builtin),
                        );

                        //self.errors.push(err);

                        //return Err(err);

                        return self.err(err);
                    }
                };

                Ok(rhs)
            }
            Token::CmpEqual
            | Token::CmpLessThan
            | Token::CmpGreaterThan
            | Token::CmpLessThanOrEqual
            | Token::CmpGreaterThanOrEqual
            | Token::CmpNotEqual => Ok(ComparisonOperationExpression::new_expr(
                node_info, t, lhs, rhs,
            )),
            _ => {
                println!("got unexpected token {:?}", t);
                panic!("Programming error: no way to build binary expression from given token");
            }
        }
    }
}

pub enum DotAccess {
    Field(StringSymbol),
    Method(StringSymbol, Vec<Box<ast::ExpressionWrapper>>),
}

use ast::IntoAstNode;
pub fn prefix_binding_power(t: Token) -> Option<u32> {
    match t {
        Token::Plus
        | Token::Dash
        | Token::Asterisk
        | Token::Bang
        | Token::Ampersand
        | Token::Let
        | Token::Return => Some(100),

        _ => None,
    }
}

pub fn postfix_binding_power(t: Token) -> Option<u32> {
    match t {
        Token::As => Some(1),
        Token::Colon => Some(1),
        _ => None,
    }
}

pub fn infix_binding_power(t: Token) -> Option<(u32, u32)> {
    match t {
        Token::As => Some((1, 300)),

        Token::Dot => Some((250, 250)),

        Token::Equals => Some((200, 2)),

        Token::LogicalOr => Some((3, 4)),

        Token::LogicalAnd => Some((5, 6)),

        Token::CmpEqual
        | Token::CmpLessThan
        | Token::CmpGreaterThan
        | Token::CmpLessThanOrEqual
        | Token::CmpGreaterThanOrEqual
        | Token::CmpNotEqual => Some((7, 8)),

        Token::Pipe => Some((9, 10)),

        Token::Caret => Some((11, 12)),

        Token::Ampersand => Some((13, 14)),

        //Token::ShiftLeft | Token::ShiftRight => Some((15, 16)),
        Token::Plus | Token::Dash => Some((17, 18)),

        Token::Asterisk | Token::FSlash | Token::Modulo => Some((19, 20)),

        _ => None,
    }
}
