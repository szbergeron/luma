use crate::ast::{self, TypeReference};
use crate::lex::{CodeLocation, ParseResultError, Token};

//use crate::helper::lex_wrap::{CodeLocation, ParseResultError};
use crate::helper::EitherNone;

use crate::parse::*;

//use crate::parse_helper::*;

use ast::base::*;
use ast::expressions::*;
use ast::outer::*;

use super::schema::{
    CorrectionBubblingError, CorrectionBubblingResult, ParseResult, ResultHint, TokenProvider,
};

//type LetRes

type ExpressionResult = ParseResult<Box<ast::ExpressionWrapper>>;

impl<'lexer> Parser<'lexer> {
    pub fn parse_expr(&mut self, t: &TokenProvider) -> ExpressionResult {
        let t = t.child();

        let r = self.parse_expr_inner(0, 1, &t);

        r
    }

    /// Parses the recursively defined first part of a let statement,
    ///
    /// let <parse_binding()> = <expression>;
    ///
    /// includes support for parsing type specifiers within
    pub fn parse_binding(
        &mut self,
        t: &TokenProvider,
    ) -> ParseResult<Box<LetComponent>> {
        // want to find if this is going to be a destructuring operation
        let mut t = t.child().predict(&[Token::LParen, Token::Identifier]);
        let next_token = t.sync().la(0).map_err(|e| CorrectionBubblingError::from_fatal_error(e))?;

        let mut end;
        let start;

        // parse body/binding part of let statement
        let content = match next_token.token {
            Token::LParen => {
                let mut elements: Vec<LetComponent> = Vec::new();
                //todo!("Pattern assignment not yet implemented")
                start = t.take(Token::LParen).hard(&mut t)?.join(&mut t).start; // consume start lparen

                while let Ok(expr) = self.parse_binding(&t).soft() {
                    let expr = expr.join(&mut t);

                    elements.push(*expr);

                    match t.try_take(Token::Comma) {
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

                end = t.take(Token::RParen).hard(&mut t)?.join(&mut t).end; // if user opened, must close

                // see if a type exists, user may specify type anywhere during parsing, but
                // must be possible to resolve types directly for every binding
                // without needing to infer
                Either::A(elements)
            }
            Token::Identifier => {
                let variable = t.take(Token::Identifier).hard(&mut t)?.join(&mut t);
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
        let specified_type = match t.try_take(Token::Colon) {
            Some(_colon) => {
                let r = self.parse_type_specifier(&t).hard(&mut t)?.join(&mut t);
                r.end().map(|loc| end = loc);
                Some(Box::new(r))
            }
            None => None,
        };

        let node_info = NodeInfo::from_indices(start, end);

        match content {
            Either::A(tuple_elements) => {
                t.success(Box::new(LetComponent::Tuple(LetComponentTuple {
                    elements: tuple_elements,
                    node_info,
                    type_specifier: specified_type,
                })))
            }
            Either::B(name) => {
                t.success(Box::new(LetComponent::Identifier(LetComponentIdentifier {
                    identifier_string: name,
                    node_info,
                    type_specifier: specified_type,
                })))
            }
        }
    }

    /// syntax: let <binding>: <type> = <expr>
    pub fn parse_let(&mut self, t: &TokenProvider) -> ExpressionResult {
        let mut t = t.child();

        let start = t.take(Token::Let).hard(&mut t)?.join(&mut t).start;

        let binding = self.parse_binding(&t).hard(&mut t)?.join(&mut t);

        let _eq = t.take(Token::Equals).hard(&mut t)?.join(&mut t);

        let expr = self.parse_expr(&t).hard(&mut t)?.join(&mut t);

        // TODO: eval if this could ever be None, potentially force expr to have a
        // CodeLocation::Parsed
        let end = expr.as_node().end().unwrap_or(CodeLocation::Builtin);

        let lexpr = LetExpression {
            node_info: NodeInfo::from_indices(start, end),
            primary_component: binding,
            expression: expr,
        };

        //todo!("Let expression parsing not yet complete")
        t.success(Box::new(ExpressionWrapper::LetExpression(lexpr)))
    }

    // follows typespecifier? pattern
    // where:
    //     typespecifier: <typelist>
    //     pattern: (expressionlist)
    pub fn parse_pattern(&mut self, t: &TokenProvider) -> ParseResult<Pattern> {
        let mut t = t.child();
        // can be a single literal or tuple, and each tuple is a set of expressions

        let lp = t.take(Token::LParen).hard(&mut t)?.join(&mut t);

        let start = lp.start;

        let mut expressions = Vec::new();

        while let Ok(expr) = self.parse_expr(&t).soft() {
            let expr = expr.join(&mut t);

            expressions.push(expr);

            match t.try_take(Token::Comma) {
                Some(_comma) => continue,
                None => break,
            }
        }

        let end = t.take(Token::RParen).hard(&mut t)?.join(&mut t).end;

        let node_info = NodeInfo::from_indices(start, end);

        t.success(Pattern {
            node_info,
            expressions,
        })
    }
    /*pub fn parse_type_specifier(&mut self) -> Vec<ast::TypeReference<'input>> {
        let mut r = Vec::new();

        if let Some(_) = self.eat_match(Token::CmpLessThan) {
            let _ = self.hard_expect(Token::CmpGreaterThan);
        }

        r
    }*/

    pub fn parse_array_literal(&mut self, t: &TokenProvider) -> ExpressionResult {
        todo!()
    }

    pub fn parse_scoped_name(
        &mut self,
        t: &TokenProvider,
    ) -> ParseResult<Box<ScopedNameReference>> {
        let mut t = t.child();

        let mut r = Box::new(ScopedNameReference {
            scope: Vec::new(),
            silent: true,
            node_info: NodeInfo::Builtin,
        });

        let mut start = None;
        let mut end = None;

        match t.try_take(Token::DoubleColon) {
            Some(dc) => {
                r.scope.push(intern("global"));
                r.scope.extend(self.scope.as_slice()); // TODO: revisit `global` prepending
                r.silent = false;
                start = Some(dc.start);
                end = Some(dc.end);
            }
            None => {
                eprintln!(
                    "need to address `local` vars within {}: {}",
                    std::file!(),
                    std::line!()
                );
                r.scope.push(intern("local"));
            }
        }

        while let Some(id) = t.try_take(Token::Identifier) {
            r.scope.push(id.slice);
            r.silent = false;

            start = Some(start.unwrap_or(id.start));
            end = Some(id.end);

            match t.try_take(Token::DoubleColon) {
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

        t.success(r)
    }

    pub fn atomic_expression(&mut self, t: &TokenProvider) -> ExpressionResult {
        let t = t.child();

        if let Ok(tw) = self.lex.next() {
            match tw.token {
                Token::UnknownIntegerLiteral => {
                    t.success(ast::ExpressionWrapper::literal_expression(tw))
                }
                Token::StringLiteral => t.success(ast::ExpressionWrapper::literal_expression(tw)),
                Token::Underscore => t.success(ast::ExpressionWrapper::wildcard(tw)),
                Token::LParen | Token::DoubleColon | Token::Identifier => {
                    self.lex.backtrack();
                    self.parse_access_base(&t, None)
                }
                _ => {
                    self.lex.backtrack();
                    t.failure(Some(ParseResultError::UnexpectedToken(
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
                    )))
                }
            }
        } else {
            t.failure(Some(ParseResultError::EndOfFile))
        }
    }

    pub fn parse_access_trailing(
        &mut self,
        t: &TokenProvider,
        on: Box<ExpressionWrapper>,
    ) -> ExpressionResult {
        let mut t = t.child();

        let p = self.parse_pattern(&t).hard(&mut t)?.join(&mut t);
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

        t.success(Box::new(ExpressionWrapper::Access(ae)))
    }

    pub fn parse_access_base(
        &mut self,
        t: &TokenProvider,
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
        let mut t = t.child();

        let mut either: EitherNone<Span, Span> = EitherNone::Neither();

        let base = self
            .parse_scoped_name(&t)
            .hard(&mut t)
            .unwrap()
            .join(&mut t);

        match base.node_info.as_parsed() {
            Some(pni) => {
                either = either.with_a(pni.span);
            }
            _ => {}
        }

        let b_pattern = match base.silent {
            true => Some(self.parse_pattern(&t).hard(&mut t)?.join(&mut t)),
            false => self.parse_pattern(&t).ok().map(|v| { todo!() }), // TODO: fallible patterns
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
            EitherNone::Neither() => panic!("Somehow got neither a pattern nor a base"),
            EitherNone::A(a) => (a.start, a.end),
            EitherNone::B(b) => (b.start, b.end),
            EitherNone::Both(a, b) => (a.start, b.end),
        };

        let node_info = NodeInfo::from_indices(start, end);

        let ae = AccessExpression {
            node_info,
            on,
            scope: base,
            pattern: b_pattern,
        };

        t.success(Box::new(ExpressionWrapper::Access(ae)))
    }

    pub fn parse_llvm_builtin(&mut self, t: &TokenProvider) -> ExpressionResult {
        let mut t = t.child();

        let start = t.take(Token::InteriorBuiltin).hard(&mut t)?.join(&mut t);
        //let name = self.hard_expect(Token::Identifier)?;

        //while let Some(llvm_directive) = self.eat_match_in(&[Token::LL_Bind
        let mut bindings = Vec::new();
        let mut vars = Vec::new();

        while let Some(tok) = t.try_take_in(&[Token::LL_Bind, Token::LL_Var]) {
            match tok.token {
                Token::LL_Bind => {
                    let exp = self.parse_expr(&t).hard(&mut t)?.join(&mut t);
                    let _ = t.take(Token::ThickArrow).hard(&mut t)?.join(&mut t);
                    let name = t.take(Token::Identifier).hard(&mut t)?.join(&mut t);

                    bindings.push((*exp, name.slice));
                }
                Token::LL_Var => {
                    let name = t.take(Token::Identifier).hard(&mut t)?.join(&mut t);

                    vars.push(name.slice);
                }
                _ => panic!("fell out of match while parsing llvm_builtin"),
            }
        }

        let maybe_result = if let Some(_) = t.try_take(Token::LL_Result) {
            let name = t.take(Token::Identifier).hard(&mut t)?.join(&mut t);
            t.take(Token::Colon).hard(&mut t)?.join(&mut t);
            let tr = self.parse_type_specifier(&t).hard(&mut t)?.join(&mut t);
            Some((tr, name.slice))
        } else {
            None
        };

        let body = t.take(Token::LLVMBlock).hard(&mut t)?.join(&mut t);

        let llvmle = LLVMLiteralExpression {
            node_info: NodeInfo::from_indices(start.start, body.end),
            bindings,
            vars,
            text: body.slice,
            output: maybe_result,
        };

        t.success(Box::new(ExpressionWrapper::LLVMLiteral(llvmle)))
    }

    pub fn parse_if_then_else(&mut self, t: &TokenProvider) -> ExpressionResult {
        let mut t = t.child();

        let start = t
            .take(Token::If)
            .hard(&mut t)
            .expect("Tried to parse if-else with no beginning if")
            .join(&mut t)
            .start;
        let if_exp = self.parse_expr(&t).hint("An 'if' must be followed by a conditional expression followed by a then expression").hard(&mut t)?.join(&mut t);
        let then_exp = self.parse_expr(&t).hard(&mut t)?.join(&mut t);
        let (else_exp, end) = if t.try_take(Token::Else).is_some() {
            let exp = self.parse_expr(&t).hard(&mut t)?.join(&mut t);
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

        t.success(IfThenElseExpression::new_expr(
            node_info, if_exp, then_exp, else_exp,
        ))
    }

    pub fn parse_while(&mut self, t: &TokenProvider) -> ExpressionResult {
        let mut t = t.child();

        let start = t.take(Token::While).hard(&mut t)?.join(&mut t).start;
        let while_exp = self.parse_expr(&t).hard(&mut t)?.join(&mut t);
        let do_exp = self.parse_expr(&t).hard(&mut t)?.join(&mut t);

        let node_info = ast::NodeInfo::from_indices(start, do_exp.as_node().end().unwrap_or(start));

        t.success(WhileExpression::new_expr(node_info, while_exp, do_exp))
    }

    pub fn syntactic_block(&mut self, t: &TokenProvider) -> ExpressionResult {
        let mut t = t
            .child()
            .predict(&[Token::LBrace, Token::Semicolon, Token::RBrace]);

        t.take(Token::LBrace).hard(&mut t)?.join(&mut t);
        let mut declarations: Vec<Box<ast::ExpressionWrapper>> =
            Vec::new();
        let start = self.lex.la(0).map_or(CodeLocation::Builtin, |tw| tw.start);

        //let mut failed = false;

        loop {
            match self.lex.la(0).map_err(|e| CorrectionBubblingError::from_fatal_error(e))?.token {
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
                    let e = self.parse_expr(&t);

                    let final_exp = e
                        .handled(&mut t)
                        .map(|exp| {
                            let exp = exp.join(&mut t);
                            let exp = match t.try_take(Token::Semicolon) {
                                Some(semi) => {
                                    let start =
                                        exp.as_node().start().map_or(CodeLocation::Builtin, |v| v);
                                    let end = semi.end;
                                    let node_info = ast::NodeInfo::from_indices(start, end);
                                    ast::StatementExpression::new_expr(node_info, exp)
                                }
                                None => exp,
                            };

                            declarations.push(exp);
                        })
                        .map_err(|err| {
                            //self.report_err(err.clone());

                            let _ = t.take_in(&[Token::Semicolon, Token::RBrace]); // need to eat up to recovery point // TODO

                            //failed = true;
                            //self.eat_to(vec![Token::Semicolon, Token::RBrace]);
                            //err.internal_error
                        });

                    //self.expect(Token::Semicolon)?; // TODO: eval if this is required

                }
            }
        }

        let end = self.lex.la(-1).map_or(start, |tw| tw.start);

        let node_info = ast::NodeInfo::from_indices(start, end);

        t.take(Token::RBrace).hard(&mut t)?.join(&mut t);

        t.success(ast::BlockExpression::new_expr(node_info, declarations))
    }

    pub fn parse_expr_inner(
        &mut self,
        min_bp: u32,
        level: usize,
        t: &TokenProvider,
    ) -> ExpressionResult {
        //let t1 = self.lex.la(0)?;

        let mut t = t.child();

        let t1 = t.sync().la(0).map_err(|e| CorrectionBubblingError::from_fatal_error(e))?;
        let mut lhs = match t1.token {
            Token::LBracket => {
                let r = self.parse_array_literal(&t).hard(&mut t)?.join(&mut t);

                r
            }
            Token::LBrace => {
                let r = self.syntactic_block(&t).hard(&mut t)?.join(&mut t);

                r
            }
            Token::If => {
                let r = self.parse_if_then_else(&t).hard(&mut t)?.join(&mut t);

                r
            }
            Token::InteriorBuiltin => {
                let r = self.parse_llvm_builtin(&t).hard(&mut t)?.join(&mut t);

                r
            }
            Token::While => {
                let r = self.parse_while(&t).hard(&mut t)?.join(&mut t);

                r
            }
            Token::Let => {
                let r = self.parse_let(&t).hard(&mut t)?.join(&mut t);

                r
            }
            /*Token::Let => {
                let pattern = self.parse_atomic(0, level);

            }*/
            /*Token::Let => {
                let r = self.variable_declaration();

                r?
            },*/
            tok if prefix_binding_power(tok).is_some() => {
                self.lex.advance();

                let bp =
                    prefix_binding_power(tok).expect("bp should already be Some from match guard");
                let rhs = self.parse_expr_inner(bp, level + 1, &t).hard(&mut t)?.join(&mut t);
                let start = t1.start;
                let end = rhs.as_node().end().expect("parsed rhs has no end?");
                let node_info = ast::NodeInfo::from_indices(start, end);

                self.build_unary(&t, node_info, tok, rhs).hard(&mut t)?.join(&mut t)
            }
            //
            _other => {
                self.atomic_expression(&t).hard(&mut t)?;
                todo!()
            }
        };

        loop {
            if let Some(_colon) = t.try_take(Token::Colon) {
                todo!("Type constraints not implemented yet")
            } else if let Some(_as) = t.try_take(Token::As) {
                let typeref: Box<ast::TypeReference> = Box::new(self.parse_type_specifier(&t).hard(&mut t)?.join(&mut t));
                let start = lhs
                    .as_node()
                    .start()
                    .expect("parsed lhs did not have a start");
                let end = typeref.end().expect("parsed typeref did not have an end");
                let node_info = NodeInfo::from_indices(start, end);
                lhs = ast::CastExpression::new_expr(node_info, lhs, typeref);
                continue;
            } else if let Some(((l_bp, r_bp), tw)) = t.try_take_if(|tw| infix_binding_power(tw.token)) {
                //if let Some(((l_bp, r_bp), tw)) = operator {
                if l_bp < min_bp {
                    self.lex.backtrack();
                    break;
                } else {
                    let rhs = self.parse_expr_inner(r_bp, level + 1, &t).hard(&mut t)?.join(&mut t);
                    let start = lhs.as_node().start().expect("parsed lhs has no start?");
                    let end = rhs.as_node().end().expect("parsed rhs has no end?");
                    let node_info = ast::NodeInfo::from_indices(start, end);
                    lhs = self.build_binary(&t, node_info, tw.token, lhs, rhs).hard(&mut t)?.join(&mut t);
                    continue;
                }
            } else if let Some(_tw) = t.try_take_in(&[
                Token::LParen,
                Token::LBracket,
                Token::Identifier,
                Token::DoubleColon,
            ]) {
                self.lex.backtrack(); // want to know it's there, but not consume it
                                      // not a binary operator per-se, could be chained access?

                lhs = self.parse_access_trailing(&t, lhs).hard(&mut t)?.join(&mut t);

                continue;
            } else {
                break;
            }
        }

        t.success(lhs)
    }

    fn build_unary(
        &mut self,
        t: &TokenProvider,
        node_info: NodeInfo,
        token: Token,
        lhs: Box<ast::ExpressionWrapper>,
    ) -> ExpressionResult {
        match token {
            Token::Ampersand | Token::Asterisk | Token::Dash | Token::Bang => {
                t.success(UnaryOperationExpression::new_expr(node_info, token, lhs))
            }
            //Token::Let => LetExpression::new_expr(node_info, lhs),
            Token::Return => t.success(ReturnExpression::new_expr(node_info, lhs)),
            _ => {
                println!("got unexpected token {:?}", token);
                panic!("Programming error: no way to build unary expression from given token");
            }
        }
    }

    fn build_binary(
        &mut self,
        t: &TokenProvider,
        node_info: NodeInfo,
        token: Token,
        lhs: Box<ast::ExpressionWrapper>,
        rhs: Box<ast::ExpressionWrapper>,
    ) -> ExpressionResult {
        let t = t.child();

        match token {
            Token::Plus | Token::Dash | Token::Asterisk | Token::FSlash => {
                t.success(BinaryOperationExpression::new_expr(node_info, token, lhs, rhs))
            }
            //Token::As => Ok(CastExpression::new_expr(node_info, lhs, rhs)),
            Token::Equals => t.success(AssignmentExpression::new_expr(node_info, lhs, rhs)),
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

                        //return Err(t.failure(Some(err)).hard(&mut t).err());
                        return t.failure(Some(err));
                    }
                };

                t.success(rhs)
            }
            Token::CmpEqual
            | Token::CmpLessThan
            | Token::CmpGreaterThan
            | Token::CmpLessThanOrEqual
            | Token::CmpGreaterThanOrEqual
            | Token::CmpNotEqual => t.success(ComparisonOperationExpression::new_expr(
                node_info, token, lhs, rhs,
            )),
            _ => {
                println!("got unexpected token {:?}", token);
                panic!("Programming error: no way to build binary expression from given token");
            }
        }
    }
}

pub enum DotAccess {
    Field(IStr),
    Method(IStr, Vec<Box<ast::ExpressionWrapper>>),
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
