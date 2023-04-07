//use crate::ast;
use crate::cst::{self, StructLiteralExpression, SyntacticTypeReference};
use crate::lex::{CodeLocation, ParseResultError, Token};
use either::Either;
use itertools::Itertools;

//use crate::helper::lex_wrap::{CodeLocation, ParseResultError};

use crate::parse::*;

//use crate::parse_helper::*;

//use cst::expressions::*;
//use cst::declarations::*;
use cst::cst_traits::*;

use super::schema::{ResultHint, TokenProvider};

//type LetRes

type ExpressionResult = ParseResult<Box<cst::expressions::ExpressionWrapper>>;

impl<'lexer> Parser<'lexer> {
    pub fn parse_expr(&mut self, t: &TokenProvider, with_generics: &Vec<IStr>) -> ExpressionResult {
        //let t = t.child();
        let mut t = parse_header!(t);

        let r = self
            .parse_expr_inner(0, 1, &t, with_generics)
            .join_hard(&mut t)
            .catch(&mut t)?;

        /*println!("Expression:");
        println!("{r}");*/

        t.success(r)
    }

    /// Parses the recursively defined first part of a let statement,
    ///
    /// let <parse_binding()> = <expression>;
    ///
    /// includes support for parsing type specifiers within
    pub fn parse_binding(
        &mut self,
        t: &TokenProvider,
        with_generics: &Vec<IStr>,
    ) -> ParseResult<Box<cst::LetComponent>> {
        // want to find if this is going to be a destructuring operation
        /*let mut t = t
        .child()
        .predict(&[(Token::LParen, 1.0), (Token::Identifier, 1.0)]);*/
        let mut t = parse_header!(t, [Token::LParen => 1.0, Token::Identifier => 1.0]);
        //let next_token = t.sync().la(0).map_err(|e| CorrectionBubblingError::from_fatal_error(e))?;
        let next_token = t.la(0).join_noncommittal().catch(&mut t)?;
        //let next_token = t.lh.la(0).;

        let mut end;
        let start;

        // parse body/binding part of let statement
        let content = match next_token.token {
            Token::LParen => {
                let mut elements: Vec<cst::LetComponent> = Vec::new();
                //todo!("Pattern assignment not yet implemented")
                start = t.take(Token::LParen).join()?.start; // consume start lparen

                while let Some(expr) = self
                    .parse_binding(&t, with_generics)
                    .join_noncommittal()
                    .catch(&mut t)
                    .try_get()
                {
                    elements.push(*expr);

                    match t.try_take(Token::Comma) {
                        None => break,

                        // consequence of this is& that trailing comma is discarded
                        // arity of tuple is not increased in this case, but
                        // these semantics make sense to me
                        //&
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

                end = t.take(Token::RParen).join()?.end; // if user opened, must close

                // see if a type exists, user may specify type anywhere during parsing, but
                // must be possible to resolve types directly for every binding
                // without needing to infer
                Either::Left(elements)
            }
            Token::Identifier => {
                let variable = t.take(Token::Identifier).join()?;
                let variable_name = variable.slice;

                start = variable.start;
                end = variable.end;

                Either::Right(variable_name)
            }
            _ => {
                unreachable!("expect_next_in forwarded a bad token")
            }
        };

        // if there's a colon, we assume that a type specifier must directly follow it
        //if let Some(_colon) = self.eat_match(Token::
        let specified_type = match t.try_take(Token::Colon) {
            Some(_colon) => {
                let r = self
                    .parse_type_specifier(&t, with_generics)
                    .join_hard(&mut t)
                    .catch(&mut t)?;
                r.end().map(|loc| end = loc);
                Some(Box::new(r.intern()))
            }
            None => None,
        };

        let node_info = NodeInfo::from_indices(start, end);

        match content {
            Either::Left(tuple_elements) => {
                t.success(Box::new(cst::LetComponent::Tuple(cst::LetComponentTuple {
                    elements: tuple_elements,
                    node_info,
                    type_specifier: specified_type,
                })))
            }
            Either::Right(name) => t.success(Box::new(cst::LetComponent::Identifier(
                cst::LetComponentIdentifier {
                    identifier_string: name,
                    node_info,
                    type_specifier: specified_type,
                },
            ))),
        }
    }

    pub fn parse_struct_literal(
        &mut self,
        t: &TokenProvider,
        with_generics: &Vec<IStr>,
    ) -> ExpressionResult {
        let mut t = parse_header!(t);
        tracing::info!("todo: better error handling here");

        let start = t.take(Token::Struct).join()?.start;

        let struct_type = self
            .parse_type_specifier(&t, with_generics)
            .join_hard(&mut t)
            .catch(&mut t)?;

        t.take(Token::LBrace).join()?;

        let mut pairs = Vec::new();

        let end = loop {
            let la = t.la(0).catch(&mut t).join_hard(&mut t)?;
            match la.token {
                Token::RBrace => {
                    t.take(Token::RBrace).join()?;
                    break la.end;
                }
                _ => {
                    let field_name = t.take(Token::Identifier).join()?.slice;
                    let _colon = t.take(Token::Colon).join()?;
                    // TODO: recover errors
                    let val = self
                        .parse_expr(&t, with_generics)
                        .join_hard(&mut t)
                        .catch(&mut t)?;

                    pairs.push((field_name, val));
                }
            }
        };

        tracing::info!("struct literal thing");

        let (sb, sg) = match struct_type.inner {
            cst::SyntacticTypeReferenceInner::Unconstrained() => todo!("what"),
            cst::SyntacticTypeReferenceInner::Tuple(_) => todo!("no"),
            cst::SyntacticTypeReferenceInner::Single { name } => (name, vec![]),
            cst::SyntacticTypeReferenceInner::Generic { label } => todo!("no"),
            cst::SyntacticTypeReferenceInner::Parameterized { name, generics } => {
                (name, generics.into_iter().map(|g| g.intern()).collect_vec())
            }
            cst::SyntacticTypeReferenceInner::Reference { to, mutable } => todo!("huh"),
            cst::SyntacticTypeReferenceInner::Pointer { to, mutable } => todo!("wheee"),
        };

        t.success(Box::new(cst::ExpressionWrapper::StructLiteral(
            StructLiteralExpression {
                info: NodeInfo::from_indices(start, end),
                bind_from: pairs,
                struct_base: sb,
                generics: sg,
            },
        )))
    }

    /// syntax: let <binding>: <type> = <expr>
    pub fn parse_let(&mut self, t: &TokenProvider, with_generics: &Vec<IStr>) -> ExpressionResult {
        //let mut t = t.child();
        let mut t = parse_header!(t);

        let start = t.take(Token::Let).join()?.start;

        let binding = self
            .parse_binding(&t, with_generics)
            .join_hard(&mut t)
            .catch(&mut t)?;

        let _eq = t.take(Token::Equals).join()?;

        let expr = self
            .parse_expr(&t, with_generics)
            .join_hard(&mut t)
            .catch(&mut t)?;

        // TODO: eval if this could ever be None, potentially force expr to have a
        // CodeLocation::Parsed
        let end = expr.as_node().end().unwrap_or(CodeLocation::Builtin);

        let lexpr = cst::LetExpression {
            node_info: NodeInfo::from_indices(start, end),
            primary_component: binding,
            expression: expr,
            constrained_to: SyntacticTypeReference::unconstrained().intern(),
        };

        //todo!("Let expression parsing not yet complete")
        t.success(Box::new(cst::ExpressionWrapper::LetExpression(lexpr)))
    }

    // follows typespecifier? pattern
    // where:
    //     typespecifier: <typelist>
    //     pattern: (expressionlist)
    pub fn parse_pattern(
        &mut self,
        t: &TokenProvider,
        with_generics: &Vec<IStr>,
    ) -> ParseResult<cst::Tuple> {
        //let mut t = t.child();
        let mut t = parse_header!(t);
        // can be a single literal or tuple, and each tuple is a set of expressions

        let lp = t.take(Token::LParen).join()?;

        let start = lp.start;

        let mut expressions = Vec::new();

        while let Some(expr) = self
            .parse_expr(&t, with_generics)
            .join_sync(&mut t)
            .catch(&mut t)
            .try_get()
        {
            expressions.push(expr);

            match t.try_take(Token::Comma) {
                Some(_comma) => continue,
                None => break,
            }
        }

        let end = t.take(Token::RParen).join()?.end;

        let node_info = NodeInfo::from_indices(start, end);

        t.success(cst::Tuple {
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

    pub fn parse_array_literal(&mut self, _t: &TokenProvider) -> ExpressionResult {
        todo!()
    }

    pub fn parse_scope(&mut self, t: &TokenProvider) -> ParseResult<cst::ScopedNameReference> {
        println!("parsing a scope");
        let mut t = parse_header!(t);

        let mut names = Vec::new();

        let mut node_info = NodeInfo::Builtin;

        while let Some(s) = t.try_take_string([Token::Identifier, Token::DoubleColon]) {
            let ident = s[0];
            println!("pushes {} to scope", ident.slice);
            names.push(ident.slice);

            node_info = node_info.extended(ident);
            node_info = node_info.extended(s[1]);
        }

        t.success(cst::ScopedNameReference {
            node_info,
            silent: names.len() > 0,
            scope: names,
        })
    }

    /*pub fn parse_scoped_name(
        &mut self,
        t: &TokenProvider,
    ) -> ParseResult<Box<ScopedNameReference>> {
        //let mut t = t.child();
        let mut t = parse_header!(t);

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
    }*/

    pub fn atomic_expression(
        &mut self,
        t: &TokenProvider,
        with_generics: &Vec<IStr>,
    ) -> ExpressionResult {
        //let mut t = t.child();
        let mut t = parse_header!(t);
        let tw = t
            .take_in(&[
                Token::UnknownIntegerLiteral,
                Token::StringLiteral,
                Token::Underscore,
                Token::LParen,
                Token::DoubleColon,
                Token::Identifier,
            ])
            .join()?;

        match tw.token {
            Token::UnknownIntegerLiteral | Token::StringLiteral => {
                t.success(cst::ExpressionWrapper::literal_expression(tw))
            }
            Token::Underscore => t.success(cst::ExpressionWrapper::wildcard(tw)),
            Token::Identifier | Token::DoubleColon => {
                t.lh.backtrack();

                self.parse_access_base(&t, with_generics)
            }
            _ => todo!(),
        }
    }

    //pub fn parse_scope(&mut self, t: &TokenProvider) -> S

    /// Use this to try to parse "continuations" of accesses.
    /// This includes indexing with [], doing calls with .f(...), and doing member accesses with .a
    ///
    /// Returns a tuple of (continue, result)
    /// where <continue> is whether parse_access_continuation should be called again on the
    /// returned result (if any more continuation can be expected) since the current continuation
    /// was not null deriving or a "passthrough"
    pub fn parse_access_continuation(
        &mut self,
        t: &TokenProvider,
        on: Box<cst::ExpressionWrapper>,
        with_generics: &Vec<IStr>,
    ) -> ParseResult<(bool, Box<cst::ExpressionWrapper>)> {
        let mut t = parse_header!(t);

        let next = t.try_take_in(&[Token::LBracket, Token::Dot, Token::LParen]);
        let next = match next {
            Some(tw) => tw,
            None => return t.success((false, on)),
        };

        let ni = on.as_node().node_info();
        let start = ni
            .as_parsed()
            .map(|pni| pni.span.start)
            .unwrap_or(CodeLocation::Builtin);

        match next.token {
            Token::LParen => {
                // doing a function call on the current continuation
                t.lh.backtrack();
                let call_tuple = self
                    .parse_tuple(&t, with_generics)
                    .join_hard(&mut t)
                    .catch(&mut t)?;
                let end = if let NodeInfo::Parsed(pni) = call_tuple.as_node().node_info() {
                    pni.span.end
                } else {
                    CodeLocation::Builtin
                };

                let ni = NodeInfo::from_indices(start, end);

                let fc = cst::FunctionCall {
                    function: on,
                    args: call_tuple,
                    node_info: ni,
                };

                t.success((true, Box::new(cst::ExpressionWrapper::FunctionCall(fc))))
            }
            Token::Dot => {
                // doing a member access
                let ident = t
                    .take_in(&[Token::Identifier, Token::FnOperator])
                    .hint("Any dot access should be followed by a member name")
                    .join()?;

                let end = ident.end;

                let ni = NodeInfo::from_indices(start, end);

                let mae = cst::MemberAccessExpression {
                    on,
                    name: ident.slice,
                    node_info: ni,
                };

                t.success((true, Box::new(cst::ExpressionWrapper::MemberAccess(mae))))
            }
            Token::RBracket => {
                todo!("Array access not yet implemented")
            }
            _ => unreachable!(),
        }
    }

    /// Parses an "operand", where in "x = y + z" x, y, and z are all operands.
    /// In the expression "foo.bar = baz.qux()", both foo.bar and baz.qux() are operands.
    pub fn parse_operand(
        &mut self,
        t: &TokenProvider,
        with_generics: &Vec<IStr>,
    ) -> ExpressionResult {
        let mut t = parse_header!(t);

        //let scope = self.parse_scope(&t).join_hard(&mut t).catch(&mut t)?;

        let mut base = self
            .parse_access_base(&t, with_generics)
            .join_hard(&mut t)
            .catch(&mut t)?;

        loop {
            match self
                .parse_access_continuation(&t, base, with_generics)
                .join_hard(&mut t)
                .catch(&mut t)?
            {
                (true, expr) => {
                    base = expr;
                    continue;
                }
                (false, expr) => {
                    base = expr;
                    break;
                }
            }
        }

        t.success(base)
    }

    /// Parses a literal, tuple, or identifier that forms the "base" of an access or operand.
    /// Any scoping must have been parsed prior
    pub fn parse_access_base(
        &mut self,
        t: &TokenProvider,
        with_generics: &Vec<IStr>,
    ) -> ExpressionResult {
        let mut t = parse_header!(t, [Token::Identifier => 1.0, Token::UnknownIntegerLiteral => 1.0, Token::StringLiteral => 1.0]);

        let tw = t.try_take_if(|tw| {
            (tw.token.is_operand_base() || tw.token.matches(Token::LParen)).then_some(())
        });

        let tw = match tw {
            Some((_, v)) => v,
            None => return t.unexpected_token(&[], "Hint"),
        };

        match tw.token {
            Token::LParen => {
                t.lh.backtrack(); // ungrab the lparen so it can be consumed by tuple
                self.parse_tuple(&t, with_generics)
            }
            Token::Identifier => {
                t.lh.backtrack();

                println!("parsing scope");
                let mut sn = self.parse_scope(&t).join_hard(&mut t).catch(&mut t)?;

                let last = t.take(Token::Identifier).join()?;

                let node_info = sn.node_info.extended(last);

                sn.scope.push(last.slice);

                println!("got scope, turning into ident");
                let e = cst::IdentifierExpression { node_info, ident: sn.to_raw_scope() };

                println!("turned into ident, it is {e:?}");

                println!("info: {}", sn.node_info);

                let e = cst::ExpressionWrapper::Identifier(e);

                //let e = cst::IdentifierExpression::from_token(tw);

                //let remainder = 

                t.success(Box::new(e))
            }
            other if other.is_literal() => {
                let e = cst::LiteralExpression::new_expr(tw);

                t.success(e)
            }
            other => {
                tracing::info!("Got other token: {other:?}");
                unreachable!()
            }
        }
    }

    pub fn parse_tuple(
        &mut self,
        t: &TokenProvider,
        with_generics: &Vec<IStr>,
    ) -> ExpressionResult {
        let mut t =
            parse_header!(t, [Token::LParen => 1.0, Token::Comma => 1.0, Token::RParen => 1.0]);

        let leading = t.take(Token::LParen).join()?;

        let mut exprs = Vec::new();

        while let None = t.peek_for(Token::RParen) {
            let e = self
                .parse_expr(&t, with_generics)
                .join_hard(&mut t)
                .catch(&mut t)
                .handle_here()?;
            let (v, _, _) = e.open_anyway();
            match v {
                Some(e) => exprs.push(e),
                None => (),
            }

            match t.take_in(&[Token::Comma, Token::RParen]).join()?.token {
                Token::Comma => continue, // look for another expression in next loop
                Token::RParen => {
                    // we have a close for the tuple, allow it to be instead taken by the outer
                    t.lh.backtrack();
                    break;
                }
                _ => unreachable!(),
            }
        }

        let trailing = t.take(Token::RParen).join()?;

        //

        //let self.parse_expr(&t)

        let start = leading.start;
        let end = trailing.end;

        let ni = NodeInfo::from_indices(start, end);

        let ex = cst::Tuple::new_expr(ni, exprs);

        t.success(ex)
    }

    pub fn parse_llvm_builtin(
        &mut self,
        t: &TokenProvider,
        with_generics: &Vec<IStr>,
    ) -> ExpressionResult {
        //let mut t = t.child();
        let mut t = parse_header!(t);

        let start = t.take(Token::InteriorBuiltin).join()?;
        //let name = self.hard_expect(Token::Identifier)?;

        //while let Some(llvm_directive) = self.eat_match_in(&[Token::LL_Bind
        let mut bindings = Vec::new();
        let mut vars = Vec::new();

        while let Some(tok) = t.try_take_in(&[Token::LL_Bind, Token::LL_Var]) {
            match tok.token {
                Token::LL_Bind => {
                    let exp = self
                        .parse_expr(&t, with_generics)
                        .join_hard(&mut t)
                        .catch(&mut t)?;
                    let _ = t.take(Token::ThickArrow).join()?;
                    let name = t.take(Token::Identifier).join()?;

                    bindings.push((*exp, name.slice));
                }
                Token::LL_Var => {
                    let name = t.take(Token::Identifier).join()?;

                    vars.push(name.slice);
                }
                _ => panic!("fell out of match while parsing llvm_builtin"),
            }
        }

        let maybe_result = if let Some(_) = t.try_take(Token::LL_Result) {
            let name = t.take(Token::Identifier).join()?;
            t.take(Token::Colon).join()?;
            let tr = self
                .parse_type_specifier(&t, with_generics)
                .join_hard(&mut t)
                .catch(&mut t)?;
            Some((tr.intern(), name.slice))
        } else {
            None
        };

        let body = t.take(Token::LLVMBlock).join()?;

        let llvmle = cst::LLVMLiteralExpression {
            node_info: NodeInfo::from_indices(start.start, body.end),
            bindings,
            vars,
            text: body.slice,
            output: maybe_result,
        };

        t.success(Box::new(cst::ExpressionWrapper::LLVMLiteral(llvmle)))
    }

    pub fn parse_if_then_else(
        &mut self,
        t: &TokenProvider,
        with_generics: &Vec<IStr>,
    ) -> ExpressionResult {
        //let mut t = t.child();
        let mut t = parse_header!(t);

        let start = t
            .take(Token::If)
            .hint("Tried to parse if-else with no beginning if")
            .join()?
            .start;
        let if_exp = self.parse_expr(&t, with_generics).hint("An 'if' must be followed by a conditional expression followed by a then expression").join_hard(&mut t).catch(&mut t)?;
        let then_exp = self
            .parse_expr(&t, with_generics)
            .join_hard(&mut t)
            .catch(&mut t)?;
        let (else_exp, end) = if t.try_take(Token::Else).is_some() {
            let exp = self
                .parse_expr(&t, with_generics)
                .join_hard(&mut t)
                .catch(&mut t)?;
            let end = exp
                .as_node()
                .end()
                .expect("successfully parsed else had no end");

            (exp, end)
        } else {
            let exp = cst::BlockExpression::new_expr(NodeInfo::Builtin, Vec::new());
            let end = then_exp.as_node().end().expect("then had no end?");

            (exp, end)
        };

        let node_info = NodeInfo::from_indices(start, end);

        t.success(cst::IfThenElseExpression::new_expr(
            node_info, if_exp, then_exp, else_exp,
        ))
    }

    pub fn parse_while(
        &mut self,
        t: &TokenProvider,
        with_generics: &Vec<IStr>,
    ) -> ExpressionResult {
        //let mut t = t.child();
        let mut t = parse_header!(t);

        let start = t.take(Token::While).join()?.start;
        let while_exp = self
            .parse_expr(&t, with_generics)
            .join_hard(&mut t)
            .catch(&mut t)?;
        let do_exp = self
            .parse_expr(&t, with_generics)
            .join_hard(&mut t)
            .catch(&mut t)?;

        let node_info = NodeInfo::from_indices(start, do_exp.as_node().end().unwrap_or(start));

        t.success(cst::WhileExpression::new_expr(node_info, while_exp, do_exp))
    }

    pub fn syntactic_block(
        &mut self,
        t: &TokenProvider,
        with_generics: &Vec<IStr>,
    ) -> ExpressionResult {
        let mut t = parse_header!(t, [Token::LBrace => 1.0, Token::Semicolon => 10.0, Token::RBrace => 10.0]);

        /*let mut t = t.child().predict(&[
            (Token::LBrace, 1.0),
            (Token::Semicolon, 10.0),
            (Token::RBrace, 10.0),
        ]);*/

        t.take(Token::LBrace).join()?;
        let mut declarations: Vec<Box<cst::ExpressionWrapper>> = Vec::new();

        let start = t.lh.la(0).map_or(CodeLocation::Builtin, |tw| tw.start);

        //let mut failed = false;

        loop {
            match t.la(0).catch(&mut t).join_hard(&mut t)?.token {
                Token::RBrace => {
                    break;
                }
                Token::Semicolon => {
                    //self.lex.advance();
                    t.take(Token::Semicolon).join()?;
                    t.predict_next((Token::Semicolon, 10.0));
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
                    let e = self
                        .parse_expr(&t, with_generics)
                        .join_hard(&mut t)
                        .catch(&mut t)
                        .handle_here()?;

                    let (v, mut _es, _s) = e.update_solution(&t).open();

                    v.map(|exp| {
                        let exp = match t.try_take(Token::Semicolon) {
                            Some(semi) => {
                                let start =
                                    exp.as_node().start().map_or(CodeLocation::Builtin, |v| v);
                                let end = semi.end;
                                let node_info = NodeInfo::from_indices(start, end);
                                cst::StatementExpression::new_expr(node_info, exp)
                            }
                            None => exp,
                        };

                        declarations.push(exp);
                    });

                    //self.expect(Token::Semicolon)?; // TODO: eval if this is required
                }
            }
        }

        let end = t.lh.la(-1).map_or(start, |tw| tw.start);

        let node_info = NodeInfo::from_indices(start, end);

        t.take(Token::RBrace).join()?;

        t.success(cst::BlockExpression::new_expr(node_info, declarations))
    }

    pub fn parse_expr_inner(
        &mut self,
        min_bp: u32,
        level: usize,
        t: &TokenProvider,
        with_generics: &Vec<IStr>,
    ) -> ExpressionResult {
        //let t1 = self.lex.la(0)?;

        //let mut t = t.child();
        let mut t = parse_header!(t);

        //let t1 = t.sync().la(0).map_err(|e| CorrectionBubblingError::from_fatal_error(e))?;
        let t1 = t.la(0).join_hard(&mut t).catch(&mut t)?;
        /*let t1 = t
        .take_in(&[
            Token::LBracket,
            Token::LBrace,
            Token::If,
            Token::InteriorBuiltin,
            Token::While,
            Token::Let,
        ])
        .join()?;*/
        let mut lhs = match t1.token {
            Token::LBracket => {
                let r = self
                    .parse_array_literal(&t)
                    .join_hard(&mut t)
                    .catch(&mut t)?;

                r
            }
            Token::LBrace => {
                let r = self
                    .syntactic_block(&t, with_generics)
                    .join_hard(&mut t)
                    .catch(&mut t)?;

                r
            }
            Token::If => {
                let r = self
                    .parse_if_then_else(&t, with_generics)
                    .join_hard(&mut t)
                    .catch(&mut t)?;

                r
            }
            Token::InteriorBuiltin => {
                let r = self
                    .parse_llvm_builtin(&t, with_generics)
                    .join_hard(&mut t)
                    .catch(&mut t)?;

                r
            }
            Token::While => {
                let r = self
                    .parse_while(&t, with_generics)
                    .join_hard(&mut t)
                    .catch(&mut t)?;

                r
            }
            Token::Struct => {
                tracing::info!("parsing a struct literal");
                let r = self
                    .parse_struct_literal(&t, with_generics)
                    .join_hard(&mut t)
                    .catch(&mut t)?;

                r
            }
            Token::Let => {
                let r = self
                    .parse_let(&t, with_generics)
                    .join_hard(&mut t)
                    .catch(&mut t)?;

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
                t.lh.advance();

                let bp =
                    prefix_binding_power(tok).expect("bp should already be Some from match guard");
                let rhs = self
                    .parse_expr_inner(bp, level + 1, &t, with_generics)
                    .join_hard(&mut t)
                    .catch(&mut t)?;
                let start = t1.start;
                let end = rhs.as_node().end().expect("parsed rhs has no end?");
                let node_info = NodeInfo::from_indices(start, end);

                self.build_unary(&t, node_info, tok, rhs)
                    .join_hard(&mut t)
                    .catch(&mut t)?
            }
            // for anything that isn't an operator or start of a syntactic feature, so identifiers,
            // patterns, and literals (operands)
            tok if tok.is_operand_base() => {
                /*let ae = self.atomic_expression(&t).join_hard(&mut t).catch(&mut t)?;
                tracing::info!("ae is: {ae:?}");
                ae*/
                let operand = self
                    .parse_operand(&t, with_generics)
                    .join_hard(&mut t)
                    .catch(&mut t)?;
                operand
            }
            _other => {
                return t.failure(ParseResultError::UnexpectedToken(
                        t1, vec![], Some("Was looking for a start of a construct in an expression, found another token instead")));
            }
        };

        loop {
            if let Some(_colon) = t.try_take(Token::Colon) {
                todo!("Type constraints not implemented yet")
            } else if let Some(_as) = t.try_take(Token::As) {
                let typeref: cst::SyntacticTypeReference = self
                    .parse_type_specifier(&t, with_generics)
                    .join_hard(&mut t)
                    .catch(&mut t)?;
                let start = lhs
                    .as_node()
                    .start()
                    .expect("parsed lhs did not have a start");
                let end = typeref.end().expect("parsed typeref did not have an end");
                let node_info = NodeInfo::from_indices(start, end);
                lhs = cst::CastExpression::new_expr(node_info, lhs, Box::new(typeref.intern()));
                continue;
            } else if let Some(_arrow) = t.try_take(Token::ThinArrowLeft) {
                tracing::info!("Parsing implementation expression");
                let v = self
                    .parse_implementation_expression(&t, lhs)
                    .join_hard(&mut t)
                    .catch(&mut t)?;

                lhs = Box::new(v);
                continue;
            } else if let Some(((l_bp, r_bp), tw)) =
                t.try_take_if(|tw| infix_binding_power(tw.token))
            {
                //if let Some(((l_bp, r_bp), tw)) = operator {
                if l_bp < min_bp {
                    //self.lex.backtrack();
                    t.lh.backtrack();

                    break;
                } else {
                    let rhs = self
                        .parse_expr_inner(r_bp, level + 1, &t, with_generics)
                        .join_hard(&mut t)
                        .catch(&mut t)?;
                    let start = lhs.as_node().start().expect("parsed lhs has no start?");
                    let end = rhs.as_node().end().expect("parsed rhs has no end?");
                    let node_info = NodeInfo::from_indices(start, end);
                    lhs = self
                        .build_binary(&t, node_info, tw.token, lhs, rhs)
                        .join_hard(&mut t)
                        .catch(&mut t)?;
                    continue;
                }
            } else if t
                .lh
                .la(0)
                .map(|tw| tw.token.is_operand_base())
                .unwrap_or(false)
            {
                lhs = self
                    .parse_operand(&t, with_generics)
                    .join_hard(&mut t)
                    .catch(&mut t)?;
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
        lhs: Box<cst::ExpressionWrapper>,
    ) -> ExpressionResult {
        match token {
            Token::Ampersand | Token::Asterisk | Token::Dash | Token::Bang => t.success(
                cst::UnaryOperationExpression::new_expr(node_info, token, lhs),
            ),
            //Token::Let => LetExpression::new_expr(node_info, lhs),
            Token::Return => t.success(cst::ReturnExpression::new_expr(node_info, lhs)),
            _ => {
                tracing::info!("got unexpected token {:?}", token);
                panic!("Programming error: no way to build unary expression from given token");
            }
        }
    }

    fn build_binary(
        &mut self,
        t: &TokenProvider,
        node_info: NodeInfo,
        token: Token,
        lhs: Box<cst::ExpressionWrapper>,
        rhs: Box<cst::ExpressionWrapper>,
    ) -> ExpressionResult {
        //let t = t.child();
        let t = parse_header!(t);

        match token {
            Token::Plus | Token::Dash | Token::Asterisk | Token::FSlash => t.success(
                cst::BinaryOperationExpression::new_expr(node_info, token, lhs, rhs),
            ),
            //Token::As => Ok(CastExpression::new_expr(node_info, lhs, rhs)),
            Token::Equals => t.success(cst::AssignmentExpression::new_expr(node_info, lhs, rhs)),
            Token::CmpEqual
            | Token::CmpLessThan
            | Token::CmpGreaterThan
            | Token::CmpLessThanOrEqual
            | Token::CmpGreaterThanOrEqual
            | Token::CmpNotEqual => t.success(cst::ComparisonOperationExpression::new_expr(
                node_info, token, lhs, rhs,
            )),
            tok => {
                tracing::info!("got unexpected token {:?}", token);
                let start = lhs.as_node().node_info().as_parsed().unwrap().span.start;
                let end = rhs.as_node().node_info().as_parsed().unwrap().span.end;
                let msg = format!(
                    "Couldn't build a binary expression, {:?} is not an understood operator yet",
                    tok
                );
                let msg = intern(&msg).resolve();
                return t.failure(ParseResultError::SemanticIssue(msg, start, end));
                //panic!("Programming error: no way to build binary expression from given token");
            }
        }
    }
}

pub enum DotAccess {
    Field(IStr),
    Method(IStr, Vec<Box<cst::ExpressionWrapper>>),
}

use IntoCstNode;

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
