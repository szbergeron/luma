#[allow(non_upper_case_globals)]
use crate::ast;
use crate::lex::Token;

//use crate::helper::lex_wrap::LookaheadStream;
use crate::helper::lex_wrap::{CodeLocation, ParseResultError};
//use crate::helper::lex_wrap::TokenWrapper;
//use std::collections::HashSet;
use ast::IntoAstNode;

use crate::parse::*;

impl<'lexer> Parser<'lexer> {
    pub fn entry(&mut self) -> Result<ast::OuterScope, ParseResultError> {
        let mut declarations = Vec::new();

        let start = self.lex.la(0).map_or(CodeLocation::Builtin, |tw| tw.start);

        //let mut failed = false;

        let sync = self.sync_next(&[Token::RBrace]);

        while let Ok(tw) = self.lex.la(0) {
            let _ = match tw.token {
                Token::RBrace => break,
                _ => {
                    //
                    let sync = self.sync_next(&Self::first_global);
                    let r = self.parse_global_declaration();
                    self.unsync(sync)?;

                    let _ = match r {
                        Err(e) => {
                            //failed = true;
                            self.report_err(e.clone());
                            //self.eat_through(vec![Token::RBrace, Token::Semicolon]);
                        }
                        Ok(ok) => {
                            declarations.push(ok);
                        }
                    };
                }
            };
        }

        self.unsync(sync)?;

        let end = self.lex.la(-1).map_or(start, |tw| tw.start);

        Ok(ast::OuterScope {
            declarations,
            node_info: ast::NodeInfo::from_indices(start, end),
        })
    }

    pub fn parse_symbol_specifiers(&mut self) -> (bool, bool, bool) {
        let mut public = Option::None;
        let mut mutable = Option::None;
        let mut dynamic = Option::None;

        while let Some(tok) = self.eat_match_in(&[Token::Public, Token::Mutable, Token::Dynamic]) {
            match tok.token {
                Token::Dynamic | Token::Nodynamic => {
                    match dynamic.replace(if let Token::Dynamic = tok.token {
                        true
                    } else {
                        false
                    }) {
                        Some(_val) => self.report_err(ParseResultError::SemanticIssue(
                            "tried to mark a symbol dyn/nodyn when dyn/nodyn was already specified",
                            tok.start,
                            tok.end,
                        )),
                        None => {}
                    }
                }
                Token::Mutable | Token::Immutable => {
                    match mutable.replace(if let Token::Mutable = tok.token {
                        true
                    } else {
                        false
                    }) {
                        Some(_val) => self.report_err(ParseResultError::SemanticIssue(
                            "tried to mark a symbol mut/nomut when mut/nomut was already specified",
                            tok.start,
                            tok.end,
                        )),
                        None => {}
                    }
                }
                Token::Public | Token::Private => {
                    match public.replace(if let Token::Public = tok.token {
                        true
                    } else {
                        false
                    }) {
                        Some(_val) => self.report_err(ParseResultError::SemanticIssue(
                            "tried to mark a symbol pub/nopub when pub/nopub was already specified",
                            tok.start,
                            tok.end,
                        )),
                        None => {}
                    }
                }
                _ => {
                    unreachable!("Guarded by eat_match_in")
                }
            }
        }

        (
            public.unwrap_or(false),
            mutable.unwrap_or(false),
            dynamic.unwrap_or(false),
        )
    }

    /*pub fn parse_where_clause(
        &mut self,
    ) -> Result<Vec<ast::TypeConstraint<'input>>, ParseResultError<'input>> {
        todo!()
    }*/
    #[allow(non_upper_case_globals)]
    const first_global: [Token; 3] = [Token::Module, Token::Function, Token::Struct];
    pub fn parse_global_declaration(&mut self) -> Result<ast::SymbolDeclaration, ParseResultError> {
        //let has_pub = self.eat_match(Token::Public);
        let mut failed = false;

        let (public, _mutable, _dynamic) = self.parse_symbol_specifiers();

        self.expect_next_in(&[
            Token::Module,
            Token::Function,
            Token::Struct,
            Token::Let,
            Token::Use,
        ])?;

        if let Ok(tw) = self.lex.la(0) {
            let r = match tw.token {
                Token::Module => self
                    .parse_namespace()
                    .map(|mut ns| {
                        ns.set_public(public);
                        ast::SymbolDeclaration::NamespaceDeclaration(ns)
                    })
                    .map_err(|e| {
                        failed = true;
                        e
                    }),
                Token::Function => self
                    .parse_function_declaration()
                    .map(|fd| ast::SymbolDeclaration::FunctionDeclaration(fd))
                    .map_err(|e| {
                        failed = true;
                        e
                    }),
                // TODO: maybe add global variable declaration?
                Token::Struct => self
                    .parse_struct_definition()
                    .map(|sd| ast::SymbolDeclaration::TypeDefinition(sd))
                    .map_err(|e| {
                        failed = true;
                        e
                    }),
                Token::Use => self
                    .parse_use_declaration()
                    .map(|ud| ast::SymbolDeclaration::UseDeclaration(ud))
                    .map_err(|e| {
                        failed = true;
                        e
                    }),

                // only parse let expressions for now, since other (pure) expressions would be
                // useless
                /*Token::Let => self.parse_static_declaration().map(|sd| {
                    let mut ed = ast::SymbolDeclaration::ExpressionDeclaration(sd);
                    if has_pub.is_some() {
                        ed.mark_public()
                    }
                    ed
                }),*/
                _ => {
                    // may be expression?

                    self.err(ParseResultError::UnexpectedToken(
                        tw,
                        vec![Token::Module, Token::Let, Token::Function],
                        None,
                    ))
                }
            };

            r
        } else {
            Err(ParseResultError::EndOfFile)
        }
    }

    //const first_namespace: [Token; 1] = [Token::Module];
    pub fn parse_namespace(&mut self) -> Result<ast::Namespace, ParseResultError> {
        let start = self.lex.la(0).map_or(CodeLocation::Builtin, |tw| tw.start);

        self.hard_expect(Token::Module)?;
        let id = self.hard_expect(Token::Identifier)?.slice;
        self.hard_expect(Token::LBrace)?;
        let pu = self.entry();
        self.hard_expect(Token::RBrace)?;

        let end = self.lex.la(-1).map_or(CodeLocation::Builtin, |tw| tw.end);

        //let failed = pu.is_err();

        let node_info = ast::NodeInfo::from_indices(start, end);

        Ok(ast::Namespace {
            name: Some(id),
            contents: pu,
            public: false,
            node_info,
        })
    }

    /*fn type_reference_inner(
        &mut self,
    ) -> Result<Option<ast::TypeReference>, ParseResultError> {
        let typename = self.eat_match(Token::Identifier);
        match typename {
            None => Ok(None),
            Some(tn) => {
                let type_param_open = self.eat_match(Token::CmpLessThan);
                let start = tn.start;
                let mut end = tn.end;
                let type_param_list: Vec<ast::TypeReference> = match type_param_open {
                    None => Vec::new(),
                    Some(_arrow) => {
                        let mut params = Vec::new();

                        while let Ok(Some(tr)) = self.type_reference_inner() {
                            params.push(tr);
                            match self.eat_match(Token::Comma) {
                                None => break,
                                Some(_) => continue,
                            }
                        }

                        end = self.hard_expect(Token::CmpGreaterThan)?.end;

                        params
                    }
                };

                let node_info = ast::NodeInfo::from_indices(start, end);

                todo!("Type references not implemented");

                /*let tr = ast::TypeReference {
                    typename: tn.slice,
                    type_parameters: type_param_list,
                    refers_to: None,
                    node_info,
                };*/

                //Ok(Some(tr))
            }
        }
    }

    pub fn type_reference(
        &mut self,
    ) -> Result<ast::TypeReference<'input>, ParseResultError<'input>> {
        let index = self
            .lex
            .la(0)
            .map(|tw| tw.start)
            .unwrap_or(CodeLocation::Builtin);
        match self.type_reference_inner() {
            Err(e) => Err(e),
            Ok(None) => Err(ParseResultError::SemanticIssue(
                "expected a type reference expression, but none was found",
                index,
                index.offset_by(0, 1),
            )),
            Ok(Some(tr)) => Ok(tr),
        }
    }*/

    #[allow(dead_code, unreachable_code)]
    pub fn parse_function_param_list(
        &mut self,
    ) -> Result<Vec<(IStr, ast::TypeReference)>, ParseResultError> {
        let mut rvec: Vec<(IStr, ast::TypeReference)> = Vec::new();

        while let Some(i) = self.eat_match(Token::Identifier) {
            self.hard_expect(Token::Colon)?;
            let tr = self.parse_type_specifier()?;

            let r = (i.slice, tr);

            rvec.push(r);

            match self.eat_match(Token::Comma) {
                Some(_) => continue,
                None => break,
            }
        }

        Ok(rvec)
    }

    //const first_function: [Token; 1] = [Token::Function];
    pub fn parse_function_declaration(
        &mut self,
    ) -> Result<ast::FunctionDefinition, ParseResultError> {
        let start = self.hard_expect(Token::Function)?.start;
        let function_name = self.hard_expect(Token::Identifier)?;

        self.hard_expect(Token::LParen)?;
        let params = self.parse_function_param_list()?;
        self.hard_expect(Token::RParen)?;

        self.hard_expect(Token::ThinArrow)?;
        let return_type = self.parse_type_specifier()?;

        let body = self.parse_expr()?;

        let end = body.as_node().start().expect("Some(_) body has None end");

        let node_info = ast::NodeInfo::from_indices(start, end);

        Ok(ast::FunctionDefinition {
            node_info,
            body,
            params,
            //params: Vec::new(), // TODO
            return_type,
            name: function_name.slice,
            public: false,
        })
    }

    pub fn parse_use_declaration(&mut self) -> Result<ast::UseDeclaration, ParseResultError> {
        let start = self.hard_expect(Token::Use)?.start;

        let mut scope = Vec::new();

        self.expect_next_in(&[Token::Identifier, Token::Global, Token::Super])
            .hint("Use statements should only start with an identifier or the 'global' or 'super' keywords")?;
        let tw = self.lex.next()?;

        /*let first_str = match tw {
        Token::Super | Token::Global | Token::*/
        //let first = self.hard_expect(Token::Identifier)

        scope.push(tw.slice);

        let mut end = tw.end;

        while let Some(_) = self.eat_match(Token::DoubleColon) {
            self.expect_next_in(&[Token::Asterisk, Token::Identifier])
            .hint("Use statements should only either specify a more specific scope (an identifier) or a glob (*) after specifying an initial starting scope")?;

            let tw = self.lex.next()?;

            scope.push(tw.slice);

            end = tw.end;
        }

        let mut alias: Option<IStr> = None;

        if let Some(_) = self.eat_match(Token::As) {
            let id = self.hard_expect(Token::Identifier)?; // don't bubble, recoverable

            alias = Some(id.slice);
        }

        end = self.hard_expect(Token::Semicolon)?.end; // don't need to directly bubble, since this individual statement is recoverable

        let node_info = ast::NodeInfo::from_indices(start, end);

        Ok(ast::UseDeclaration {
            public: false,
            scope,
            node_info,
            alias,
        })
    }

    /*pub fn builtin_declaration(
        &mut self,
    ) -> Result<ast::BuiltinDeclaration, ParseResultError> {
    }*/

    /*pub fn closure(&mut self) -> Result<ast::Closure, ParseResultError> {
        todo!("inline closures not implemented yet");
    }*/
}
