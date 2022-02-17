#[allow(non_upper_case_globals)]
use crate::ast;
use crate::lex::{ParseResultError, Token};

//use crate::helper::lex_wrap::LookaheadStream;
//use crate::helper::lex_wrap::{CodeLocation, ParseResultError};
use crate::lex::{CodeLocation, TokenWrapper};
//use crate::helper::lex_wrap::TokenWrapper;
//use std::collections::HashSet;
use ast::IntoAstNode;

use crate::parse::*;

use super::schema::{
    CorrectionBubblingError, CorrectionBubblingResult, ParseResult, ResultHint, TokenProvider,
};

impl<'lexer> Parser<'lexer> {
    pub fn entry(&mut self) -> Result<ast::OuterScope, ParseResultError> {
        let mut declarations = Vec::new();

        let start = self.lex.la(0).map_or(CodeLocation::Builtin, |tw| tw.start);

        let mut t: TokenProvider = TokenProvider::from_handle(self.lex.clone());

        //let mut failed = false;

        while let Ok(tw) = self.lex.la(0) {
            let _ = match tw.token {
                Token::RBrace => break,
                _ => {
                    //
                    let r = self.parse_global_declaration(&t).hard(&mut t);

                    let _ = match r {
                        Err(e) => {
                            //failed = true;
                            for e in t.errors() {
                                self.report_err(e);
                            }
                            //self.eat_through(vec![Token::RBrace, Token::Semicolon]);
                        }
                        Ok(ok) => {
                            declarations.push(ok.join(&mut t));
                        }
                    };
                }
            };
        }

        let end = self.lex.la(-1).map_or(start, |tw| tw.start);

        Ok(ast::OuterScope {
            declarations,
            node_info: ast::NodeInfo::from_indices(start, end),
        })
    }

    pub fn parse_symbol_specifiers(&mut self, t: &TokenProvider) -> (bool, bool, bool) {
        let mut t = t.child();

        let mut public = Option::None;
        let mut mutable = Option::None;
        let mut dynamic = Option::None;

        while let Some(tok) = t.try_take_in(&[Token::Public, Token::Mutable, Token::Dynamic]) {
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
    pub fn parse_global_declaration(
        &mut self,
        t: &TokenProvider,
    ) -> ParseResult<ast::SymbolDeclaration> {
        let mut t = t.child();
        //let has_pub = self.eat_match(Token::Public);
        let mut failed = false;

        let (public, _mutable, _dynamic) = self.parse_symbol_specifiers(&t);

        t.take_in(&[
            Token::Module,
            Token::Function,
            Token::Struct,
            Token::Let,
            Token::Use,
        ])?;

        if let Ok(tw) = self.lex.la(0) {
            let r = match tw.token {
                Token::Module => self
                    .parse_namespace(&t)
                    .hard(&mut t)
                    .map(|mut ns| {
                        let mut ns = ns.join(&mut t);
                        ns.set_public(public);
                        ast::SymbolDeclaration::NamespaceDeclaration(ns)
                    })
                    .map_err(|e| {
                        failed = true;
                        e
                    }),
                Token::Function => self
                    .parse_function_declaration(&t)
                    .hard(&mut t)
                    .map(|fd| {
                        let fd = fd.join(&mut t);
                        ast::SymbolDeclaration::FunctionDeclaration(fd)
                    })
                    .map_err(|e| {
                        failed = true;
                        e
                    }),
                // TODO: maybe add global variable declaration?
                Token::Struct => self
                    .parse_struct_definition(&t)
                    .hard(&mut t)
                    .map(|sd| {
                        let sd = sd.join(&mut t);
                        ast::SymbolDeclaration::TypeDefinition(sd)
                    })
                    .map_err(|e| {
                        failed = true;
                        e
                    }),
                Token::Use => self
                    .parse_use_declaration(&t)
                    .hard(&mut t)
                    .map(|ud| {
                        let ud = ud.join(&mut t);
                        ast::SymbolDeclaration::UseDeclaration(ud)
                    })
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

                    Err(CorrectionBubblingError::from_fatal_error(ParseResultError::UnexpectedToken(
                        tw,
                        vec![Token::Module, Token::Let, Token::Function],
                        None,
                    )))
                }
            };

            r.and_then(|v| t.success(v))
        } else {
            Err(CorrectionBubblingError::from_fatal_error(
                ParseResultError::EndOfFile,
            ))
        }
    }

    //const first_namespace: [Token; 1] = [Token::Module];
    pub fn parse_namespace(&mut self, t: &TokenProvider) -> ParseResult<ast::Namespace> {
        let mut t = t.child();

        let start = self.lex.la(0).map_or(CodeLocation::Builtin, |tw| tw.start);

        t.take(Token::Module)?;
        let id = t.take(Token::Identifier).hard(&mut t)?.join(&mut t).slice;
        t.take(Token::LBrace)?;
        let pu = self.entry();
        t.take(Token::RBrace)?;

        let end = self.lex.la(-1).map_or(CodeLocation::Builtin, |tw| tw.end);

        //let failed = pu.is_err();

        let node_info = ast::NodeInfo::from_indices(start, end);

        t.success(ast::Namespace {
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
        t: &TokenProvider,
    ) -> ParseResult<Vec<(IStr, ast::TypeReference)>> {
        let mut t = t.child();

        let mut rvec: Vec<(IStr, ast::TypeReference)> = Vec::new();

        while let Some(i) = t.try_take(Token::Identifier) {
            t.take(Token::Colon)?;
            let tr = self.parse_type_specifier(&t).hard(&mut t)?.join(&mut t);

            let r = (i.slice, tr);

            rvec.push(r);

            match t.try_take(Token::Comma) {
                Some(_) => continue,
                None => break,
            }
        }

        t.success(rvec)
    }

    //const first_function: [Token; 1] = [Token::Function];
    pub fn parse_function_declaration(
        &mut self,
        t: &TokenProvider,
    ) -> ParseResult<ast::FunctionDefinition> {
        let mut t = t.child();

        let start = t.take(Token::Function).hard(&mut t)?.join(&mut t).start;
        let function_name = t.take(Token::Identifier).hard(&mut t)?.join(&mut t);

        t.take(Token::LParen)?;
        let params = self.parse_function_param_list(&t).hard(&mut t)?.join(&mut t);
        t.take(Token::RParen)?;

        t.take(Token::ThinArrow)?;
        let return_type = self.parse_type_specifier(&t).hard(&mut t)?.join(&mut t);

        let body = self.parse_expr(&t).hard(&mut t)?.join(&mut t);

        let end = body.as_node().start().expect("Some(_) body has None end");

        let node_info = ast::NodeInfo::from_indices(start, end);

        t.success(ast::FunctionDefinition {
            node_info,
            body,
            params,
            //params: Vec::new(), // TODO
            return_type,
            name: function_name.slice,
            public: false,
        })
    }

    pub fn parse_use_declaration(&mut self, t: &TokenProvider) -> ParseResult<ast::UseDeclaration> {
        let mut t = t.child();

        let start = t.take(Token::Use).hard(&mut t)?.join(&mut t).start;

        let mut scope = Vec::new();

        let tw= t.take_in(&[Token::Identifier, Token::Global, Token::Super])
            .hint("Use statements should only start with an identifier or the 'global' or 'super' keywords").hard(&mut t)?.join(&mut t);

        //let tw = self.lex.next()?;

        /*let first_str = match tw {
        Token::Super | Token::Global | Token::*/
        //let first = self.hard_expect(Token::Identifier)

        scope.push(tw.slice);

        let mut end = tw.end;

        while let Some(_) = t.try_take(Token::DoubleColon) {
            let tw = t.take_in(&[Token::Asterisk, Token::Identifier])
            .hint("Use statements should only either specify a more specific scope (an identifier) or a glob (*) after specifying an initial starting scope").hard(&mut t)?.join(&mut t);

            //let tw = self.lex.next()?;

            scope.push(tw.slice);

            end = tw.end;
        }

        let mut alias: Option<IStr> = None;

        if let Some(_) = t.try_take(Token::As) {
            let id = t.take(Token::Identifier).hard(&mut t)?.join(&mut t); // don't bubble, recoverable

            alias = Some(id.slice);
        }

        end = t.take(Token::Semicolon).hard(&mut t)?.join(&mut t).end; // don't need to directly bubble, since this individual statement is recoverable

        let node_info = ast::NodeInfo::from_indices(start, end);

        t.success(ast::UseDeclaration {
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
