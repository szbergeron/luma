use crate::ast;
use crate::lex::Token;

//use crate::helper::lex_wrap::LookaheadStream;
use crate::helper::lex_wrap::{CodeLocation, ParseResultError};
//use crate::helper::lex_wrap::TokenWrapper;
//use std::collections::HashSet;
use ast::IntoAstNode;
use std::sync::{Arc, RwLock};

use crate::parse::*;

impl<'b, 'a> Parser<'b, 'a> {
    pub fn entry(&mut self) -> Result<ast::OuterScope<'a>, ParseResultError<'a>> {
        //let mut declarations
        let mut declarations: Vec<
            Arc<RwLock<Result<ast::SymbolDeclaration<'a>, ParseResultError<'a>>>>,
        > = Vec::new();

        let start = self.lex.la(0).map_or(CodeLocation::Builtin, |tw| tw.start);

        let mut failed = false;

        while let Ok(tw) = self.lex.la(0) {
            let r = match tw.token {
                Token::RBrace => break,
                _ => {
                    //
                    let r = self.global_declaration();

                    let r = match r {
                        Err(e) => {
                            failed = true;
                            self.report_err(e.clone());
                            self.eat_through(vec![Token::RBrace, Token::Semicolon]);

                            Err(e)
                        }
                        Ok(ok) => Ok(ok),
                    };

                    Arc::new(RwLock::new(r))
                }
            };

            declarations.push(r);
        }

        let end = self.lex.la(-1).map_or(start, |tw| tw.start);

        Ok(ast::OuterScope {
            declarations,
            node_info: ast::NodeInfo::from_indices(failed, start, end),
        })
    }

    pub fn parse_struct_declaration(
        &mut self,
    ) -> Result<ast::StructDeclaration<'a>, ParseResultError<'a>> {
        let start = self.expect(Token::Struct)?.start;
        let id = self.expect(Token::Identifier)?.slice;
        let mut typeparams = Vec::new();
        if let Some(_lt) = self.eat_match(Token::CmpLessThan) {
            while let Some(id) = self.eat_match(Token::CmpLessThan) {
                typeparams.push(id.slice);
                if let Some(_comma) = self.eat_match(Token::Comma) {
                    continue;
                } else {
                    break;
                }
            }
            self.expect(Token::CmpGreaterThan)?;
        }
        self.expect(Token::LBrace)?;

        let mut fields = Vec::new();

        while let Some(field) = self.eat_match(Token::Identifier) {
            self.expect(Token::Colon)?;
            let field_type = self.type_reference()?;
            let expr = if self.eat_match(Token::Equals).is_some() {
                Some(self.parse_expr()?)
            } else {
                None
            };

            fields.push((field.slice, field_type, expr));

            if let Some(_comma) = self.eat_match(Token::Comma) {
                continue;
            } else {
                break;
            }
        }

        let end = self.expect(Token::RBrace)?.end;

        let node_info = ast::NodeInfo::from_indices(true, start, end);

        Ok(ast::StructDeclaration {
            node_info,
            typeparams,
            fields,
            name: id,
            public: false,
        })
    }

    pub fn global_declaration(
        &mut self,
    ) -> Result<ast::SymbolDeclaration<'a>, ParseResultError<'a>> {
        let has_pub = self.eat_match(Token::Public);
        let mut failed = false;

        if let Ok(tw) = self.lex.la(0) {
            let r = match tw.token {
                Token::Module => self
                    .namespace()
                    .map(|mut ns| {
                        ns.set_public(has_pub.is_some());
                        ast::SymbolDeclaration::NamespaceDeclaration(ns)
                    })
                    .map_err(|e| {
                        failed = true;
                        e
                    }),
                Token::Function => self
                    .function_declaration()
                    .map(|fd| ast::SymbolDeclaration::FunctionDeclaration(fd))
                    .map_err(|e| {
                        failed = true;
                        e
                    }),
                // TODO: maybe add global variable declaration?
                Token::Struct => self
                    .parse_struct_declaration()
                    .map(|sd| ast::SymbolDeclaration::StructDeclaration(sd))
                    .map_err(|e| {
                        failed = true;
                        e
                    }),
                _ => {
                    self.eat_to(vec![Token::RBrace, Token::Semicolon]);

                    self.err(ParseResultError::UnexpectedToken(
                        tw,
                        vec![Token::Module, Token::Let, Token::Function],
                    ))
                }
            };

            r
        } else {
            Err(ParseResultError::EndOfFile)
        }
    }

    pub fn namespace(&mut self) -> Result<ast::Namespace<'a>, ParseResultError<'a>> {
        let start = self.lex.la(0).map_or(CodeLocation::Builtin, |tw| tw.start);

        self.expect(Token::Module)?;
        let id = self.expect(Token::Identifier)?.slice;
        self.expect(Token::LBrace)?;
        let pu = self.entry();
        self.expect(Token::RBrace)?;

        let end = self.lex.la(-1).map_or(CodeLocation::Builtin, |tw| tw.end);

        let failed = pu.is_err();

        let node_info = ast::NodeInfo::from_indices(failed, start, end);

        Ok(ast::Namespace {
            name: Some(id),
            contents: pu,
            public: false,
            node_info,
        })
    }

    fn type_reference_inner(
        &mut self,
    ) -> Result<Option<ast::TypeReference<'a>>, ParseResultError<'a>> {
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

                        end = self.expect(Token::CmpGreaterThan)?.end;

                        params
                    }
                };

                let node_info = ast::NodeInfo::from_indices(true, start, end);

                let tr = ast::TypeReference {
                    typename: tn.slice,
                    type_parameters: type_param_list,
                    refers_to: None,
                    node_info,
                };

                Ok(Some(tr))
            }
        }
    }

    pub fn type_reference(&mut self) -> Result<ast::TypeReference<'a>, ParseResultError<'a>> {
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
    }

    pub fn function_param_list(
        &mut self,
    ) -> Result<Vec<(Box<ast::ExpressionWrapper<'a>>, ast::TypeReference<'a>)>, ParseResultError<'a>>
    {
        let mut rvec: Vec<(Box<ast::ExpressionWrapper<'a>>, ast::TypeReference<'a>)> = Vec::new();
        while let Ok(a) = self.atomic_expression() {
            self.expect(Token::Colon)?;
            let tr = self.type_reference()?;

            let r = (a, tr);

            rvec.push(r);

            match self.eat_match(Token::Comma) {
                Some(_) => continue,
                None => break,
            }
        }

        Ok(rvec)
    }

    pub fn function_declaration(
        &mut self,
    ) -> Result<ast::FunctionDeclaration<'a>, ParseResultError<'a>> {
        let start = self.expect(Token::Function)?.start;
        let function_name = self.expect(Token::Identifier)?;
        self.expect(Token::LParen)?;
        let params = self.function_param_list()?;
        self.expect(Token::RParen)?;

        self.expect(Token::ThinArrow)?;
        let return_type = self.type_reference()?;

        let body = self.parse_expr()?;

        let end = body.as_node().start().expect("Some(_) body has None end");

        let node_info = ast::NodeInfo::from_indices(true, start, end);

        //let _ = self.eat_match(Token::Semicolon);

        Ok(ast::FunctionDeclaration {
            node_info,
            body,
            params,
            return_type,
            name: function_name.slice,
            public: false,
        })
    }

    pub fn closure(&mut self) -> Result<ast::Closure<'a>, ParseResultError<'a>> {
        panic!()
    }
}
