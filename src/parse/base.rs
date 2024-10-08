use std::collections::HashMap;

use either::Either;
use tracing::info;

use crate::ast::types::ParamInfo;
#[allow(non_upper_case_globals)]
//use crate::ast;
//use crate::ast::{StaticVariableDeclaration, TopLevel};
use crate::cst::cst_traits::NodeInfo;
//use crate::cst::declarations::{OuterScope, TopLevel, Namespace, TypeReference, FunctionDefinition};
use crate::cst::{self, FunctionBuiltin, ScopedName, SyntacticTypeReference, SyntacticTypeReferenceRef};
use crate::lex::{ParseResultError, Token};

//use crate::helper::lex_wrap::LookaheadStream;
//use crate::helper::lex_wrap::{CodeLocation, ParseResultError};
use crate::lex::CodeLocation;
//use crate::helper::lex_wrap::TokenWrapper;
//use std::collections::HashSet;
use crate::cst::cst_traits::IntoCstNode;

use crate::mir::scribe::OutputType;
use crate::parse::*;

use super::schema::{ResultHint, TokenProvider};

impl<'lexer> Parser<'lexer> {
    pub fn entry(&mut self, t: &TokenProvider, is_file: bool) -> ParseResult<cst::OuterScope> {
        //let mut t = t.child();
        let mut t = parse_header!(t, [Token::Module => 1.0, Token::Function => 1.0, Token::Struct => 1.0, Token::Use => 1.0, Token::DExpression => 10.0]);

        let mut declarations = Vec::new();

        //let start = self.lex.la(0).map_or(CodeLocation::Builtin, |tw| tw.start);
        let start = t.lh.la(0).map_or(CodeLocation::Builtin, |tw| tw.start);

        //let mut failed = false;
        tracing::info!("At entry, idx: {}", t.lh.index());

        //while let Ok(tw) = t.sync().la(0) {
        loop {
            if t.lh.la(0).is_err() && is_file {
                tracing::info!("breaks out of entry since hit end of file");
                break; // no decs left in file
            }

            let tw = t
                .take_in(&[
                    Token::RBrace,
                    Token::Module,
                    Token::Function,
                    Token::Struct,
                    Token::Use,
                    Token::DExpression,
                    Token::Implementation,
                    Token::Specification,
                ])
                .join()?;

            tracing::info!("Entry: got a tw {:?}", tw);
            match tw.token {
                Token::RBrace => {
                    //t.take(Token::RBrace).join()?;
                    if !is_file || t.sync().la(0).is_err() {
                        tracing::info!("Ending entry since found }}");
                    } else {
                        //
                        let e = ParseResultError::UnexpectedToken(
                            t.sync().la(0).unwrap(),
                            vec![],
                            Some("Found trailing input at end of file"),
                        );
                        tracing::info!("{e:?}");
                        t.add_error(e);
                        //panic!();
                    }
                    tracing::info!("exits from entry since got closing brace");
                    //panic!();
                    break;
                }
                _ => {
                    t.lh.backtrack();

                    tracing::info!("Trying for a global dec");
                    //let r = self.parse_global_declaration(&t);

                    let r = self
                        .parse_global_declaration(&t)
                        .join_hard(&mut t)
                        .catch(&mut t)
                        .handle_here()?;

                    tracing::info!("Got global dec");

                    tracing::info!("___");
                    for e in r.errors().iter() {
                        tracing::info!("{:?}", e);
                    }

                    let (v, mut _es, _s) = r.update_solution(&t).open();
                    tracing::info!("Opened the value");

                    //t.sync_with_solution(s);

                    tracing::info!("T errors:");
                    for e in t.errors() {
                        tracing::info!("{e:?}");
                    }

                    let _ = match v {
                        None => (),
                        Some(ok) => {
                            tracing::info!("Ok");
                            declarations.push(ok);
                        }
                    };
                }
            }
        }

        //let end = self.lex.la(-1).map_or(start, |tw| tw.start);
        let end = t.lh.la(-1).map_or(start, |tw| tw.end);

        if is_file && let Ok(tw) = t.lh.la(0) {
            t.add_error(ParseResultError::UnexpectedToken(tw, vec![], Some("Found trailing input at end of file")));
        }

        t.success(cst::OuterScope {
            declarations,
            node_info: NodeInfo::from_indices(start, end),
        })
    }

    pub fn parse_symbol_specifiers(&mut self, t: &TokenProvider) -> (bool, bool, bool) {
        //let mut t = t.child();
        let mut t = parse_header!(t);

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
    //const first_global: [Token; 3] = [Token::Module, Token::Function, Token::Struct];
    pub fn parse_global_declaration(&mut self, t: &TokenProvider) -> ParseResult<cst::TopLevel> {
        tracing::info!("Parsing global declaration, idx: {}", t.lh.index());

        //let mut t = parse_header!(t, [Token::Module => 1.0, Token::Function => 1.0, Token::Struct => 1.0, Token::Use => 1.0, Token::DExpression => 10.0]);

        let mut t = parse_header!(t);

        //let mut t = t.child().predict(&[(Token::Module, 1.0), (Token::Function, 1.0), (Token::Struct, 1.0), (Token::Use, 1.0), (Token::DExpression, 10.0)]);
        //let has_pub = self.eat_match(Token::Public);
        //let mut failed = false;

        // TODO
        let (public, _mutable, _dynamic) = self.parse_symbol_specifiers(&t);

        /*t.take_in(&[
            Token::Module,
            Token::Function,
            Token::Struct,
            Token::Let,
            Token::Use,
        ]).hard(&mut t)?.join(&mut t);*/

        if let Ok(tw) = t.lh.la(0) {
            let r = match t
                .take_in(&[
                    Token::Module,
                    Token::Function,
                    Token::Struct,
                    Token::Use,
                    Token::DExpression,
                    Token::Implementation,
                    Token::Specification,
                ])
                .join()?
                .token
            {
                Token::Module => {
                    t.lh.backtrack();
                    //todo!("modules");
                    let mut ns = self.parse_namespace(&t).join_hard(&mut t).catch(&mut t)?;
                    ns.set_public(public);
                    cst::TopLevel::Namespace(ns)
                }
                Token::Function => {
                    t.lh.backtrack();

                    //todo!();
                    let fd = self
                        .parse_function_declaration(&t, &vec![], false)
                        .join_hard(&mut t)
                        .catch(&mut t)?;
                    cst::TopLevel::Function(fd)
                }
                // TODO: maybe add global variable declaration?
                Token::Struct => {
                    t.lh.backtrack();
                    let sd = self.parse_struct(&t).join_hard(&mut t).catch(&mut t)?;
                    cst::TopLevel::Struct(sd)
                }
                Token::Implementation => {
                    t.lh.backtrack();

                    let v = self
                        .parse_implementation_block(&t)
                        .join_hard(&mut t)
                        .catch(&mut t)?;
                    cst::TopLevel::Impl(v)
                }

                Token::Specification => {
                    t.lh.backtrack();
                    let v = self
                        .parse_specification(&t)
                        .join_hard(&mut t)
                        .catch(&mut t)?;
                    cst::TopLevel::Trait(v)
                }

                Token::Use => {
                    info!("got a use statement, maybe not right?");
                    t.lh.backtrack();
                    let ud = self
                        .parse_use_declaration(&t)
                        .join_hard(&mut t)
                        .catch(&mut t)?;
                    cst::TopLevel::UseDeclaration(ud)
                }
                Token::DExpression => {
                    tracing::info!("Taking dexpr");
                    //let _ = t.take(Token::DExpression).join()?;

                    t.predict_next((Token::Semicolon, 10.0));

                    tracing::info!("Parsing dbg expr, got first tok");
                    let e = self
                        .parse_expr(&t, &Vec::new())
                        .join_hard(&mut t)
                        .catch(&mut t)
                        .handle_here()?;
                    tracing::info!("Got expr");
                    let _ = t.take(Token::Semicolon).join()?;
                    tracing::info!("Took semi");
                    tracing::info!("Errors in e: {:?}", e.errors());
                    //println!("Took semi");
                    //println!("{:?}", e);
                    //println!("E: {:?}", e);
                    tracing::info!("About to update solution for e, idx is {}", t.lh.index());
                    let e = e.update_solution(&t)?;
                    tracing::info!("Value of e: {e:#?}");
                    cst::TopLevel::ExpressionDeclaration(e)
                }

                // only parse let expressions for now, since other (pure) expressions would be
                // useless
                /*Token::Let => self.parse_static_declaration().map(|sd| {
                    let mut ed = ast::SymbolDeclaration::ExpressionDeclaration(sd);
                    if has_pub.is_some() {
                        ed.mark_public()
                    }
                    ed
                }),*/
                tw => {
                    // may be expression?
                    tracing::info!("Token was not as expected for a global declaration");

                    tracing::info!("Token was: {tw:?}");
                    panic!("bad take")

                    /*return t.failure(ParseResultError::UnexpectedToken(
                        tw,
                        vec![
                            Token::Module,
                            Token::Let,
                            Token::Function,
                            Token::Struct,
                            Token::DExpression,
                        ],
                        None,
                    ));*/
                }
            };

            t.try_take(Token::Semicolon); // take any trailing semi for a decl

            t.success(r)

            //r.and_then(|v| t.success(v))
        } else {
            t.failure(ParseResultError::EndOfFile)
        }
    }

    //const first_namespace: [Token; 1] = [Token::Module];
    pub fn parse_namespace(&mut self, t: &TokenProvider) -> ParseResult<cst::NamespaceDefinition> {
        tracing::info!("Parsing a namespace");

        //let mut t = t.child().predict(&[(Token::Module, 10.0), (Token::Identifier, 0.01), (Token::LBrace, 0.5), (Token::RBrace, 0.5)]);
        let mut t = parse_header!(t, [Token::Module => 10.0, Token::Identifier => 0.01, Token::LBrace => 0.5, Token::RBrace => 0.5]);

        //let start = t.la(0).join_hard(&mut t).catch(&mut t).try_get().map_or(CodeLocation::Builtin, |tw| tw.start);

        tracing::info!("Idx is: {}", t.lh.index());

        let start = t.take(Token::Module).join()?.start;

        tracing::info!("Idx is: {}", t.lh.index());

        tracing::info!("Pulled a mod");
        let id = t.take(Token::Identifier).join()?.slice;
        tracing::info!("Pulled an id");
        t.take(Token::LBrace).join()?;
        tracing::info!("Pulling entry");

        let pu = self.entry(&t, false).join_hard(&mut t).catch(&mut t)?;

        //t.take(Token::RBrace).join()?; // entry already takes the rbrace for us
        tracing::info!("got rbrace");

        let end = self.lex.la(-1).map_or(CodeLocation::Builtin, |tw| tw.end);

        //let failed = pu.is_err();

        let node_info = NodeInfo::from_indices(start, end);

        t.success(cst::NamespaceDefinition {
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
        with_generics: &Vec<IStr>,
    ) -> ParseResult<Vec<ParamInfo>> {
        //let mut t = t.child();
        let mut t = parse_header!(t);

        let mut rvec: Vec<_> = Vec::new();

        while let Some(i) = t.try_take(Token::Identifier) {
            let tr = if let Some(i) = t.try_take(Token::Colon) {
                let tr = self
                    .parse_type_specifier(&t, with_generics)
                    .join_hard(&mut t)
                    .catch(&mut t)?
                    .intern();

                tr
            } else {
                SyntacticTypeReference::unconstrained().intern()
            };

            let br = t.try_take_in(&[Token::IsRef, Token::IsNoRef]);

            let br = match br.map(|tw| tw.token) {
                None => false,
                Some(Token::IsNoRef) => false,
                Some(Token::IsRef) => true,
                _ => unreachable!()
            };

            //let r = (i.slice, tr);
            let r = ParamInfo { typ: tr, name: i.slice, byref: br };

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
        parent_generics: &Vec<IStr>,
        is_method: bool,
    ) -> ParseResult<cst::FunctionDefinition> {
        //let mut t = t.child();
        let mut t = parse_header!(t);

        let start = t.take(Token::Function).join()?.start;
        let function_name = t.take_in(&[Token::Identifier, Token::FnOperator]).join()?;

        let inner_generics = self
            .parse_generic_param_list(&t)
            .join_hard(&mut t)
            .catch(&mut t)?;

        //let mut generics = parent_generics.clone();
        tracing::info!("pass through generics later, fix things now");

        let mut generics = Vec::new();

        for (inner, _strefref) in inner_generics.iter() {
            if parent_generics.contains(&inner) {
                tracing::error!("user tried to shadow generics, prevent this at some point");
            }

            //panic!("added generic {inner}");

            generics.push(*inner);
        }

        t.take(Token::LParen).join()?;
        let params = self
            .parse_function_param_list(&t, &generics)
            .join_hard(&mut t)
            .catch(&mut t)?;
        let p_end = t.take(Token::RParen).join()?.end;

        let return_type = if let Some(a) = t.try_take(Token::ThinArrow) {
            self.parse_type_specifier(&t, &generics)
                .join_hard(&mut t)
                .catch(&mut t)?
                .intern()
        } else {
            SyntacticTypeReferenceRef::from_std("std::Unit")
        };

        //if let Some(v) =
        //
        let (body, end) = if let Some(v) = t.try_take(Token::InteriorBuiltin) {
            let fast = t.take(Token::StringLiteral).join()?;
            let silly = t.take(Token::StringLiteral).join()?;
            let slow = t.take(Token::StringLiteral).join()?;

            let end = slow.end;

            let fast = fast.slice.drop_front(1).drop_end(1);
            let silly = silly.slice.drop_front(1).drop_end(1);
            let slow = slow.slice.drop_front(1).drop_end(1);

            let mut hm = HashMap::new();

            hm.insert(OutputType::FullInf(), fast);
            hm.insert(OutputType::AssumeTypeSafe(), silly);
            hm.insert(OutputType::AssumeTypeUnsafe(), slow);

            let fd = FunctionBuiltin { impls: hm };

            (Either::Right(fd), end)
        } else {
            let body = self
                .parse_expr(&t, &generics)
                .join_hard(&mut t)
                .catch(&mut t)?;
            let end = body.as_node().start().expect("Some(_) body has None end");
            (Either::Left(body), end)
        };

        let info = NodeInfo::from_indices(start, end);

        let header_end = return_type
            .resolve()
            .unwrap()
            .info
            .as_parsed()
            .map(|ni| ni.span.end)
            .unwrap_or(p_end);

        let header = NodeInfo::from_indices(start, header_end);

        t.success(cst::FunctionDefinition {
            info,
            body,
            params,
            is_method,
            //params: Vec::new(), // TODO
            return_type,
            name: function_name.slice,
            public: false,
            generics: inner_generics,
            header,
        })
    }

    pub fn parse_use_declaration(&mut self, t: &TokenProvider) -> ParseResult<cst::UseDeclaration> {
        //let mut t = t.child();
        let mut t = parse_header!(t);

        let start = t.take(Token::Use).join()?.start;

        let mut scope = Vec::new();

        let tw= t.take(Token::Identifier)
            .hint("Use statements should only start with an identifier or the 'global' or 'super' keywords").join()?;

        //let tw = self.lex.next()?;

        /*let first_str = match tw {
        Token::Super | Token::Global | Token::*/
        //let first = self.hard_expect(Token::Identifier)

        scope.push(tw.slice);

        //let mut end = tw.end;

        while let Some(_) = t.try_take(Token::DoubleColon) {
            let tw = t.take_in(&[Token::Asterisk, Token::Identifier])
            .hint("Use statements should only either specify a more specific scope (an identifier) or a glob (*) after specifying an initial starting scope").join()?;

            //let tw = self.lex.next()?;

            scope.push(tw.slice);

            //end = tw.end;
        }

        let mut alias: Option<IStr> = None;

        if let Some(_) = t.try_take(Token::As) {
            let id = t.take(Token::Identifier).join()?; // don't bubble, recoverable

            alias = Some(id.slice);
        }

        let end = t.take(Token::Semicolon).join()?.end; // don't need to directly bubble, since this individual statement is recoverable

        let node_info = NodeInfo::from_indices(start, end);

        let scope = ScopedName::new(scope);

        t.success(cst::UseDeclaration {
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
