//use crate::ast::base::IntoAstNode;

use crate::cst::{self, CstNode, IntoCstNode, NodeInfo, ScopedName, SyntacticTypeReference};

/*use cst::{
    self, FieldMember, Implementation, ImplementationBody, ImplementationItem, MemberAttributes,
};*/
use crate::lex::{ErrorSet, Token};
use crate::parse::*;

use super::schema::{ResultHint, TokenProvider};
//use ast::base::*;
//use ast::expressions::*;
//use ast::outer::*;

impl<'lexer> Parser<'lexer> {
    /// Currently unconditionally returns memberattrs as they are null-deriving
    pub fn parse_member_attributes(&mut self, t: &mut TokenProvider) -> cst::MemberAttributes {
        let public = t.try_take(Token::Public).is_some();
        let mutable = t.try_take(Token::Mutable).is_some();

        cst::MemberAttributes { public, mutable }
    }

    pub fn parse_implementation_expression(
        &mut self,
        t: &TokenProvider,
        lhs: Box<cst::ExpressionWrapper>,
    ) -> ParseResult<cst::ExpressionWrapper> {
        let mut t = parse_header!(t);

        let start = lhs.as_node().node_info().as_parsed().unwrap().span.start;

        let impl_block = self
            .parse_implementation_block_expression(&t)
            .join_hard(&mut t)
            .catch(&mut t)?;

        let end = impl_block.node_info().as_parsed().unwrap().span.end;

        let node_info = NodeInfo::from_indices(start, end);

        t.success(cst::ExpressionWrapper::ImplementationModification(
            cst::ImplementationModificationExpression {
                modifying: lhs,
                node_info,
                impl_block: box impl_block,
            },
        ))
    }

    pub fn parse_field(&mut self, t: &TokenProvider) -> ParseResult<cst::Field> {
        println!("Parsing a field");
        let mut t = parse_header!(t);

        let start = t.take(Token::Var).join()?.start;

        let name = t.take(Token::Identifier).hint("A field declaration should have the name of the field directly following the <var> keyword").join()?;

        t.take(Token::Colon).join()?;

        let has_type = self
            .parse_type_specifier(&t)
            .join_hard(&mut t)
            .catch(&mut t)?;

        let end = has_type.as_node().node_info().as_parsed().unwrap().span.end;

        //let end = t.take(Token::Semicolon).join()?.end;

        let info = NodeInfo::from_indices(start, end);

        t.success(cst::Field {
            info,
            has_type,
            has_name: name.slice,
        })
    }

    pub fn parse_generic_param_list(
        &mut self,
        t: &TokenProvider,
    ) -> ParseResult<Vec<(IStr, SyntacticTypeReference)>> {
        let mut t = parse_header!(t);

        let mut params = Vec::new();

        match t.try_take(Token::CmpLessThan) {
            Some(_) => loop {
                let id = t.take(Token::Identifier).join()?;

                let pair = (
                    id.slice,
                    SyntacticTypeReference {
                        info: NodeInfo::Builtin,
                        inner: cst::SyntacticTypeReferenceInner::Unconstrained(),
                    },
                );

                params.push(pair);

                match t
                    .take_in(&[Token::Comma, Token::CmpGreaterThan])
                    .join()?
                    .token
                {
                    Token::Comma => continue,
                    Token::CmpGreaterThan => break,
                    _ => unreachable!(),
                }
            },
            None => (),
        }

        t.success(params)
    }

    pub fn parse_struct(&mut self, t: &TokenProvider) -> ParseResult<cst::StructDefinition> {
        let mut t = parse_header!(t, [Token::Struct => 10, Token::Identifier => 1, Token::LBrace => 10, Token::Comma => 10, Token::RBrace => 10]);

        let start = t.take(Token::Struct).join()?.start;
        let mut end = start;

        let name = t.take(Token::Identifier).join()?.slice;

        let generics = self
            .parse_generic_param_list(&t)
            .join_hard(&mut t)
            .catch(&mut t)?;

        let mut fields = Vec::new();

        t.take(Token::LBrace).join()?;

        //while let Ok(Token::Var) = t.lh.la(0).map(|tw| tw.token) {
        while let next = t.take_in(&[Token::Var, Token::RBrace]).join()? {
            match next.token {
                Token::RBrace => {
                    println!("Got a brace");
                    break;
                }
                Token::Var => {
                    t.lh.backtrack();
                    println!("getting a var");
                }
                _ => unreachable!(),
            }

            let field = self
                .parse_field(&t)
                .join_hard(&mut t)
                .catch(&mut t)
                .handle_here()?
                .update_solution(&t)
                .try_get();

            //fields.push(field);
            if let Some(v) = field {
                fields.push(v);
            }

            let ending = t.take_in(&[Token::Comma, Token::RBrace]).join()?;
            match ending.token {
                Token::Comma => {
                    t.predict_next((Token::Comma, 10.0));
                    continue;
                }
                Token::RBrace => {
                    end = ending.end;
                    break;
                }
                _ => unreachable!(),
            }
        }

        t.success(cst::StructDefinition {
            info: NodeInfo::from_indices(start, end),
            fields,
            name,
            generics,
            public: true, // TODO: parse public/private on structs
        })
    }

    pub fn parse_implementation_body(
        &mut self,
        t: &TokenProvider,
    ) -> ParseResult<(Vec<cst::Field>, Vec<cst::FunctionDefinition>, NodeInfo)> {
        let mut t = parse_header!(t, [Token::LBrace => 1.0, Token::Function => 10.0, Token::Var => 10.0, Token::RBrace => 10.0]);

        let start = t.take(Token::LBrace).join()?.start;
        let end;

        let mut fields = Vec::new();
        let mut functions = Vec::new();

        loop {
            //let expected = [Token::RBrace, Token::Function, Token::Var];

            let tw = t
                .take_in(&[Token::RBrace, Token::Function, Token::Var])
                .join()?;

            //match t.la(0).join_hard(&mut t).catch(&mut t)?.token {
            match tw.token {
                Token::Function => {
                    let _ = t.lh.prev();

                    let f = self
                        .parse_function_declaration(&t)
                        .join_hard(&mut t)
                        .catch(&mut t)
                        .handle_here()?;

                    let (v, _e, _s) = f.update_solution(&t).open();

                    match v {
                        Some(v) => {
                            functions.push(v);
                        }
                        None => (),
                    }
                }
                Token::Var => {
                    let f = self.parse_field(&t).join_hard(&mut t).catch(&mut t)?; // TODO: fix ya error handlin'
                    fields.push(f);
                    /*let ident = t.take(Token::Identifier).join()?;
                    let _ = t.take(Token::Colon).join()?;
                    let tr = self
                        .parse_type_specifier(&t)
                        .join_hard(&mut t)
                        .catch(&mut t)?;

                    fields.push(cst::Field {
                        has_name: ident.slice,
                        has_type: tr,
                    });*/
                }
                Token::RBrace => {
                    end = t.lh.la(-1).unwrap().end;
                    //end = t.take(Token::RBrace).join()?.end;
                    break;
                }
                _ => unreachable!(), //_ => return t.unexpected_token(&expected, "Was attempting to parse an implementation body")
            }
        }

        let info = NodeInfo::from_indices(start, end);

        t.success((fields, functions, info))
    }

    /*pub fn parse_implementation_item(&mut self, t: &TokenProvider) -> ParseResult<ImplementationItem> {
        let
    }*/

    /*pub fn parse_struct_definition(
        &mut self,
        t: &TokenProvider,
    ) -> ParseResult<cst::StructDefinition> {
        let mut t = t.child();

        let start = t.take(Token::Struct).join()?.start;
        let name = t.take(Token::Identifier).join()?.slice;

        t.take(Token::LBrace).join()?;

        let mut fields = Vec::new();

        while let Some(field_name) = t.try_take(Token::Identifier) {
            t.take(Token::Colon).join()?;

            let field_type = self
                .parse_type_specifier(&t)
                .join_hard(&mut t)
                .catch(&mut t)?;
            /*let expr = if self.eat_match(Token::Equals).is_some() {
                Some(self.parse_expr()?)
            } else {
                None
            };*/

            let field_member = cst::Field {
                //attributes: ma,
                name: field_name.slice,
                has_type: field_type,
            };

            fields.push(field_member);

            if let Some(_comma) = t.try_take(Token::Comma) {
                continue;
            } else {
                break;
            }
        }

        let end = t.take(Token::RBrace).join()?.end;

        let node_info = NodeInfo::from_indices(start, end);

        t.success(cst::StructDefinition {
            name,
            fields,
            info: node_info,
        })
    }*/

    pub fn parse_implementation_block(
        &mut self,
        t: &TokenProvider,
    ) -> ParseResult<cst::ImplementationDefinition> {
        let mut t = parse_header!(t);

        t.take(Token::Implementation).join()?;

        self.parse_implementation_block_expression(&t)
    }

    pub fn parse_implementation_block_expression(
        &mut self,
        t: &TokenProvider,
    ) -> ParseResult<cst::ImplementationDefinition> {
        let mut t = parse_header!(t);

        //t.take(Token::Implementation).join()?;

        let generics = self
            .parse_generic_param_list(&t)
            .join_hard(&mut t)
            .catch(&mut t)?;

        let t1 = self
            .parse_type_specifier(&t)
            .join_hard(&mut t)
            .catch(&mut t)?;

        let start = t1.node_info().as_parsed().unwrap().span.start;

        let t2 = if let Some(_v) = t.try_take(Token::For) {
            Some(
                self.parse_type_specifier(&t)
                    .join_hard(&mut t)
                    .catch(&mut t)?,
            )
        } else {
            None
        };

        let (impl_of, impl_for) = match (t1, t2) {
            (t1, Some(t2)) => (Some(t1), t2),
            (t1, None) => (None, t1),
        };

        let (fields, functions, info) = self
            .parse_implementation_body(&t)
            .join_hard(&mut t)
            .catch(&mut t)
            .hint("Any implementation requires a body")?;

        let end = info.as_parsed().unwrap().span.end;

        t.success(cst::ImplementationDefinition {
            info: NodeInfo::from_indices(start, end),
            for_type: Some(impl_for),
            of_trait: impl_of,
            fields,
            functions,
            generics,
        })
    }

    pub fn parse_specification(&mut self, t: &TokenProvider) -> ParseResult<cst::TraitDefinition> {
        let mut t = parse_header!(t);

        let start = t.take(Token::Specification).join()?.start;

        let generics = self
            .parse_generic_param_list(&t)
            .join_hard(&mut t)
            .catch(&mut t)?;

        let name = t.take(Token::Identifier).join()?.slice;

        let (fields, functions, info) = self
            .parse_implementation_body(&t)
            .join_hard(&mut t)
            .catch(&mut t)
            .hint("Any trait requires an implementation/declaration body")?;

        let constraint = if let Some(_) = t.try_take(Token::Colon) {
            Some(
                self.parse_type_specifier(&t)
                    .join_hard(&mut t)
                    .catch(&mut t)?,
            )
        } else {
            None
        };

        let end = info.as_parsed().unwrap().span.end;

        t.success(cst::TraitDefinition {
            info: NodeInfo::from_indices(start, end),
            generics,
            name,
            functions,
            constraint,
        })
    }

    pub fn parse_type_block(
        &mut self,
        t: &TokenProvider,
    ) -> ParseResult<cst::ImplementationDefinition> {
        let mut _errors = ErrorSet::new();
        /*let type_spec = Rule("type specifier");
        let field = subrule(&[Terminal(Token::Identifier), Terminal(Token::Colon), type_spec, ])

        let t = t.child()
            .rules(&[
                  Nonterminal::Terminal(Token::Identifier),
                  Nonterminal::Terminal(Token::LBrace),
                  Nonterminal::Skip { index: 6 },
                    Nonterminal::Terminal(Token::Identifier), Nonterminal::Rule("This would strictly match a type description, but here we allow only matching the basic blocks"),
                    Nonterminal::Terminal(Token::Colon),
                    Nonterminal::Terminal(Token::Identifier),
                  Nonterminal::Repeat { index: 2 },
                  Nonterminal::Terminal(Token::RBrace),
            ]);*/

        let mut t = t.child().predict(&[
            (Token::LBrace, 1.0),
            (Token::Opaque, 1.0),
            (Token::Comma, 1.0),
            (Token::RBrace, 1.0),
        ]);

        let _type_name = t
            .take(Token::Identifier)
            .join()
            .handle_here()?
            .update_solution(&t)
            .try_get();

        t.take(Token::LBrace).join()?;

        loop {
            let _r = || {
                let mut _t = t.child().predict(&[]);

                todo!()

                //
            };

            match t.take_in(&[Token::Comma, Token::RBrace]).join()?.token {
                Token::Comma => {
                    // we could have another field, so try looking again
                    t.predict_next((Token::Comma, 1.0)); // we are potentially still looking for a comma in our lookahead,
                                                         // so leave it in the rule
                    continue;
                }
                Token::RBrace => {
                    // end of the block, so return
                    break;
                }
                other => {
                    println!("Offending input token: {:?}", other);
                    panic!("Got a token that should not be there after synchronization")
                }
            }
        }

        todo!()

        /*t.take(Token::LBrace)?; // if this unit isn't supposed to be "taking" tokens (even if they are virtual) then bubble error

        let sync = loop {
            let r = || {
                //
                let t = t.child().linear(&[Token::Identifier, Token::Colon, Token::Opaque]);

                match t.try_take(Token::Identifier) {
                    Some(field_name) => {
                        t.take(Token::Colon)?
                    },
                }
            };
        }

                loop {
        let sync = || {
            let t = t.child().linear(&[Token::Identifier, Token::Colon, Token::Opaque]);

            match t.try_take(Token::Identifier) {
                Some(field_name) => {
                    t.take(Token::Colon)?
                },
            }

            break t.syncer();

            let field_name = t.take(Token::Identifier)?;
        }
        }();

        while let Some(_) = t.peek_for(Token::Identifier) {
            let t =
                t.child()
                    .linear(&[Token::Identifier, Token::Colon, Token::Opaque, Token::Comma]);

            let field_name = t.take(Token::Identifier)?;

            t.take(Token::Colon)?;

            let type_spec = self.parse_type_specifier().catch(&t)?;

            if let None = t.peek_for(Token::Comma) {
                break;
            }
        }*/

        //t.take(Token::RBrace)?;
    }

    /*
    /**
     * Used for:
     *
     * <concrete || interface> type<<generic params>> T {
     *   <fields and method decs/defs>
     * }
     */
    pub fn parse_type_declaration(&mut self) -> Result<cst::TypeDefinition, ParseResultError> {
        todo!()
    }

    /**
     * Used for:
     *
     * <fully || partially> provide <some type> for <some other type> {
     *   <fields and method defs/overrides>
     * }
     */
    pub fn parse_type_implementation(&mut self) -> Result<cst::TypeDefinition, ParseResultError> {
        todo!()
    }*/

    pub fn parse_static_declaration(
        &mut self,
        t: &TokenProvider,
    ) -> ParseResult<cst::StaticVariableDeclaration> {
        let mut t = t.child();

        let expr = self.parse_expr(&t).join_hard(&mut t).catch(&mut t)?;

        t.take(Token::Semicolon).join()?;

        t.success(cst::StaticVariableDeclaration {
            node_info: expr.as_node().node_info(),
            public: false,
            expression: expr,
        })
    }

    /// Scope spec:
    /// | <empty string>
    /// | <SCOPE>IDENTIFIER::
    /*pub fn parse_scope(&mut self, t: &TokenProvider) -> ParseResult<Vec<IStr>> {
        let mut t = t.child();

        let mut svec = Vec::new();
        while let Some(s) = t.try_take_string([Token::Identifier, Token::DoubleColon]) {
            //let s = s.as_result().expect("TODO").join(&mut t);
            //let s = todo!();
            let id = s[0]; // always exists, as we passed in a chain of length 2
            svec.push(id.slice);
        }

        t.success(svec)
    }*/

    pub fn parse_type_list(
        &mut self,
        t: &TokenProvider,
        terminator: Token,
    ) -> ParseResult<Vec<cst::SyntacticTypeReference>> {
        let mut t = t.child();

        //t.take(Token::CmpLessThan).join()?;

        //GuardedResult::catch(GuardedResult::join_hard(t.take(Token::CmpLessThan), &mut t), &mut t);

        let mut tvec = Vec::new();
        while let None = t.try_take(terminator) {
            let tspec = self
                .parse_type_specifier(&t)
                .join_hard(&mut t)
                .catch(&mut t)?;
            tvec.push(tspec);
            match t.try_take(Token::Comma) {
                Some(_) => continue, // comma means there might be another type
                None => break,
            }
        }

        t.success(tvec)
    }

    /*pub fn parse_type_reference(&mut self, t: &TokenProvider) -> ParseResult<cst::TypeReference> {
        let mut t = t.child();

        let _scope = self.parse_scope(&t).join_hard(&mut t).catch(&mut t)?; // fine to do unconditionally since null deriving

        let _typename = t
            .take(Token::Identifier)
            .hint("All types start with a scope, and must be followed by a typename")
            .join()?;

        todo!()
    }*/

    /// Type reference specification:
    /// | &TYPE
    /// | (TYPELIST)
    /// | <SCOPE>IDENTIFIER
    /// | <SCOPE>IDENTIFIER<TYPELIST>
    ///
    /// Typelist:
    /// | TYPE
    /// | TYPELIST, TYPE
    pub fn parse_type_specifier(
        &mut self,
        t: &TokenProvider,
    ) -> ParseResult<cst::SyntacticTypeReference> {
        //let mut t = t.child();
        let mut t = parse_header!(t);

        // these can start with a ( for a typle, a & for a reference, or a str, [<] for a named and
        // optionally parameterized type
        let start = t.lh.la(0).unwrap().start;

        match t.try_take_in(&[Token::Ampersand, Token::Asterisk, Token::LParen]) {
            None => {
                //let scope = self.parse_scope(&t).join_hard(&mut t).catch(&mut t)?;
                let mut scope = self.parse_scope(&t).join_hard(&mut t).catch(&mut t)?;

                let typename = t
                    .take(Token::Identifier)
                    .hint("Any scoped type requires a typename to follow the scope")
                    .join()?;

                scope.scope.push(typename.slice);

                let s: ScopedName = ScopedName { scope: scope.scope };

                let end = typename.end;

                let tr = cst::SyntacticTypeReference {
                    info: NodeInfo::from_indices(start, end),
                    inner: cst::SyntacticTypeReferenceInner::Single { name: s },
                };

                t.success(tr)
            }
            Some(tw) if tw.token.matches(Token::LParen) => {
                let list = self
                    .parse_type_list(&t, Token::RParen)
                    .join_hard(&mut t)
                    .catch(&mut t)?;

                let tr = cst::SyntacticTypeReferenceInner::Tuple(list);

                /*let tr = cst::SyntacticTypeReference::generic_new(
                    cst::ScopedNameReference {
                        node_info: NodeInfo::Builtin,
                        scope: Vec::new(),
                        silent: true,
                    },
                    "".intern(),
                    NodeInfo::from_indices(tw.start, t.lh.la(-1).unwrap().end),
                    list,
                );*/

                let tr = cst::SyntacticTypeReference {
                    inner: tr,
                    info: NodeInfo::from_indices(tw.start, t.lh.la(-1).unwrap().end),
                };

                t.success(tr)
                //todo!("Need to implement tuple types/type references")
            }
            Some(tw) if tw.token.matches(Token::Ampersand) => {
                panic!()
            }
            Some(tw) if tw.token.matches(Token::Asterisk) => {
                panic!()
            }
            _ => unreachable!(),
        }
    }
}
