use crate::ast::base::IntoAstNode;
use crate::ast::{
    self, FieldMember, Implementation, ImplementationBody, ImplementationItem, MemberAttributes,
};
use crate::lex::{CodeLocation, ErrorSet, ParseResultError, Token};
use crate::parse::*;

use super::schema::{ResultHint, TokenProvider};
use ast::base::*;
use ast::expressions::*;
use ast::outer::*;

impl<'lexer> Parser<'lexer> {
    /// Currently unconditionally returns memberattrs as they are null-deriving
    pub fn parse_member_attributes(&mut self, t: &mut TokenProvider) -> ast::MemberAttributes {
        let public = t.try_take(Token::Public).is_some();
        let mutable = t.try_take(Token::Mutable).is_some();

        MemberAttributes { public, mutable }
    }

    pub fn parse_implementation_expression(
        &mut self,
        t: &TokenProvider,
        lhs: Box<ExpressionWrapper>,
    ) -> ParseResult<ExpressionWrapper> {
        let mut t = parse_header!(t);

        let start = lhs.as_node().node_info().as_parsed().unwrap().span.start;

        // parse trait list

        let mut trait_list = Vec::new();

        loop {
            let ts = self
                .parse_type_specifier(&t)
                .join_hard(&mut t)
                .catch(&mut t)?;
            trait_list.push(ts);
            match t.try_take(Token::Plus) {
                None => break,
                Some(_) => continue,
            }
        }

        let body = self
            .parse_implementation_body(&t)
            .join_hard(&mut t)
            .catch(&mut t)?;

        let end = body.node_info.as_parsed().unwrap().span.end;

        let node_info = NodeInfo::from_indices(start, end);

        t.success(ExpressionWrapper::ImplementationModification(
            ImplementationModificationExpression {
                modifying: lhs,
                traits: trait_list,
                node_info,
                impl_block: box body,
            },
        ))
    }

    pub fn parse_implementation_body(
        &mut self,
        t: &TokenProvider,
    ) -> ParseResult<ImplementationBody> {
        let mut t =
            parse_header!(t, [Token::RBrace => 1.0, Token::Function => 10.0, Token::Var => 10.0]);

        let start = t.take(Token::LBrace).join()?.start;
        let end;

        let mut items = Vec::new();

        loop {
            let expected = [Token::RBrace, Token::Function, Token::Var];
            let next = t.take_in(&expected).join()?;
            match next.token {
                Token::Function => {
                    t.lh.backtrack();
                    let f = self
                        .parse_function_declaration(&t)
                        .join_hard(&mut t)
                        .catch(&mut t)
                        .handle_here()?;

                    let (v, e, s) = f.update_solution(&t).open();

                    match v {
                        Some(v) => {
                            items.push(ImplementationItem::Function(v));
                        }
                        None => (),
                    }

                    //t.sync_with_solution(s);
                }
                Token::Var => {
                    let ident = t.take(Token::Identifier).join()?;
                    let _ = t.take(Token::Colon).join()?;
                    let tr = self
                        .parse_type_specifier(&t)
                        .join_hard(&mut t)
                        .catch(&mut t)?;

                    items.push(ImplementationItem::Field(FieldMember {
                        name: ident.slice,
                        ftype: tr,
                    }));
                }
                Token::RBrace => {
                    end = next.end;
                    break;
                }
                _ => unreachable!(), //_ => return t.unexpected_token(&expected, "Was attempting to parse an implementation body")
            }
        }

        let info = NodeInfo::from_indices(start, end);

        t.success(ImplementationBody {
            node_info: info,
            items,
        })
    }

    /*pub fn parse_implementation_item(&mut self, t: &TokenProvider) -> ParseResult<ImplementationItem> {
        let
    }*/

    pub fn parse_struct_definition(
        &mut self,
        t: &TokenProvider,
    ) -> ParseResult<ast::StructDefinition> {
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

            let field_member = ast::FieldMember {
                //attributes: ma,
                name: field_name.slice,
                ftype: field_type,
            };

            fields.push(field_member);

            if let Some(_comma) = t.try_take(Token::Comma) {
                continue;
            } else {
                break;
            }
        }

        let end = t.take(Token::RBrace).join()?.end;

        let node_info = ast::NodeInfo::from_indices(start, end);

        t.success(ast::StructDefinition {
            name,
            fields,
            node_info,
        })
    }

    pub fn parse_implementation_block(
        &mut self,
        t: &TokenProvider,
    ) -> ParseResult<ast::Implementation> {
        let mut t = parse_header!(t);

        t.take(Token::Implementation).join()?;

        let t1 = self
            .parse_type_specifier(&t)
            .join_hard(&mut t)
            .catch(&mut t)?;

        let start = t1.node_info().as_parsed().unwrap().span.start;

        let t2 = if let Some(v) = t.try_take(Token::For) {
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

        let body = self
            .parse_implementation_body(&t)
            .join_hard(&mut t)
            .catch(&mut t)
            .hint("Any implementation requires a body")?;

        let end = body.node_info.as_parsed().unwrap().span.end;

        t.success(ast::Implementation {
            node_info: NodeInfo::from_indices(start, end),
            impl_for, impl_of, body
        })
    }

    pub fn parse_type_block(&mut self, t: &TokenProvider) -> ParseResult<ast::ImplementationBody> {
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

    /**
     * Used for:
     *
     * <concrete || interface> type<<generic params>> T {
     *   <fields and method decs/defs>
     * }
     */
    pub fn parse_type_declaration(&mut self) -> Result<ast::TypeDefinition, ParseResultError> {
        todo!()
    }

    /**
     * Used for:
     *
     * <fully || partially> provide <some type> for <some other type> {
     *   <fields and method defs/overrides>
     * }
     */
    pub fn parse_type_implementation(&mut self) -> Result<ast::TypeDefinition, ParseResultError> {
        todo!()
    }

    pub fn parse_static_declaration(
        &mut self,
        t: &TokenProvider,
    ) -> ParseResult<ast::StaticVariableDeclaration> {
        let mut t = t.child();

        let expr = self.parse_expr(&t).join_hard(&mut t).catch(&mut t)?;

        t.take(Token::Semicolon).join()?;

        t.success(ast::StaticVariableDeclaration {
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

    pub fn parse_type_list(&mut self, t: &TokenProvider) -> ParseResult<Vec<ast::TypeReference>> {
        let mut t = t.child();

        t.take(Token::CmpLessThan).join()?;

        //GuardedResult::catch(GuardedResult::join_hard(t.take(Token::CmpLessThan), &mut t), &mut t);

        let mut tvec = Vec::new();
        while let None = t.try_take(Token::CmpLessThan) {
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

    pub fn parse_type_reference(&mut self, t: &TokenProvider) -> ParseResult<ast::TypeReference> {
        let mut t = t.child();

        let _scope = self.parse_scope(&t).join_hard(&mut t).catch(&mut t)?; // fine to do unconditionally since null deriving

        let _typename = t
            .take(Token::Identifier)
            .hint("All types start with a scope, and must be followed by a typename")
            .join()?;

        todo!()
    }

    /// Type reference specification:
    /// | &TYPE
    /// | (TYPELIST)
    /// | <SCOPE>IDENTIFIER
    /// | <SCOPE>IDENTIFIER<TYPELIST>
    ///
    /// Typelist:
    /// | TYPE
    /// | TYPELIST, TYPE
    pub fn parse_type_specifier(&mut self, t: &TokenProvider) -> ParseResult<ast::TypeReference> {
        //let mut t = t.child();
        let mut t = parse_header!(t);

        // these can start with a ( for a typle, a & for a reference, or a str, [<] for a named and
        // optionally parameterized type
        let start = t.lh.la(0).unwrap().start;

        match t.try_take_in(&[Token::Ampersand, Token::Asterisk, Token::RParen]) {
            None => {
                //let scope = self.parse_scope(&t).join_hard(&mut t).catch(&mut t)?;
                let scope = self.parse_scope(&t).join_hard(&mut t).catch(&mut t)?;

                let typename = t
                    .take(Token::Identifier)
                    .hint("Any scoped type requires a typename to follow the scope")
                    .join()?;

                let end = typename.end;

                let tr = ast::TypeReference::new(scope, typename.slice, NodeInfo::from_indices(start, end));

                t.success(tr)
            }
            Some(t) if t.token.matches(Token::RParen) => {
                todo!("Need to implement tuple types/type references")
            }
            Some(t) if t.token.matches(Token::Ampersand) => {
                panic!()
            }
            Some(t) if t.token.matches(Token::Asterisk) => {
                panic!()
            }
            _ => unreachable!(),
        }
    }
}
