use crate::ast::base::IntoAstNode;
use crate::ast::{self, MemberAttributes};
use crate::lex::{ErrorSet, Token, ParseResultError, CodeLocation};
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

    pub fn parse_struct_definition(
        &mut self,
        t: &TokenProvider,
    ) -> ParseResult<ast::TypeDefinition> {
        let mut t = t.child();

        let start = t.take(Token::Struct).join()?.start;
        let name = t.take(Token::Identifier).join()?.slice;

        t.take(Token::LBrace).join()?;

        let mut fields = Vec::new();

        while let (ma, Some(field_name)) = (
            self.parse_member_attributes(&mut t),
            t.try_take(Token::Identifier),
        ) {
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
                attributes: ma,
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

        t.success(ast::TypeDefinition::Struct(ast::RecordValueDefinition {
            name,
            fields,
            node_info,
        }))
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

        match t.try_take_in(&[Token::Ampersand, Token::Asterisk]) {
            None => {
                //let scope = self.parse_scope(&t).join_hard(&mut t).catch(&mut t)?;
                let scope = self.parse_scope(&t).join_hard(&mut t).catch(&mut t)?;

                let typename = t
                    .take(Token::Identifier)
                    .hint("Any scoped type requires a typename to follow the scope")
                    .join()?;

                let tr = ast::TypeReference::new(scope, typename.slice);

                t.success(tr)
            }
            Some(t) if t.token.matches(Token::Ampersand) => {
                panic!()
            }
            Some(t) if t.token.matches(Token::Asterisk) => {
                panic!()
            }
            _ => unreachable!()
        }
    }
}
