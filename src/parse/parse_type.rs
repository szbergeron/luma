use crate::ast::base::IntoAstNode;
use crate::ast::{self, MemberAttributes};
use crate::lex::{Token, ErrorSet};
use crate::parse::*;

use super::parse_tools::{ParseValueGuard};
use super::schema::{Nonterminal, TokenProvider, CorrectionBubblingResult, ParseResult, ResultHint};
use Nonterminal::*;

impl<'lexer> Parser<'lexer> {
    /// Currently unconditionally returns memberattrs as they are null-deriving
    pub fn parse_member_attributes(&mut self) -> Result<ast::MemberAttributes, ParseResultError> {
        let mut mutable: Option<bool> = None;
        let mut public: Option<bool> = None;

        Ok(MemberAttributes {
            public: public.unwrap_or(true),
            mutable: mutable.unwrap_or(true),
        }) // TODO
    }

    pub fn parse_struct_definition(&mut self, t: &TokenProvider) -> ParseResult<ast::TypeDefinition> {
        let t = t.child();

        let start = t.take(Token::Struct).hard(&mut t)?.join(&mut t).start;
        let name = t.take(Token::Identifier).hard(&mut t)?.join(&mut t).slice;

        t.take(Token::LBrace)?;

        let mut fields = Vec::new();

        while let (Ok(ma), Some(field_name)) = (
            self.parse_member_attributes(),
            t.try_take(Token::Identifier),
        ) {
            t.take(Token::Colon).hard(&mut t)?.join(&mut t);
            let field_type = self.parse_type_specifier(&t).hard(&mut t)?.join(&mut t);
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

        let end = t.take(Token::RBrace).hard(&mut t)?.join(&mut t).end;

        let node_info = ast::NodeInfo::from_indices(start, end);

        t.success(ast::TypeDefinition::Struct(ast::RecordValueDefinition {
            fields,
            node_info,
        }))
    }

    pub fn parse_type_block(
        &mut self,
        t: &TokenProvider,
    ) -> ParseResult<ast::ImplementationBody> {
        let mut errors = ErrorSet::new();
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

        let t = t.child().predict(&[Token::LBrace, Token::Opaque, Token::Comma, Token::RBrace]);

        let type_name = t.take(Token::Identifier)?.optionize(&mut errors);

        t.take(Token::LBrace)?;

        loop {
            let r = || {
                let t = t.child().predict(&[Token::Identifier, Token::Colon, Token::Opaque, Token::Comma]);

                //
            };

            match t.take_in(&[Token::Comma, Token::RBrace]).hard(&mut t)?.join(&mut t).token {
                Token::Comma => {
                    // we could have another field, so try looking again
                    t.predict_next(Token::Comma); // we are potentially still looking for a comma in our lookahead,
                    // so leave it in the rule
                    continue;
                },
                Token::RBrace => {
                    // end of the block, so return
                    break;
                },
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
        &mut self, t: &TokenProvider
    ) -> ParseResult<ast::StaticVariableDeclaration> {
        let t = t.child();

        let expr = self.parse_expr(&t).hard(&mut t)?.join(&mut t);

        t.take(Token::Semicolon).hard(&mut t)?.join(&mut t);

        t.success(ast::StaticVariableDeclaration {
            node_info: expr.as_node().node_info(),
            public: false,
            expression: expr,
        })
    }

    /// Scope spec:
    /// | <empty string>
    /// | <SCOPE>IDENTIFIER::
    pub fn parse_scope(
        &mut self,
        t: &TokenProvider,
    ) -> ParseResult<Vec<IStr>> {
        let t = t.child();

        let mut svec = Vec::new();
        while let Ok(s) = t
            .try_take_string([Token::Identifier, Token::DoubleColon])
            .hint(
                "A scope, if not null deriving, has an IDENTIFIER followed by a double colon (::)",
            )
        {
            //let s = todo!();
            let id = s[0]; // always exists, as we passed in a chain of length 2
            svec.push(id.slice);
        }

        t.success(svec)
    }

    pub fn parse_type_list(&mut self, t: &TokenProvider) -> ParseResult<Vec<ast::TypeReference>> {
        let t = t.child();

        t.take(Token::CmpLessThan).hard(&mut t)?.join(&mut t);

        let mut tvec = Vec::new();
        while let None = t.try_take(Token::CmpLessThan) {
            let tspec = self.parse_type_specifier(&t).hard(&mut t)?.join(&mut t);
            tvec.push(tspec);
            match t.try_take(Token::Comma) {
                Some(_) => continue, // comma means there might be another type
                None => break,
            }
        }

        t.success(tvec)
    }

    pub fn parse_type_reference(
        &mut self,
        t: &TokenProvider,
    ) -> ParseResult<ast::TypeReference> {
        let t = t.child();

        let scope = self.parse_scope(&t).hard(&mut t)?.join(&mut t); // fine to do unconditionally since null deriving

        let typename = t
            .take(Token::Identifier)
            .hint("All types start with a scope, and must be followed by ")
            .hard(&mut t)?;

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
        let t = t.child();

        // these can start with a ( for a typle, a & for a reference, or a str, [<] for a named and
        // optionally parameterized type

        let scope = self.parse_scope(&t).hard(&mut t)?.join(&mut t);

        /*if !scope.silent {
            self.hard_expect
        }*/
        // TODO: eval if should require a trailing :: during scope lookup

        let final_id = scope
            .last()
            .expect("ScopedNameReference had no final id, not even implicit");
        let final_id = *final_id;

        let mut tr = ast::TypeReference::new(todo!("scope"), final_id);

        let continuation = [Token::LParen, Token::Ampersand];
        //self.eat_match_in(&continuation)?;
        //println!("Trying to eat a type_spec with la {:?}", self.lex.la(0));
        match t.try_take_in(&continuation) {
            Some(tw) => {
                match tw.token {
                    Token::Ampersand => {
                        //println!("adding a & to a ctx that had {:?}", tr.ctx.scope);
                        tr.ctx.scope.push(intern("&"));

                        let inner = self.parse_type_specifier(&t).hard(&mut t)?.join(&mut t);
                        tr.type_args.push(inner);
                    }
                    Token::LParen => {
                        tr.ctx.scope.push(intern("Tuple"));

                        //println!("la is {:?}", self.lex.la(0));

                        #[allow(irrefutable_let_patterns)]
                        while let tri = self.parse_type_specifier(&t).hard(&mut t)?.join(&mut t) {
                            tr.type_args.push(tri);
                            match t.try_take(Token::Comma) {
                                None => break,
                                Some(_comma) => continue,
                            }
                        }

                        t.take(Token::RParen).hard(&mut t)?.join(&mut t);
                    }
                    _ => {
                        return t.failure(Some(ParseResultError::UnexpectedToken(
                            tw,
                            continuation.to_vec(),
                            Some(
                                "Was trying to parse a type specifier, but didn't find a valid
                            start to such an expression",
                            ),
                        )));
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

        t.success(tr)
    }
}
