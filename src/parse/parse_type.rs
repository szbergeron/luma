use crate::ast::base::IntoAstNode;
use crate::ast::{self, MemberAttributes};
use crate::lex::Token;
use crate::parse::*;

use super::parse_tools::{LexerStreamHandle, ParseResult, ParseValueGuard};
use super::schema::Nonterminal;
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

    pub fn parse_struct_definition(&mut self) -> Result<ast::TypeDefinition, ParseResultError> {
        let start = self.hard_expect(Token::Struct)?.start;
        let name = self.hard_expect(Token::Identifier)?.slice;

        self.hard_expect(Token::LBrace)?;

        let mut fields = Vec::new();

        while let (Ok(ma), Some(field_name)) = (
            self.parse_member_attributes(),
            self.eat_match(Token::Identifier),
        ) {
            self.hard_expect(Token::Colon)?;
            let field_type = self.parse_type_specifier()?;
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

            if let Some(_comma) = self.eat_match(Token::Comma) {
                continue;
            } else {
                break;
            }
        }

        let end = self.hard_expect(Token::RBrace)?.end;

        let node_info = ast::NodeInfo::from_indices(start, end);

        Ok(ast::TypeDefinition::Struct(ast::RecordValueDefinition {
            fields,
            node_info,
        }))
    }

    pub fn parse_type_block(&mut self, t: &TokenProvider) -> Result<ast::ImplementationBody, ParseResultError> {
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

        let type_name = t.take(Token::Identifier)?;

        t.take(Token::LBrace)?; // if this unit isn't supposed to be "taking" tokens (even if they are virtual) then bubble error

        while let Some(_) = t.peek_for(Token::Identifier) {
            let field_name = t.take(Token::Identifier)?;

            t.take(Token::Colon)?;

            let type_spec = self.parse_type_specifier().catch(&t)?;

            if let None = t.peek_for(Token::Comma) {
                break;
            }
        }

        t.take(Token::RBrace)?;
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
    ) -> Result<ast::StaticVariableDeclaration, ParseResultError> {
        let expr = self.parse_expr()?;
        self.hard_expect(Token::Semicolon)?;

        Ok(ast::StaticVariableDeclaration {
            node_info: expr.as_node().node_info(),
            public: false,
            expression: expr,
        })
    }

    /// Scope spec:
    /// | <empty string>
    /// | <SCOPE>IDENTIFIER::
    pub fn parse_scope(&mut self, next: LexerStreamHandle) -> ParseResult<Vec<IStr>, ParseResultError> {
        let mut svec = Vec::new();
        while let Ok(s) = self.eat_match_string([Token::Identifier, Token::DoubleColon]).hint("A scope, if not null deriving, has an IDENTIFIER followed by a double colon (::)") {
            let id = s[0]; // always exists, as we passed in a chain of length 2
            svec.push(id.slice);
        }

        ParseValueGuard::success(svec, next)
    }

    pub fn parse_type_list(&mut self) -> Result<Vec<ast::TypeReference>, ParseResultError> {
        let mut tvec = Vec::new();
        while let Ok(t) = self.parse_type_specifier() {
            tvec.push(t);
            match self.eat_match(Token::Comma) {
                Some(_) => continue, // comma means there might be another type
                None => break,
            }
        }

        Ok(tvec)
    }

    pub fn parse_type_reference(&mut self, next: LexerStreamHandle) -> Result<ast::TypeReference, ParseResultError> {
        let (scope, next) = self.parse_scope(next)?.open(); // fine to do unconditionally since null deriving

        let typename = self.hard_expect(Token::Identifier).hint("All types start with a scope, and must be followed by ")?;
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
    pub fn parse_type_specifier(
        &mut self,
    ) -> Result<ast::TypeReference, ParseResultError> {
        // these can start with a ( for a typle, a & for a reference, or a str, [<] for a named and
        // optionally parameterized type

        let scope = self.parse_scope();

        /*if !scope.silent {
            self.hard_expect
        }*/
        // TODO: eval if should require a trailing :: during scope lookup

        let final_id = scope
            .scope
            .last()
            .expect("ScopedNameReference had no final id, not even implicit");
        let final_id = *final_id;
        let mut tr = ast::TypeReference::new(scope, final_id);

        let continuation = [Token::LParen, Token::Ampersand];
        //self.eat_match_in(&continuation)?;
        //println!("Trying to eat a type_spec with la {:?}", self.lex.la(0));
        match self.eat_match_in(continuation) {
            Some(tw) => {
                match tw.token {
                    Token::Ampersand => {
                        //println!("adding a & to a ctx that had {:?}", tr.ctx.scope);
                        tr.ctx.scope.push(intern("&"));

                        let inner = self.parse_type_specifier()?;
                        tr.type_args.push(inner);
                    }
                    Token::LParen => {
                        tr.ctx.scope.push(intern("Tuple"));

                        //println!("la is {:?}", self.lex.la(0));

                        #[allow(irrefutable_let_patterns)]
                        while let tri = self.parse_type_specifier()? {
                            tr.type_args.push(tri);
                            match self.eat_match(Token::Comma) {
                                None => break,
                                Some(_comma) => continue,
                            }
                        }

                        self.hard_expect(Token::RParen)?;
                    }
                    _ => {
                        self.err(ParseResultError::UnexpectedToken(
                            tw,
                            continuation.to_vec(),
                            Some(
                                "Was trying to parse a type specifier, but didn't find a valid
                            start to such an expression",
                            ),
                        ))?;
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

        Ok(Box::new(tr))
    }

}
