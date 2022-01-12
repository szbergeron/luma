use crate::ast::base::IntoAstNode;
use crate::ast::{self, MemberAttributes};
use crate::lex::Token;
use crate::parse::*;

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
            let field_type = *self.parse_type_specifier()?;
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
        }))
    }

    pub fn parse_type_block(&mut self) -> Result<ast::ImplementationBody, ParseResultError> {
        todo!()
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
}
