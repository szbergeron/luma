use crate::ast::base::IntoAstNode;
use crate::parse::*;
use crate::lex::Token;
use crate::ast;

impl<'lexer> Parser<'lexer> {

    /// Parses the elements of a type parameter (not argument!) list,
    /// such as with `struct a<T, U, V> {}` where those parameters
    /// are to be used to substitute within the struct
    ///
    /// This will also include bounds and `where` clauses later on
    pub fn parse_type_param_list(&mut self) -> Result<Vec<IStr>, ParseResultError> {
        let mut typeparams = Vec::new();
        if let Some(_lt) = self.eat_match(Token::CmpLessThan) {
            //let first = self.parse_type_specifier();

            while let Some(id) = self.eat_match(Token::Identifier) {
                typeparams.push(id.slice);
                if let Some(_comma) = self.eat_match(Token::Comma) {
                    continue;
                } else {
                    break;
                }
            }
            self.hard_expect(Token::CmpGreaterThan)?;
        }

        Ok(typeparams)

    }

    //const first_struct: [Token; 1] = [Token::Struct];
    pub fn parse_struct_declaration(&mut self) -> Result<ast::TypeDefinition, ParseResultError> {
        let start = self.hard_expect(Token::Struct)?.start;
        let _id = self.hard_expect(Token::Identifier)?.slice;

        let _typeparams = self.parse_type_param_list()?;

        self.hard_expect(Token::LBrace)?;

        let mut fields = Vec::new();

        while let Some(field) = self.eat_match(Token::Identifier) {
            self.hard_expect(Token::Colon)?;
            let field_type = *self.parse_type_specifier()?;
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

        let end = self.hard_expect(Token::RBrace)?.end;

        let _node_info = ast::NodeInfo::from_indices(start, end);

        todo!()

        /*Ok(ast::VirtualDefinition {
            node_info,
            fields,
            name: id,
            public: false,
        })*/
    }

    //type TypeBlock = (Vec<FieldDeclaration>, Vec<MethodDeclaration>, Vec<MethodDefinition>);

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
