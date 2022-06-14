use crate::parse::*;

use super::schema::TokenProvider;

impl<'lexer> Parser<'lexer> {
    pub fn parse_spec(&mut self, t: &TokenProvider) -> ParseResult<Spec> {
        let mut t = parse_header!(t, [Token::Use => 1]);
        //
        todo!()
    }

    pub fn parse_spec_declaration(&mut self, t: &TokenProvider) -> ParseResult<SpecEntry> {
        let mut t = parse_header!(t,
            [Token::Use => 1,
            Token::Identifier => 1,
            Token::Dot => 1,
            Token::Identifier => 1,
            Token::As => 1,
            Token::Identifier => 1]);

        t.take(Token::Use).join()?;

        t.take(Token::Identifier).join()?;

        let is_source = 

        let file_root = t.take(Token::Identifier).join()?;

        t.take(Token::Dot).join()?;

        let name = t.take(Token::Identifier).join()?;

        match name.slice.resolve() {
            "luma" => todo!(),
            "spec" => todo!(),
            _ => {
                // TODO: is a file type other than the
                // usual suspects an error? What should we do with it?
                // maybe just include it as a data file in output binary
                // data segment?
            }
        }

        todo!()
    }
}
