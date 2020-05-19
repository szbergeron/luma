//use crate::lex;

pub mod lex_wrap {
    use logos::Logos;

    type ParseResult<'a> = Result<TokenWrapper<'a>, ParseResultError<'a>>;

    pub struct Wrapper<'a> {
        lexer: logos::Lexer<'a, crate::lex::Token>,
        cur: Result<TokenWrapper<'a>, ParseResultError<'a>>, 
    }

    #[derive(Debug, Clone, Copy)]
    pub struct TokenWrapper<'a> {
        pub token: crate::lex::Token,
        pub slice: &'a str,

    }

    #[derive(Debug, Clone, Copy)]
    pub enum ParseResultError<'a> {
        EndOfFile,
        NotYetParsed,
        UnexpectedToken(TokenWrapper<'a>),
        //
    }

    impl<'a> Wrapper<'a> {
        pub fn new(input: &'a str) -> Wrapper<'a> {
            let lex = crate::lex::Token::lexer(input);

            Wrapper { lexer: lex, cur: Err(ParseResultError::NotYetParsed) }
        }

        pub fn peek(&mut self) -> ParseResult<'a> {
            self.cur.clone()
        }

        pub fn advance(&mut self) -> () {
            let tok = self.lexer.next();
            println!("Advance finds token: {:?}", tok);
            match tok {
                Some(tok) => self.cur = Ok(TokenWrapper { token: tok, slice: self.lexer.slice() }),
                None => self.cur = Err(ParseResultError::EndOfFile),
            }
        }

        pub fn next(&mut self) -> ParseResult<'a> {
            self.advance();
            self.peek()
            /*if let Ok(wrapper) = self.peek() {
                Ok(TokenWrapper { token: other, slice: self.lexer.slice

            self.peek()*/
        }

        /*pub fn next_semantic_token(&mut self) -> Result<TokenWrapper<'a>, ParseResultError> {
            /*self.advance();
            while let Ok(wrapper) = self.peek() {
                println!("nst finds a: {:?}", wrapper);
                match wrapper.token {
                    crate::lex::Token::Newline => { self.advance(); continue },
                    other => return Ok(TokenWrapper { token: other, slice: self.lexer.slice() } ),
                }
            }*/
            panic!("HELP");

            Err(ParseResultError::EndOfFile)
        }*/
    }
}
