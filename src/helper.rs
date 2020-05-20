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
            println!("Advance finds token: {:?} with contents {}", tok, self.lexer.slice());
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

    pub struct LookaheadStream<'a> {
        tokens: Vec<TokenWrapper<'a>>,
        index: usize,
    }

    impl<'a> LookaheadStream<'a> {
        pub fn new(w: &mut Wrapper<'a>) -> LookaheadStream<'a> {
            let mut v = Vec::new();
            while let Ok(tw) = w.next() {
                v.push(tw);
            }

            LookaheadStream {
                tokens: v,
                index: 0,
            }
        }

        pub fn next(&mut self) -> ParseResult<'a> {
            //self.tokens[self.index]
            let r = self.la(0);

            //self.index += 1;
            self.advance();

            r
            //self.tokens.get(self.index).map_or(Err(ParseResultError::EndOfFile), |&t| Ok(t))
        }

        pub fn prev(&mut self) -> ParseResult<'a> {
            let r = self.la(0);

            //self.index -= 1;
            self.backtrack();

            r
        }

        pub fn backtrack(&mut self) {
            self.index -= 1;
        }

        pub fn advance(&mut self) {
            self.index += 1;
        }

        pub fn la(&self, offset: isize) -> ParseResult<'a> {
            let index = self.index as isize + offset;
            println!("Lookahead asked for index {}", index);
            if index < 0 {
                Err(ParseResultError::NotYetParsed)
            } else {
                let r = self.tokens
                    .get(index as usize)
                    .map_or(
                        Err(ParseResultError::EndOfFile),
                        |&t| Ok(t));

                println!("la gives result: {:?}", r);

                r
            }
        }
    }
}
