//use crate::lex;

pub mod lex_wrap {
    use logos::Logos;
    use std::rc::Rc;

    type ParseResult<'a> = Result<TokenWrapper<'a>, ParseResultError<'a>>;

    pub struct Wrapper<'a> {
        lexer: logos::Lexer<'a, crate::lex::Token>,
        cur: Result<TokenWrapper<'a>, ParseResultError<'a>>, 
    }

    #[derive(Debug, Clone, Copy)]
    pub struct TokenWrapper<'a> {
        pub token: crate::lex::Token,
        pub slice: &'a str,
        pub start: usize,
        pub end: usize,

    }

    #[derive(Debug, Clone, Copy)]
    pub enum ParseResultError<'a> {
        EndOfFile,
        NotYetParsed,
        ExpectedExpressionNotPresent,
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
                Some(tok) => self.cur = Ok(TokenWrapper { token: tok, slice: self.lexer.slice(), start: self.lexer.span().start, end: self.lexer.span().end }),
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

    #[derive(Clone)]
    pub struct LookaheadStream<'a> {
        tokens: Rc<Vec<TokenWrapper<'a>>>,
        index: usize,

        //latest: Option<TokenWrapper<'a>>,
    }

    impl<'a> LookaheadStream<'a> {
        pub fn new(w: &mut Wrapper<'a>) -> LookaheadStream<'a> {
            let mut v = Vec::new();
            let mut comment_level = 0;
            let mut inside_line_comment = false;
            while let Ok(tw) = w.next() { // handle comments
                use crate::lex::Token;
                match tw.token {
                    Token::LineCommentStart => { inside_line_comment = true; continue; },
                    Token::Newline => { inside_line_comment = false; continue; },
                    Token::LBlockComment | Token::LDocComment => { comment_level += 1; continue; },
                    Token::RBlockComment | Token::RDocComment => {
                        if comment_level > 0 {
                            comment_level -= 1; // will cause syntax error in else during parse
                        }

                        continue;
                    },
                    _ => {},
                }
                if !inside_line_comment && comment_level == 0 {
                    v.push(tw);
                }
            }

            LookaheadStream {
                tokens: Rc::new(v),
                index: 0,
                //latest: None,
            }
        }

        /*pub fn after(&self) -> Option<usize> {
            self.latest.map(|tw| tw.end)
        }

        pub fn before(&self) -> Option<usize> {
            self.latest.map(|tw| tw.start)
        }*/

        pub fn seek_to(&mut self, index: usize) {
            self.index = index;
        }

        pub fn seek_by(&mut self, offset: isize) {
            self.index = (self.index as isize + offset) as usize;
        }

        pub fn index(&self) -> usize {
            self.index
        }

        pub fn ffwd(&mut self, other: &LookaheadStream<'a>) {
            self.seek_to(other.index());
        }

        pub fn next(&mut self) -> ParseResult<'a> {
            //self.tokens[self.index]
            let r = self.la(0);
            //self.latest = Some(r);

            //self.index += 1;
            self.advance();

            //println!("LookaheadStream advances, takes token {:?}", r);

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
            //println!("LookaheadStream advances over token {:?}", self.la(0));
            self.index += 1;
        }

        pub fn la(&self, offset: isize) -> ParseResult<'a> {
            let index = self.index as isize + offset;
            //println!("Lookahead asked for index {}", index);
            if index < 0 {
                Err(ParseResultError::NotYetParsed)
            } else {
                let r = self.tokens
                    .get(index as usize)
                    .map_or(
                        Err(ParseResultError::EndOfFile),
                        |&t| Ok(t));

                //println!("la gives result: {:?}", r);

                r
            }
        }
    }
}
