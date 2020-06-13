//use crate::lex;

pub enum EitherAnd<A, B> {
    A(A),
    B(B),
    Both(A, B),
    Neither,
}

impl<A, B> EitherAnd<A, B> {
    pub fn with_a(self, a: A) -> EitherAnd<A, B> {
        match self {
            Self::A(_) | Self::Neither => Self::A(a),
            Self::B(b) => Self::Both(a, b),
            Self::Both(_, b) => Self::Both(a, b),
        }
    }

    pub fn with_b(self, b: B) -> EitherAnd<A, B> {
        match self {
            Self::A(a) => Self::Both(a, b),
            Self::B(_) | Self::Neither => Self::B(b),
            Self::Both(a, _) => Self::Both(a, b),
        }
    }

    pub fn a(&self) -> Option<&A> {
        match self {
            Self::A(a) => Some(&a),
            Self::B(_) | Self::Neither => None,
            Self::Both(a, _) => Some(&a),
        }
    }

    pub fn b(&self) -> Option<&B> {
        match self {
            Self::A(_) | Self::Neither => None,
            Self::B(b) => Some(&b),
            Self::Both(_, b) => Some(&b),
        }
    }
}

pub mod lex_wrap {
    use logos::Logos;
    use std::rc::Rc;

    type ParseResult<'a> = Result<TokenWrapper<'a>, ParseResultError<'a>>;

    pub struct Wrapper<'a> {
        lexer: logos::Lexer<'a, crate::lex::Token>,
        cur: Result<TokenWrapper<'a>, ParseResultError<'a>>,

        current_line: isize,
        last_newline_absolute: usize,
    }

    #[derive(Debug, Clone, Copy)]
    pub enum CodeLocation {
        Parsed(Loc),
        Builtin,
    }

    impl CodeLocation {
        pub fn offset_by(&self, line: isize, offset: isize) -> CodeLocation {
            match self {
                Self::Builtin => Self::Builtin,
                Self::Parsed(l) => Self::Parsed(Loc {
                    line: l.line + line,
                    offset: l.offset + offset,
                }),
            }
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub struct Loc {
        //pub absolute: usize,
        pub line: isize,
        pub offset: isize,
    }

    impl std::fmt::Display for CodeLocation {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Parsed(l) => write!(f, "({}:{})", l.line, l.offset),
                Self::Builtin => write!(f, "(builtin)"),
            }
            //write!(f, "({}:{})", self.line, self.offset)
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub struct TokenWrapper<'a> {
        pub token: crate::lex::Token,
        pub slice: &'a str,
        pub start: CodeLocation,
        pub end: CodeLocation,
    }

    #[derive(Debug, Clone)]
    pub enum ParseResultError<'a> {
        EndOfFile,
        NotYetParsed,
        //ExpectedExpressionNotPresent,
        UnexpectedToken(TokenWrapper<'a>, Vec<crate::lex::Token>),
        SemanticIssue(&'a str, CodeLocation, CodeLocation),
    }

    impl<'a> ParseResultError<'a> {
        pub fn add_expect(&mut self, toks: &[crate::lex::Token]) {
            match self {
                Self::UnexpectedToken(_tw, v) => {
                    v.extend(toks);
                }
                _ => {}
            }
        }
    }

    impl<'a> Wrapper<'a> {
        pub fn new(input: &'a str) -> Wrapper<'a> {
            let lex = crate::lex::Token::lexer(input);

            Wrapper {
                lexer: lex,
                cur: Err(ParseResultError::NotYetParsed),
                last_newline_absolute: 0,
                current_line: 1,
            }
        }

        pub fn peek(&mut self) -> ParseResult<'a> {
            self.cur.clone()
        }

        pub fn advance(&mut self) -> () {
            let tok = self.lexer.next();
            match tok {
                Some(tok) => {
                    let (startloc, endloc) = match tok {
                        crate::lex::Token::Newline => {
                            let sp = self.lexer.span();

                            let start = Loc {
                                line: self.current_line,
                                offset: (sp.start - self.last_newline_absolute) as isize,
                            };

                            self.current_line += 1;
                            self.last_newline_absolute = sp.end;

                            let end = Loc {
                                line: self.current_line,
                                offset: (sp.end - self.last_newline_absolute) as isize,
                            };

                            (start, end)
                        }
                        _ => {
                            let sp = self.lexer.span();
                            let start = Loc {
                                line: self.current_line,
                                offset: (sp.start - self.last_newline_absolute) as isize,
                            };
                            let end = Loc {
                                line: self.current_line,
                                offset: (sp.end - self.last_newline_absolute) as isize,
                            };
                            (start, end)
                        }
                    };
                    self.cur = Ok(TokenWrapper {
                        token: tok,
                        slice: self.lexer.slice(),
                        start: CodeLocation::Parsed(startloc),
                        end: CodeLocation::Parsed(endloc),
                    })
                }
                None => self.cur = Err(ParseResultError::EndOfFile),
            }
        }

        pub fn next(&mut self) -> ParseResult<'a> {
            self.advance();
            self.peek()
        }
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
            while let Ok(tw) = w.next() {
                // handle comments
                use crate::lex::Token;
                match tw.token {
                    Token::LineCommentStart => {
                        inside_line_comment = true;
                        continue;
                    }
                    Token::Newline => {
                        inside_line_comment = false;
                        continue;
                    }
                    Token::LBlockComment | Token::LDocComment => {
                        comment_level += 1;
                        continue;
                    }
                    Token::RBlockComment | Token::RDocComment => {
                        if comment_level > 0 {
                            comment_level -= 1; // will cause syntax error in else during parse
                        }

                        continue;
                    }
                    _ => {}
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

            r
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
            if index < 0 {
                Err(ParseResultError::NotYetParsed)
            } else {
                let r = self
                    .tokens
                    .get(index as usize)
                    .map_or(Err(ParseResultError::EndOfFile), |&t| Ok(t));

                //println!("la gives result: {:?}", r);

                r
            }
        }
    }
}
