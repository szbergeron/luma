//use crate::lex;
use crate::ast::*;
use std::fs;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};

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

#[derive(Debug, Clone)]
pub enum Error<'a> {
    DuplicateDefinition {
        duplicate_symbol: Arc<RwLock<SymbolDeclaration<'a>>>,
        existing_symbol: Arc<RwLock<SymbolDeclaration<'a>>>,
    },
}

use crate::mid_repr::ScopeContext;

pub type PathId = usize;
pub type PathIdMapHandle<'a> = Arc<RwLock<PathIdMap<'a>>>;

#[allow(dead_code)]
pub struct FileHandle<'a> {
    id: usize,
    context: Arc<RwLock<ScopeContext<'a>>>,
    location: PathBuf,
    //scope: Vec<String>,
    contents: Option<Arc<String>>,
}

impl<'a> FileHandle<'a> {
    pub fn new(
        p: PathBuf,
        /*scope: Vec<String>,*/ id: usize,
        context: Arc<RwLock<ScopeContext<'a>>>,
    ) -> FileHandle {
        FileHandle {
            location: p,
            //scope,
            id,
            context,
            contents: None,
        }
    }

    pub fn open(&mut self) -> std::io::Result<Arc<String>> {
        match self.contents.clone() {
            Some(contents) => Ok(contents.clone()),
            None => {
                let content_string = fs::read_to_string(self.location.clone())?;
                let content_rc = Arc::new(content_string);
                let result = content_rc.clone();
                self.contents = Some(content_rc);

                Ok(result)
            }
        }
    }

    pub fn close(&mut self) {
        self.contents = None; // any remaining Rcs will need to drop before string drops
    }

    pub fn path(&self) -> &PathBuf {
        &self.location
    }

    pub fn context(&self) -> Arc<RwLock<ScopeContext<'a>>> {
        self.context.clone()
    }
}

pub struct PathIdMap<'a> {
    paths: Vec<Option<FileHandle<'a>>>,
}

impl<'a> PathIdMap<'a> {
    pub fn new_locked() -> Arc<RwLock<PathIdMap<'a>>> {
        Arc::new(RwLock::new(Self::new()))
    }

    pub fn new() -> PathIdMap<'a> {
        let v = vec![None];

        PathIdMap { paths: v }
    }

    pub fn push_path(
        &mut self,
        p: PathBuf,
        /*scope: Vec<String>,*/ context: Arc<RwLock<ScopeContext<'a>>>,
    ) -> PathId {
        let id = self.paths.len();
        self.paths.push(Some(FileHandle::new(p, id, context)));

        id
    }

    pub fn get_path(&self, id: PathId) -> Option<&PathBuf> {
        match self.paths.get(id) {
            Some(Some(p)) => Some(p.path()),
            Some(None) => None,
            None => None,
        }
    }

    pub fn get_file(&self, id: PathId) -> Option<&FileHandle<'a>> {
        match self.paths.get(id) {
            Some(Some(f)) => Some(f),
            _ => None,
        }
    }

    pub fn get_handles(&mut self) -> &mut [Option<FileHandle<'a>>] {
        &mut self.paths[..]
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
        file_id: usize,
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
                    file_id: l.file_id,
                }),
            }
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub struct Loc {
        //pub absolute: usize,
        pub line: isize,
        pub offset: isize,
        pub file_id: usize,
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
        pub fn new(input: &'a str, file_id: usize) -> Wrapper<'a> {
            let lex = crate::lex::Token::lexer(input);

            Wrapper {
                lexer: lex,
                cur: Err(ParseResultError::NotYetParsed),
                last_newline_absolute: 0,
                current_line: 1,
                file_id,
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
                                file_id: self.file_id,
                            };

                            self.current_line += 1;
                            self.last_newline_absolute = sp.end;

                            let end = Loc {
                                line: self.current_line,
                                offset: (sp.end - self.last_newline_absolute) as isize,
                                file_id: self.file_id,
                            };

                            (start, end)
                        }
                        _ => {
                            let sp = self.lexer.span();
                            let start = Loc {
                                line: self.current_line,
                                offset: (sp.start - self.last_newline_absolute) as isize,
                                file_id: self.file_id,
                            };
                            let end = Loc {
                                line: self.current_line,
                                offset: (sp.end - self.last_newline_absolute) as isize,
                                file_id: self.file_id,
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
