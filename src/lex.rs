use logos::Logos;

#[allow(non_camel_case_types)]
#[derive(Logos, Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Token {
    #[token("mod")]
    Module,

    #[token("public")]
    Public,

    #[token("private")]
    Private,

    #[token("use")]
    Use,

    #[token("global")]
    Global,

    #[token("super")]
    Super,

    //#[token("private")]
    //Private,
    #[token("mut")]
    Mutable,

    #[token("nomut")]
    Immutable,

    #[token("dyn")]
    Dynamic,

    #[token("nodyn")]
    Nodynamic,

    #[token("fn")]
    Function,

    #[token("let")]
    Let,

    #[token("if")]
    If,

    #[token("as")]
    As,

    #[token("else")]
    Else,

    #[token("for")]
    For,

    #[token("while")]
    While,

    #[token("anon")]
    Lambda,

    #[token("return")]
    Return,

    #[token("break")]
    Break,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("struct")]
    Struct,

    #[token(";")]
    Semicolon,

    #[token(":")]
    Colon,

    #[token("::")]
    DoubleColon,

    #[token("%")]
    Modulo,

    #[token("&&")]
    LogicalAnd,

    #[token("||")]
    LogicalOr,

    #[token("&")]
    Ampersand,

    #[token("^")]
    Caret,

    #[token("#")]
    Pound,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("[")]
    RBracket,

    #[token("]")]
    LBracket,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token(",")]
    Comma,

    #[token("/*")]
    LBlockComment,

    #[token("*/")]
    RBlockComment,

    #[token("/**")]
    LDocComment,

    #[token("**/")]
    RDocComment,

    #[token("*")]
    Asterisk,

    #[token("/")]
    FSlash,

    #[token("-")]
    Dash,

    #[token("+")]
    Plus,

    #[token("=")]
    Equals,

    #[token("!=")]
    CmpNotEqual,

    #[token("==")]
    CmpEqual,

    #[token("<=")]
    CmpLessThanOrEqual,

    #[token(">=")]
    CmpGreaterThanOrEqual,

    #[token("<")]
    CmpLessThan,

    #[token(">")]
    CmpGreaterThan,

    #[token("?=")]
    QueryAssign,

    #[token("?")]
    QuestionMark,

    #[token("!")]
    Bang,

    #[token("|")]
    Pipe,

    #[token("->")]
    ThinArrow,

    #[token("=>")]
    ThickArrow,

    //#[token(">>")]
    //ShiftRight,

    //#[token("<<")]
    //ShiftLeft,
    #[token("\\t")]
    EscapeTab,

    #[token("\\n")]
    EscapeNewline,

    //#[regex("//.*\n")]
    //LineCommentStart,

    /*#[token("\n")]
    Newline,*/
    #[token(".")]
    Dot,

    #[regex("[a-zA-Z][a-zA-Z0-9_]*")]
    Identifier, // get content with lex.slice()

    #[regex("[_]")]
    Underscore,

    #[regex("[0-9]+")]
    UnknownIntegerLiteral,

    #[regex("[0-9]+u128")]
    u128Literal,

    #[regex("[0-9]+u64")]
    u64Literal,

    #[regex("[0-9]+u32")]
    u32Literal,

    #[regex("[0-9]+u16")]
    u16Literal,

    #[regex("[0-9]+u8")]
    u8Literal,

    #[regex("[0-9]+i128")]
    i128Literal,

    #[regex("[0-9]+i64")]
    i64Literal,

    #[regex("[0-9]+i32")]
    i32Literal,

    #[regex("[0-9]+i16")]
    i16Literal,

    #[regex("[0-9]+i8")]
    i8Literal,

    #[regex("[0-9]+f64")]
    f64Literal,

    #[regex("[0-9]+f32")]
    f32Literal,

    #[token(" ")]
    Space,

    #[token("\t")]
    Tab,

    #[token("\n")]
    Newline,

    #[token("//")]
    LineCommentStart,

    #[regex(r#""([^"\\]|\\t|\\u|\\n|\\")*""#)]
    StringLiteral,

    #[token("llvm{")]
    LLVMOpen,

    #[token("}llvm")]
    LLVMClose,

    #[token("llvm:")]
    LLVMLineStart,

    // no related token, this is used by LookaheadStream
    LLVMBlock,

    //#[regex(r"llvm.*?llvm")]
    //InteriorLLVMInlineBlock,
    #[token("#builtin")]
    InteriorBuiltin,

    #[token("#bind")]
    LL_Bind,

    #[token("#var")]
    LL_Var,

    #[token("#result")]
    LL_Result,

    //#[regex(r"[\t\f]+", logos::skip)]
    #[error]
    Error,
}

impl Token {
    /*pub fn operator(&self) -> bool {
        self.binary_operator() || self.unary_operator()
    }

    pub fn binary_operator(&self) -> bool {
        match self {
            Token::Asterisk | Token::FSlash | Token::Dash | Token::Plus | Token::Equals => true,
            _ => false
        }
    }

    pub fn unary_operator(&self) -> bool {
        match self {
            Token::Asterisk | Token::Dash | Token::Bang => true,
            _ => false
        }
    }

    pub fn prefix_operator(&self) -> bool {
        match self {
            Token::Asterisk | Token::Dash | Token::Bang => true,
            _ => false
        }
    }

    pub fn infix_binding_power(&self) -> Option<(u32, u32)> {
        match self {
            Token::Equals => Some((2, 1)),
            Token::Plus | Token::Dash => Some((3, 4)),
            Token::Asterisk | Token::FSlash => Some((5, 6)),
            _ => None,
        }
    }*/
}

#[allow(non_camel_case_types)]
#[derive(Logos, Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum LLVMToken {
    #[token("{{")]
    Open,

    #[token("}}")]
    Close,

    #[regex(r".")]
    Other,

    #[error]
    Error,
}

#[test]
fn string_literal() {
    let pstring = "\"test string literal with keywords like fn mod and \\n\"";
    let mut lex = Token::lexer(pstring);
    let t = lex.next();
    assert!(matches!(t, Some(Token::StringLiteral)));
    assert_eq!(lex.slice(), pstring);
    //println!("{}", lex.slice());
    assert!(matches!(lex.next(), None));
}

use crate::{
    helper::interner::*,
    parse::{LexerStreamHandle, ParseValueGuard, SyncSliceHandle},
};
use smallvec::SmallVec;

type LexResult = Result<TokenWrapper, ParseResultError>;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
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

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Loc {
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
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TokenWrapper {
    pub token: crate::lex::Token,
    pub slice: IStr,
    pub start: CodeLocation,
    pub end: CodeLocation,
}

/*pub enum Error {
    FileError(FileResultError),
    ParseError(ParseResultError),
}*/

/*pub enum FileResultError {
    FileNotFound { filename: String },
}*/

#[derive(Debug, Clone)]
pub enum ParseResultError {
    InternalParseIssue,
    EndOfFile,
    NotYetParsed,
    /// The found token (and position), followed by a list of possible tokens here, followed by
    /// a message (if applicable)
    UnexpectedToken(TokenWrapper, Vec<crate::lex::Token>, Option<&'static str>),
    SemanticIssue(&'static str, CodeLocation, CodeLocation),
    ErrorWithHint {
        hint: &'static str,
        original: Box<ParseResultError>,
    },
}

impl ParseResultError {
    pub fn add_expect(&mut self, toks: &[crate::lex::Token]) {
        match self {
            Self::UnexpectedToken(_tw, v, None) => {
                v.extend(toks);
            }
            _ => {}
        }
    }
}

pub struct TokenStream<'a> {
    lexer: logos::Lexer<'a, crate::lex::Token>,
    cur: Result<TokenWrapper, ParseResultError>,

    current_line: isize,
    last_newline_absolute: usize,
    file_id: usize,
}

impl<'a> TokenStream<'a> {
    pub fn new(input: &'a str, file_id: usize) -> TokenStream<'a> {
        let lex = crate::lex::Token::lexer(input);

        TokenStream {
            lexer: lex,
            cur: Err(ParseResultError::NotYetParsed),
            last_newline_absolute: 0,
            current_line: 1,
            file_id,
        }
    }

    pub fn peek(&mut self) -> LexResult {
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
                    //slice: interner().get_or_intern(self.lexer.slice()),
                    slice: intern(self.lexer.slice()),
                    start: CodeLocation::Parsed(startloc),
                    end: CodeLocation::Parsed(endloc),
                })
            }
            None => self.cur = Err(ParseResultError::EndOfFile),
        }
    }

    pub fn next(&mut self) -> LexResult {
        self.advance();
        self.peek()
    }

    pub fn to_vec(self) -> Vec<TokenWrapper> {
        let mut v = Vec::new();
        let mut comment_level = 0;
        let mut inside_line_comment = false;

        // NOTE: we keep this outside the loop here so that we can simply "truncate" when done
        // with it and keep the allocation for use with later blocks
        //
        // This technically grows unbounded, but it's bounded same as string interner to
        // input size in total * 2 so we don't worry about it hanging around a bit longer
        // during lex
        let mut llvm_rest = String::new();

        while let Ok(mut tw) = self.next() {
            // handle comments
            match tw.token {
                Token::LLVMOpen => {
                    // do the parsing of the llvm block in its entirety here
                    // the parser will never actually see an LLVMOpen or LLVMClose

                    'llvm_collector: while let Ok(itw) = self.next() {
                        tw.end = itw.end;
                        match itw.token {
                            Token::LLVMClose => {
                                break 'llvm_collector;
                            }
                            other => {
                                println!("llvm pushes token {:?} with slice {}", other, itw.slice);
                                llvm_rest.push_str(itw.slice.resolve());
                            }
                        }
                    }

                    tw.token = Token::LLVMBlock;
                    tw.slice = intern(llvm_rest.as_str());

                    // need to empty the string since it lives outside the loop
                    // this preserves the allocation, though
                    llvm_rest.truncate(0);
                }
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
                Token::Tab | Token::Space => {
                    // these don't have any syntactic meaning so we simply filter them
                    // we keep them in so that they are fed through to llvm, however
                    continue;
                }
                Token::Error => {
                    // filter these outside of llvm blocks
                    continue;
                }
                _ => {}
            }
            if !inside_line_comment && comment_level == 0 {
                v.push(tw);
            }
        }

        v
    }
}

pub struct LookaheadHandle<'tokenvec> {
    tokens: &'tokenvec Vec<TokenWrapper>,
    index: usize,
}

impl<'tokenvec> LookaheadHandle<'tokenvec> {
    pub fn zeroed(tokens: &'tokenvec Vec<TokenWrapper>) -> Self {
        Self { tokens, index: 0 }
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

    pub fn next(&mut self) -> LexResult {
        //self.tokens[self.index]
        let r = self.la(0);
        //self.latest = Some(r);

        //self.index += 1;
        self.advance();

        r
    }

    pub fn prev(&mut self) -> LexResult {
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

    pub fn la(&mut self, offset: isize) -> LexResult {
        self.at(self.index as isize + offset)
    }

    pub fn at(&self, index: isize) -> LexResult {
        match index > 0 {
            true => match self.tokens.get(index as usize) {
                Some(t) => Ok(*t),
                None => Err(ParseResultError::EndOfFile),
            },
            false => Err(ParseResultError::NotYetParsed)
        }
    }

    /// If the current lookahead is the token passed, consume the token and
    /// return its metadata. Otherwise, do nothing and return None
    ///
    /// fast-cased version of eat_match_in for when only one token would be possible
    pub fn eat_match(
        &mut self,
        next: LookaheadHandle,
        t: Token,
    ) -> Option<ParseValueGuard<TokenWrapper>> {
        self.eat_match_in(next, [t])
    }

    /// If the current lookahead is within the [Token] slice passed, consume the token and
    /// return its metadata. Otherwise, do nothing and return None
    pub fn eat_match_in<const LEN: usize>(
        &mut self,
        next: LexerStreamHandle,
        t: [Token; LEN],
    ) -> Option<ParseValueGuard<TokenWrapper>> {
        if let Ok(tw) = self.la(0) {
            if t.contains(&tw.token) {
                self.advance();

                Some(tw)
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Consumes a token if the passed closure returns Some(T),
    /// returning a tuple of the returned T and the token (+metadata) that was consumed
    pub fn eat_if<F, T>(&mut self, f: F) -> Option<(T, TokenWrapper)>
    where
        F: FnOnce(TokenWrapper) -> Option<T>,
    {
        match self.la(0) {
            Ok(tw) => {
                let result_f = f(tw);
                let result = match result_f {
                    Some(r) => Some((r, tw)),
                    None => None,
                };
                if result.is_some() {
                    self.advance();
                }

                result
            }
            Err(_) => None,
        }
    }

    /// Reports an error if next token is not within the [expected] slice
    /// Does not consume the expected token, should be used as an error-reporting
    /// form of eat_to
    pub fn expect_next_in(&mut self, expected: &[Token]) -> Result<(), ParseResultError> {
        let first = self.la(0);
        let sync = self.sync_next(expected);
        let corrected = self.synchronize();

        if corrected {
            println!("corrective action had to be taken");
            if let Ok(tw) = first {
                self.report_err(ParseResultError::UnexpectedToken(
                    tw,
                    expected.to_vec(),
                    None,
                    //expected.iter().cloned().collect(),
                ));
            } else {
                self.report_err(ParseResultError::EndOfFile);
            }
        }

        match self.unsync(sync) {
            Err(e) => {
                // found a synchronization point for a sync point in a superscope, so the current
                // scope's sync point is not usable. Parent scope will not get their expect_next,
                // so return err
                Err(e)
            }
            Ok(_) => {
                // found a synchronization point that allows for parse recovery, so return to
                // parent scope an Ok result
                Ok(())
            }
        }
    }

    /// Will return a failing result if parsing fails, but will not attempt to reallign input or
    /// independently dispatch any error notifications, and will not consume any erroneous input
    pub fn soft_expect(&mut self, expected: Token) -> Result<TokenWrapper, ParseResultError> {
        if let Ok(tw) = self.la(0) {
            if tw.token == expected {
                self.advance();
                Ok(tw)
            } else {
                Err(ParseResultError::UnexpectedToken(tw, vec![expected], None))
            }
        } else {
            Err(ParseResultError::EndOfFile)
        }
    }

    /// Behavior similar to soft_expect, but will attempt to reallign input stream to get to the
    /// token
    pub fn hard_expect(&mut self, expected: Token) -> Result<TokenWrapper, ParseResultError> {
        self.expect_next_in(&[expected])?;

        if let Ok(tw) = self.la(0) {
            if tw.token == expected {
                self.advance();
                Ok(tw)
            } else {
                Err(ParseResultError::UnexpectedToken(tw, vec![expected], None))
            }
        } else {
            Err(ParseResultError::EndOfFile)
        }
    }

    pub fn eat_match_string<const LEN: usize>(
        &mut self,
        expected: [Token; LEN],
    ) -> Result<SmallVec<[TokenWrapper; LEN]>, ParseResultError> {
        let old_idx = self.index();

        let mut sv: SmallVec<[TokenWrapper; LEN]> = SmallVec::new();
        for i in 0..LEN {
            let m = self.eat_match(expected[i]);
            match m {
                Some(tw) => {
                    sv.push(tw);
                }
                None => {
                    self.seek_to(old_idx);
                    return match self.la(0) {
                        Err(e) => Err(e),
                        Ok(tw) => Err(ParseResultError::UnexpectedToken(tw, expected.into(), None)),
                    };
                }
            }
        }

        Ok(sv)
    }

    /// Mark what tokens could feasibly come after some recursive parse state call that the current
    /// parse state wants to capture and handle.
    pub fn sync_next(&mut self, next: &[Token]) -> SyncSliceHandle {
        let start_len = self.next.len();

        self.next.extend(next.iter());

        SyncSliceHandle { start: start_len }
    }

    /// Remove the current recovery frame from the recovery stack,
    /// pass the handle that was provided by sync_next to remove the correct frame
    pub fn unsync(&mut self, handle: SyncSliceHandle) -> Result<(), ParseResultError> {
        if self.next.len() < handle.start {
            // maybe just < rather than <=?
            Err(ParseResultError::InternalParseIssue)
        } else {
            self.next.truncate(handle.start);
            Ok(())
        }
    }

    /// Eat up to, return whether any synchronization action was required
    ///
    /// The synchronization algorithm tries to avoid dropping as much spurious input as possible,
    /// and instead assumes the user has generally not completed typing.
    ///
    /// It will only drop an input if the `next` set does not contain the provided token.
    ///
    /// If the `next` set *does* contain the provided token, then it will
    /// remove any entries in the set more recent than that entry, and
    /// signal to restart parsing in the state that matches that entry by
    /// having unsync(...) only return Ok once that recovery scope is reached
    pub fn synchronize(&mut self) -> bool {
        let mut r = false;
        loop {
            if let Ok(tok) = self.la(0) {
                if let Some(index) = self.next.iter().rposition(|ntok| *ntok == tok.token) {
                    self.next.truncate(index + 1);
                    return r;
                } else {
                    self.advance();
                    r = true;
                }
            } else {
                // EOF
                self.next.clear(); // maintain average capacity by not reallocating
                break;
            }
        }

        return r;
    }
}

//use crate::lex::Token;

/*impl LookaheadStream {

    pub fn at(&self, index: isize) -> LexResult {
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
}*/
