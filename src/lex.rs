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

    #[token(" ", logos::skip)]
    Space,

    #[token("\t", logos::skip)]
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
