use logos::Logos;

#[derive(Logos, Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Token {
    #[token("mod")]
    Module,

    #[token("pub")]
    Public,

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

    #[token(";")]
    Semicolon,

    #[token(":")]
    Colon,

    #[token("%")]
    Modulo,

    #[token("&&")]
    LogicalAnd,

    #[token("||")]
    LogicalOr,

    #[token("&")]
    And,

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

    #[token(">>")]
    ShiftRight,

    #[token("<<")]
    ShiftLeft,

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

    #[token(" ", logos::skip)]
    Space,

    #[token("\t", logos::skip)]
    Tab,

    #[token("\n")]
    Newline,

    #[token("//")]
    LineCommentStart,

    /*#[regex(".*")]
    CommentContents,*/

    #[regex(r"[\t\f]+", logos::skip)]
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
