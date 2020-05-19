use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
pub enum Token {
    #[token("mod")]
    Module,

    #[token("fn")]
    Function,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("for")]
    For,

    #[token("while")]
    While,

    #[token("{")]
    RBrace,

    #[token("}")]
    LBrace,

    #[token("[")]
    RBracket,

    #[token("]")]
    LBracket,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("/*")]
    LBlockComment,

    #[token("*/")]
    RBlockComment,

    #[token("/**")]
    LDocComment,

    #[token("**/")]
    RDocComment,

    #[regex("//.*\n")]
    LineCommentStart,

    /*#[token("\n")]
    Newline,*/

    #[token(".")]
    Dot,

    #[regex("[a-zA-Z][a-zA-Z0-9_]*")]
    Identifier, // get content with lex.slice()

    #[regex("[0-9]+")]
    UnknownIntegerLiteral,

    #[token(" ", logos::skip)]
    Space,

    #[token("\t", logos::skip)]
    Tab,

    /*#[regex(".*")]
    CommentContents,*/

    #[regex(r"[\t\f\n]+", logos::skip)]
    #[error]
    Error,
}
