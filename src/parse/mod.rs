mod parse_base;
mod parse_expr;
mod parse_helper;

pub use parse_base::*;
pub use parse_expr::*;
pub use parse_helper::*;

use crate::helper::lex_wrap::TokenWrapper;
use crate::helper::lex_wrap::ParseResultError;
use crate::helper::lex_wrap::LookaheadStream;

pub struct Parser<'b, 'a> {
    lex: &'b mut LookaheadStream<'a>,
}
