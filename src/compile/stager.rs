use crate::parse::Parser;
use crate::helper::lex_wrap::{Wrapper, LookaheadStream, ParseResultError};
use crate::ast;

pub fn compile(contents: &str) {
    let mut lex = Wrapper::new(contents);
    let mut scanner = LookaheadStream::new(&mut lex);

    let mut parser = Parser::new(&mut scanner);

    let p = parser.entry();

    parser.print_errors(contents);


    //
    match p {
        Ok(mut punit) => {
            println!("Gets AST of: {}", punit);
            analyze(&mut punit);
        }
        Err(err) => {
            println!("Failed to parse with error: {:?}", err);
        }
    }
}

/*pub fn parse<'a>(contents: &'a str) -> Result<ast::ParseUnit<'a>, ParseResultError<'a>> {


    r
}*/

pub fn prepass<'a>(p: &mut ast::ParseUnit<'a>) {
}

pub fn analyze<'a>(p: &mut ast::ParseUnit<'a>) {
}

pub fn tollvm<'a>(p: &mut ast::ParseUnit<'a>, _filename: &str) {
}
