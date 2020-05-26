use logos::Logos;

pub fn compile(contents: &str) {
    //let mut lex = crate::lex::Token::lexer(contents);
    let mut lex = crate::helper::lex_wrap::Wrapper::new(contents);
    let mut scanner = crate::helper::lex_wrap::LookaheadStream::new(&mut lex);
    //let mut lex = crate::helper::

    /*while let Ok(token) = lex.next_token() {
        println!("Got token: {:?}", token);
        //println!("Slice is: {:?}", token.slice);
    }*/

    let r = entry(&mut scanner);
    //r.iter().for_each(|result|
    match r {
        Ok(punit) => {
            println!("Gets AST of: {}", punit);
        },
        Err(err) => {
            println!("Failed to parse with error: {:?}", err);
        }
    }
}

use crate::ast;
use crate::lex::Token;
use crate::helper::lex_wrap::Wrapper;
use crate::helper::lex_wrap::TokenWrapper;
use crate::helper::lex_wrap::ParseResultError;
use crate::helper::lex_wrap::LookaheadStream;
use std::collections::HashSet;

type TokenResult<'a> = Result<TokenWrapper<'a>, ParseResultError<'a>>;

pub fn entry<'a>(la: &mut LookaheadStream<'a>) -> Result<ast::ParseUnit<'a>, ParseResultError<'a>> {
    //let mut declarations
    let mut declarations: Vec<Result<ast::SymbolDeclaration<'a>, ParseResultError<'a>>> = Vec::new();

    let mut failed = false;

    while let Ok(tw) = la.next() {
        println!("In entry while, got tw {:?}", tw);
        let r = match tw.token {
            //Token::Module => Ok(ast::SymbolDeclaration::NamespaceDeclaration(module_entry(la)?)),
            Token::LBlockComment => { block_comment(la); continue },
            Token::LBrace => break,
            _ => {
                la.backtrack();
                let r = global_declaration(la);

                match r.is_ok() {
                    false => {
                        failed = true;
                        /*while let Ok(twi) = la.next() {
                            match twi.token {
                                Token::LBrace | Token::Semicolon => break,
                                _ => continue,
                            }

                        }*/
                        eat_through(la, vec![Token::RBrace, Token::Semicolon]);
                    },
                    true => {},
                }

                r

                //Err(ParseResultError::UnexpectedToken(tw))
            }
            //_ => panic!("Failed to parse! Got token: {:?}", tw),
        };
        println!("Got a semantic thing in entry: {:?}", r);

        declarations.push(r);
    }
    println!("Entry returns, declarations are: {:?}", declarations);
    
    Ok(ast::ParseUnit { declarations, failed })
}

pub fn global_declaration<'a>(la: &mut LookaheadStream<'a>) -> Result<ast::SymbolDeclaration<'a>, ParseResultError<'a>> {
    let has_pub = eat_if(la, Token::Public);
    let mut failed = false;

    if let Ok(tw) = la.next() {
        let r = match tw.token {
            Token::Module => {
                /*let mut m = module_entry(la);
                match m {
                    Ok(mut ns) => {
                        ns.public = has_pub.is_some();

                        Ok(ast::SymbolDeclaration::NamespaceDeclaration(ns))
                    },
                    Err(err) => Err(err),
                }*/
                //m.map(|ns| { ns.public = has_pub.is_some(); ns }).map(|ns| ast::SymbolDeclaration::NamespaceDeclaration(ns))
                module_entry(la).map(|mut ns| {
                    ns.public = has_pub.is_some(); ast::SymbolDeclaration::NamespaceDeclaration(ns)
                }).map_err(|e| {
                    failed = true;
                    e
                })
            },
            _ => {
                eat_through(la, vec![Token::RBrace, Token::Semicolon]);

                Err(ParseResultError::UnexpectedToken(tw))
            }
        };

        r
    } else {
        Err(ParseResultError::EndOfFile)
    }

    //panic!()
}

pub fn module_entry<'a>(lexer: &mut LookaheadStream<'a>) -> Result<ast::Namespace<'a>, ParseResultError<'a>> {
    println!("Module entry now");
    let id = expect(lexer, Token::Identifier)?.slice;
    expect(lexer, Token::RBrace)?;
    let pu = entry(lexer);
    //expect(lexer, Token::RBrace)?;
    println!("Module entry finds id: {:?}", id);

    let failed = pu.is_err();

    Ok(ast::Namespace { name: Some(id), contents: pu.ok(), failed, public: false })

    /*fn module_name<'a>(lexer: &mut Wrapper<'a>) -> TokenResult<'a> {
        expect(lexer, Token::Identifier)
    }

    fn module_lbrace<'a>(lexer: &mut Wrapper<'a>) -> TokenResult<'a> {
        expect(lexer, Token::LBrace)
    }

    fn module_*/


    //module_name(lexer)
    /*if let Token::Identifier = lexer::next_semantic_token() {
    } else {
        ParseResultError::
    }*/
}

pub fn closure<'a>(la: &mut LookaheadStream<'a>) -> Result<ast::Closure<'a>, ParseResultError<'a>> {
    panic!()
}

pub fn syntactic_block(_lexer: &mut LookaheadStream) {
}

// no result from comments
pub fn block_comment(la: &mut LookaheadStream) -> () {
    while let Ok(tw) = la.next() {
        match tw.token {
            Token::LBlockComment => block_comment(la),
            Token::RBlockComment => return,
            _ => continue,
        }
    }
}

// I really hate myself for this, but I'm going to potentially try both grammar based
// precedence and pratt parsing since both seem not great
pub fn expression_outer<'a>(la: &mut LookaheadStream<'a>) -> () {
    parse_expr(la, 0);

    fn parse_expr<'a>(la: &mut LookaheadStream<'a>, min_bp: u8) -> Result<(), ParseResultError<'a>> {
        while let Ok(tw) = la.next() {
            let operators: Vec<Token> = Vec::new();
            let operands: Vec<Box<dyn ast::Expression>> = Vec::new();
            
            enum State {
                //
            }

        }

        panic!()

        /*if let Ok(mut lhs) = {
            let token = la.next()?;
            match token.token {
                Token::LParen => {
                    //
                },
                Token::UnknownIntegerLiteral => {},
                t if t.prefix_operator() => {},
                _ => {
                    la.backtrack();
                    ParseResultError::UnexpectedToken(token)
                }
            }

            /*if token.token.prefix_operator() {
                let bp = token.token.prefix_binding_power();
            }*/
        } {
            Err(ParseResultError::NotYetParsed)
        } else {
            if let Ok(tw) = la.la(0) {
                Err(ParseResultError::UnexpectedToken(tw))
            } else {
                Err(ParseResultError::EndOfFile)
            }
        }*/

        /*while let Ok(tw) = la.next() {
            if tw.token.binary_operator() {
                let (l_bp, r_bp) = tw.token.infix_binding_power().expect("Token had no binding power");
            } else if tw.token.unary_operator() {
            } else {
                la.backtrack();
                return Err(ParseResultError::UnexpectedToken(tw));
            }
        }*/
    }
}

type ExpressionResult<'a> = Result<Box<dyn ast::Expression<'a>>, ParseResultError<'a>>;

/*pub fn outer_expression<'a>(la: &mut LookaheadStream<'a>, end_of_string: Token)
    -> Result<Box<dyn ast::Expression<'a>>, ParseResultError<'a>> {
}

pub fn parenthetical<'a>(la: &mut LookaheadStream<'a>)
    -> ExpressionResult<'a>
{
    let r = outer_expression(la, Token::RParen)?;
    expect(la, Token::RParen)?;

    Ok(r)
}*/

//fn find<'a>(lexer: &mut Wrapper<'a>, t: Token

fn eat_through<'a>(la: &mut LookaheadStream<'a>, toks: Vec<Token>) {
    let s: HashSet<Token> = toks.into_iter().collect();

    while let Ok(tw) = la.next() {
        if s.contains(&tw.token) {
            break;
        } else {
            continue;
        }
    }
}

fn eat_if<'a>(la: &mut LookaheadStream<'a>, t: Token) -> Option<TokenWrapper<'a>> {
    expect(la, t).ok()
    //expect(t).map_or(|t| Some(t), None)
}

fn expect<'a>(la: &mut LookaheadStream<'a>, t: Token) -> Result<TokenWrapper<'a>, ParseResultError<'a>> {
    println!("Expect asked for: {:?}", t);
    if let Ok(tw) = la.next() {
        match tw.token {
            tt if tt == t => Ok(tw),
            _ => {
                la.backtrack();

                Err(ParseResultError::UnexpectedToken(tw))
            },
        }
    } else {
        Err(ParseResultError::EndOfFile)
    }
}
