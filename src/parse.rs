
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

use crate::helper::lex_wrap::TokenWrapper;
use crate::helper::lex_wrap::ParseResultError;
use crate::helper::lex_wrap::LookaheadStream;
use std::collections::HashSet;

use crate::parse_expr::*;

use crate::parse_helper::*;

//use crate::grammar::*;

/*lazy_static! {
    static ref expression_parser: OuterExpressionParser = OuterExpressionParser::new();
    //static ref type_parser: OuterExpressionParser = 
}*/

type TokenResult<'a> = Result<TokenWrapper<'a>, ParseResultError<'a>>;

pub fn entry<'a>(la: &mut LookaheadStream<'a>) -> Result<ast::ParseUnit<'a>, ParseResultError<'a>> {
    //let mut declarations
    let mut declarations: Vec<Result<ast::SymbolDeclaration<'a>, ParseResultError<'a>>> = Vec::new();

    let start = la.la(0).map_or(0, |tw| tw.start);

    let mut failed = false;

    while let Ok(tw) = la.next() {

        println!("In entry while, got tw {:?}", tw);
        let r = match tw.token {
            //Token::Module => Ok(ast::SymbolDeclaration::NamespaceDeclaration(module_entry(la)?)),
            //Token::LBlockComment => { block_comment(la); continue },
            Token::RBrace => break,
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

    let end = la.la(-1).map_or(start, |tw| tw.start);
    
    Ok(ast::ParseUnit { declarations, node_info: ast::NodeInfo::from_indices(failed, start, end) })
}

/*pub fn function_declaration<'a>(la: &mut LookaheadStream<'a>) -> Result<ast::FunctionDeclaration<'a>, ParseResultError<'a>> {
    expect(la, Token::Function)?;
    let parameters = paren_param_list(la)?;
    let return_type_arrow = eat_if(la, Token::ThinArrow);
    let return_type = match return_type_arrow {
        Some(_) => parse_type(la)?,
        None => ast::TypeReference::unit(),
    };
}

pub fn paren_param_list<'a>(la: &mut LookaheadStream<'a>) -> Result<Vec<ast::VariableDeclaration<'a>>, ParseResultError<'a>> {
    //
}

pub fn param_list<'a>(la: &mut LookaheadStream<'a>) -> Result<Vec<ast::VariableDeclaration<'a>>, ParseResultError<'a>> {
}

pub fn type_list<'a>(la: &mut LookaheadStream<'a>) -> Result<Vec<ast::TypeReference<'a>>, ParseResultError<'a>> {
}

pub fn parse_type<'a>(la: &mut LookaheadStream<'a>) -> Result<ast::TypeReference<'a>, ParseResultError<'a>> {
}*/

pub fn global_declaration<'a>(la: &mut LookaheadStream<'a>) -> Result<ast::SymbolDeclaration<'a>, ParseResultError<'a>> {
    let has_pub = eat_if_matches(la, Token::Public);
    let mut failed = false;

    if let Ok(tw) = la.la(0) {
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
                namespace(la)
                    .map(|mut ns| {
                        ns.set_public(has_pub.is_some()); ast::SymbolDeclaration::NamespaceDeclaration(ns)
                    })
                    .map_err(|e| {
                        failed = true;
                        e
                    })
            },
            Token::Let => {
                variable_declaration(la)
                    .map(|mut vd| {
                        ast::SymbolDeclaration::VariableDeclaration(vd)
                    })
                    .map_err(|e| {
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

pub fn namespace<'a>(lexer: &mut LookaheadStream<'a>) -> Result<ast::Namespace<'a>, ParseResultError<'a>> {
    let start = lexer.la(0).map_or(0, |tw| tw.start);

    expect(lexer, Token::Module)?;
    println!("Module entry now");
    let id = expect(lexer, Token::Identifier)?.slice;
    expect(lexer, Token::LBrace)?;
    let pu = entry(lexer);
    //expect(lexer, Token::RBrace)?;
    println!("Module entry finds id: {:?}", id);

    let end = lexer.la(-1).map_or(0, |tw| tw.end);

    let failed = pu.is_err();

    let node_info = ast::NodeInfo::from_indices(true, start, end);

    Ok(ast::Namespace { name: Some(id), contents: pu, public: false, node_info })

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

pub fn variable_declaration<'a>(lexer: &mut LookaheadStream<'a>) -> Result<ast::VariableDeclaration<'a>, ParseResultError<'a>> {
    println!("Got a variable declaration, looking...");
    let start = lexer.la(0).map_or(0, |tw| tw.start);

    let _let = expect(lexer, Token::Let)?;
    let id = expect(lexer, Token::Identifier)?.slice;
    let maybe_typeref = eat_if_matches(lexer, Token::Colon);
    println!("Typeref colon: {:?}", maybe_typeref);

    let tr = match maybe_typeref {
        Some(_) => {
            println!("Got colon, expecting identifier. LookAhead is: {:?}", lexer.la(0));
            let t = expect(lexer, Token::Identifier)?;

            let node_info = ast::NodeInfo::from_indices(true, t.start, t.end);

            Some(ast::TypeReference { node_info, typename: t.slice, refers_to: None })
        },
        None => None,
    };


    let equals = eat_if_matches(lexer, Token::Equals);
    println!("Equals is: {:?}", equals);

    let var_expr = parse_expr(lexer)?;

    //let sent_lexer = lexer.clone();
    /*let mut lw = crate::parse_expr::LALRPopLexWrapper::new(lexer, vec![Token::Semicolon]);


    let expr = match equals {
        Some(_) => {
            println!("Handing off to lalrpop to parse expr");
            //let parser = crate::grammar::OuterExpressionParser::new();
            let result = expression_parser.parse("", &mut lw).unwrap();
            //Some(ast::Expression::Variable(result))
            Some(result)
        },
        None => None,
    };

    //lexer.ffwd(&lw.la);*/

    expect(lexer, Token::Semicolon)?;

    let end = lexer.la(-1).map_or(start, |tw| tw.start);

    /*let expr = expr.map(|_| {
        let mut lw = crate::parse_expr::LALRPopLexWrapper { la: lexer, end_with: vec![Token::Semicolon] };
        let mut parser = crate::grammar::OuterExpressionParser::new();
        //crate::grammar::
    });*/
    /*let expr = if_token(lexer, Token::Equals).then(|| {
        
    });*/
    //panic!()
    
    let node_info = ast::NodeInfo::from_indices(true, start, end);

    Ok(ast::VariableDeclaration {
        node_info,
        name: id,
        var_expr: Some(var_expr),
        var_type: tr,
    })
}

pub fn closure<'a>(la: &mut LookaheadStream<'a>) -> Result<ast::Closure<'a>, ParseResultError<'a>> {
    panic!()
}

// no result from comments
/*pub fn block_comment(la: &mut LookaheadStream) -> () {
    while let Ok(tw) = la.next() {
        match tw.token {
            Token::LBlockComment => block_comment(la),
            Token::RBlockComment => return,
            _ => continue,
        }
    }
}*/

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

//type ExpressionResult<'a> = Result<ast::ExpressionWrapper<'a>, ParseResultError<'a>>;

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

