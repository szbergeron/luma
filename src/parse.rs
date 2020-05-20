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

    let r = parser::entry(&mut scanner);
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

pub mod parser {
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

    pub fn expression_outer(la: &mut LookaheadStream) -> () {
    }

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
}
