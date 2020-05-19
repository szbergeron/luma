use logos::Logos;

pub fn compile(contents: &str) {
    //let mut lex = crate::lex::Token::lexer(contents);
    let mut lex = crate::helper::lex_wrap::Wrapper::new(contents);
    //let mut lex = crate::helper::

    /*while let Ok(token) = lex.next_token() {
        println!("Got token: {:?}", token);
        //println!("Slice is: {:?}", token.slice);
    }*/

    parser::entry(&mut lex);
}

pub mod parser {
    use crate::ast;
    use crate::lex::Token;
    use crate::helper::lex_wrap::Wrapper;
    use crate::helper::lex_wrap::TokenWrapper;
    use crate::helper::lex_wrap::ParseResultError;

    type TokenResult<'a> = Result<TokenWrapper<'a>, ParseResultError<'a>>;

    pub fn entry<'a>(lexer: &mut crate::helper::lex_wrap::Wrapper<'a>) -> Result<ast::ParseUnit<'a>, ParseResultError<'a>> {
        //let mut declarations
        let mut declarations: Vec<ast::SymbolDeclaration> = Vec::new();

        while let Ok(tw) = lexer.next() {
            println!("In entry while, got tw {:?}", tw);
            let r = match tw.token {
                Token::Module => module_entry(lexer),
                Token::LBrace => break,
                _ => panic!("Failed to parse! Got token: {:?}", tw),
            };
            println!("Got a semantic thing in entry: {:?}", r);

            declarations.push(ast::SymbolDeclaration::NamespaceDeclaration(r?));
        }
        println!("Entry returns, declarations are: {:?}", declarations);
        
        Ok(ast::ParseUnit { declarations })
    }

    pub fn module_entry<'a>(lexer: &mut Wrapper<'a>) -> Result<ast::Namespace<'a>, ParseResultError<'a>> {
        println!("Module entry now");
        let id = expect(lexer, Token::Identifier)?.slice;
        expect(lexer, Token::LBrace)?;
        let pu = entry(lexer)?;
        //expect(lexer, Token::RBrace)?;
        println!("Module entry finds id: {:?}", id);

        Ok(ast::Namespace { name: Some(id), contents: pu })

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

    pub fn syntactic_block(_lexer: &mut Wrapper) {
    }

    // no result from comments
    pub fn block_comment(lexer: &mut Wrapper) -> () {
        while let Ok(tw) = lexer.next() {
            match tw.token {
                Token::LBlockComment => block_comment(lexer),
                Token::RBlockComment => return,
                _ => continue,
            }
        }
    }

    fn expect<'a>(lexer: &mut Wrapper<'a>, t: Token) -> Result<TokenWrapper<'a>, ParseResultError<'a>> {
        if let Ok(tw) = lexer.next() {
            match tw.token {
                t => Ok(tw),
                _ => Err(ParseResultError::UnexpectedToken(tw)),
            }
        } else {
            Err(ParseResultError::EndOfFile)
        }
    }
}
