pub fn compile(contents: &str) {
    //let mut lex = crate::lex::Token::lexer(contents);
    let mut lex = crate::helper::lex_wrap::Wrapper::new(contents);
    let mut scanner = crate::helper::lex_wrap::LookaheadStream::new(&mut lex);
    //let mut lex = crate::helper::

    /*while let Ok(token) = lex.next_token() {
        println!("Got token: {:?}", token);
        //println!("Slice is: {:?}", token.slice);
    }*/

    let mut parser = Parser::new(&mut scanner);

    let r = parser.entry();

    parser.print_errors(contents);
    //r.iter().for_each(|result|
    match r {
        Ok(punit) => {
            println!("Gets AST of: {}", punit);
        }
        Err(err) => {
            println!("Failed to parse with error: {:?}", err);
        }
    }
}

use crate::ast;
use crate::lex::Token;

use crate::helper::lex_wrap::LookaheadStream;
use crate::helper::lex_wrap::ParseResultError;
use crate::helper::lex_wrap::TokenWrapper;
use std::collections::HashSet;
use ast::IntoAstNode;

use crate::parse::*;

//use crate::parse_helper::*;

//use crate::grammar::*;

/*lazy_static! {
    static ref expression_parser: OuterExpressionParser = OuterExpressionParser::new();
    //static ref type_parser: OuterExpressionParser =
}*/

type TokenResult<'a> = Result<TokenWrapper<'a>, ParseResultError<'a>>;

impl<'b, 'a> Parser<'b, 'a> {

    pub fn entry(&mut self) -> Result<ast::ParseUnit<'a>, ParseResultError<'a>> {
        //let mut declarations
        let mut declarations: Vec<Result<ast::SymbolDeclaration<'a>, ParseResultError<'a>>> =
            Vec::new();

        let start = self.lex.la(0).map_or(0, |tw| tw.start);

        let mut failed = false;

        while let Ok(tw) = self.lex.la(0) {
            let r = match tw.token {
                Token::RBrace => break,
                _ => {
                    //
                    let r = self.global_declaration();

                    let r = match r {
                        Err(e) => {
                            failed = true;
                            self.report_err(e.clone());
                            self.eat_through(vec![Token::RBrace, Token::Semicolon]);

                            Err(e)
                        }
                        Ok(ok) => Ok(ok),
                    };

                    r
                }
            };

            declarations.push(r);
        }

        let end = self.lex.la(-1).map_or(start, |tw| tw.start);

        Ok(ast::ParseUnit {
            declarations,
            node_info: ast::NodeInfo::from_indices(failed, start, end),
        })
    }

    pub fn global_declaration(&mut self) -> Result<ast::SymbolDeclaration<'a>, ParseResultError<'a>> {
        let has_pub = self.eat_match(Token::Public);
        let mut failed = false;

        if let Ok(tw) = self.lex.la(0) {
            let r = match tw.token {
                Token::Module => {
                    self.namespace()
                        .map(|mut ns| {
                            ns.set_public(has_pub.is_some());
                            ast::SymbolDeclaration::NamespaceDeclaration(ns)
                        })
                        .map_err(|e| {
                            failed = true;
                            e
                        })
                }
                Token::Let => self.variable_declaration()
                    .map(|mut vd| ast::SymbolDeclaration::VariableDeclaration(vd))
                    .map_err(|e| {
                        failed = true;
                        e
                    }),
                Token::Function => self.function_declaration()
                    .map(|mut fd| ast::SymbolDeclaration::FunctionDeclaration(fd))
                    .map_err(|e| {
                        failed = true;
                        e
                    }),
                _ => {
                    self.eat_to(vec![Token::RBrace, Token::Semicolon]);

                    self.err(ParseResultError::UnexpectedToken(tw, vec![Token::Module, Token::Let, Token::Function]))
                }
            };

            r
        } else {
            Err(ParseResultError::EndOfFile)
        }

        //panic!()
    }

    pub fn namespace(
        &mut self,
    ) -> Result<ast::Namespace<'a>, ParseResultError<'a>> {
        let start = self.lex.la(0).map_or(0, |tw| tw.start);

        self.expect(Token::Module)?;
        println!("Module entry now");
        let id = self.expect(Token::Identifier)?.slice;
        self.expect(Token::LBrace)?;
        let pu = self.entry();
        self.expect(Token::RBrace)?;
        println!("Module entry finds id: {:?}", id);

        let end = self.lex.la(-1).map_or(0, |tw| tw.end);

        let failed = pu.is_err();

        let node_info = ast::NodeInfo::from_indices(failed, start, end);

        Ok(ast::Namespace {
            name: Some(id),
            contents: pu,
            public: false,
            node_info,
        })
    }

    fn type_reference_inner(
        &mut self
    ) -> Result<Option<ast::TypeReference<'a>>, ParseResultError<'a>> {
        let typename = self.eat_match(Token::Identifier);
        match typename {
            None => Ok(None),
            Some(tn) => {
                let type_param_open = self.eat_match(Token::CmpLessThan);
                let start = tn.start;
                let mut end = tn.end;
                let type_param_list: Vec<ast::TypeReference> = match type_param_open {
                    None => Vec::new(),
                    Some(_arrow) => {
                        let mut params = Vec::new();

                        while let Ok(Some(tr)) = self.type_reference_inner() {
                            params.push(tr);
                            match self.eat_match(Token::Comma) {
                                None => break,
                                Some(_) => continue,
                            }
                        }

                        end = self.expect(Token::CmpGreaterThan)?.end;

                        params
                    },
                };

                let node_info = ast::NodeInfo::from_indices(true, start, end);

                let tr = ast::TypeReference {
                    typename: tn.slice,
                    type_parameters: type_param_list,
                    refers_to: None,
                    node_info
                };

                Ok(Some(tr))
            }
        }
    }

    pub fn type_reference(
        &mut self
    ) -> Result<ast::TypeReference<'a>, ParseResultError<'a>> {
        let index = self.lex.la(0).map(|tw| tw.start).unwrap_or(0);
        match self.type_reference_inner() {
            Err(e) => Err(e),
            Ok(None) => Err(ParseResultError::SemanticIssue("expected a type reference expression, but none was found", index, index)),
            Ok(Some(tr)) => Ok(tr),
        }
    }

    pub fn function_param_list(
        &mut self
    ) -> Result<Vec<(Box<ast::ExpressionWrapper<'a>>, ast::TypeReference<'a>)>, ParseResultError<'a>> {
        let mut rvec: Vec<(Box<ast::ExpressionWrapper<'a>>, ast::TypeReference<'a>)> = Vec::new();
        while let Ok(a) = self.atomic_expression() {
            self.expect(Token::Colon)?;
            let tr = self.type_reference()?;

            let r = (a, tr);

            rvec.push(r);

            match self.eat_match(Token::Comma) {
                Some(_) => continue,
                None => break,
            }
        }

        Ok(rvec)
    }


    pub fn function_declaration(
        &mut self
    ) -> Result<ast::FunctionDeclaration<'a>, ParseResultError<'a>> {
        let start = self.expect(Token::Function)?.start;
        let function_name = self.expect(Token::Identifier)?;
        self.expect(Token::LParen)?;
        let params = self.function_param_list()?;
        self.expect(Token::RParen)?;

        self.expect(Token::ThinArrow)?;
        let return_type = self.type_reference()?;

        let body = self.parse_expr()?;

        let end = body.as_node().start().expect("Some(_) body has None end");

        let node_info = ast::NodeInfo::from_indices(true, start, end);

        Ok(ast::FunctionDeclaration {
            node_info,
            body,
            params,
            return_type,
            name: function_name.slice,
        })
    }

    pub fn variable_declaration(
        &mut self
    ) -> Result<ast::VariableDeclaration<'a>, ParseResultError<'a>> {
        println!("Got a variable declaration, looking...");
        let start = self.lex.la(0).map_or(0, |tw| tw.start);

        let _let = self.expect(Token::Let)?;
        let lhs = self.parse_access_base(None)?;
        //let id = expect(lexer, Token::Identifier)?.slice;
        let maybe_typeref = self.eat_match(Token::Colon);
        println!("Typeref colon: {:?}", maybe_typeref);

        let tr = match maybe_typeref {
            Some(_) => {
                println!(
                    "Got colon, expecting identifier. LookAhead is: {:?}",
                    self.lex.la(0)
                );
                Some(self.type_reference()?)
            }
            None => None,
        };

        let equals = self.eat_match(Token::Equals);
        println!("Equals is: {:?}", equals);
        let var_expr = match equals {
            Some(_eq) => Some(self.parse_expr()?),
            None => None,
        };

        //let var_expr = self.parse_expr()?;

        self.expect(Token::Semicolon)?;

        let end = self.lex.la(-1).map_or(start, |tw| tw.start);

        let node_info = ast::NodeInfo::from_indices(true, start, end);

        Ok(ast::VariableDeclaration {
            node_info,
            lhs,
            var_expr,
            var_type: tr,
        })
    }

    pub fn closure(&mut self) -> Result<ast::Closure<'a>, ParseResultError<'a>> {
        panic!()
    }

    // I really hate myself for this, but I'm going to potentially try both grammar based
    // precedence and pratt parsing since both seem not great
    /*pub fn expression_outer(&mut self) -> () {
        parse_expr(la, 0);

        fn parse_expr<'a>(
            la: &mut LookaheadStream<'a>,
            min_bp: u8,
        ) -> Result<(), ParseResultError<'a>> {
            while let Ok(tw) = la.next() {
                let operators: Vec<Token> = Vec::new();
                let operands: Vec<Box<dyn ast::Expression>> = Vec::new();

                enum State {
                    //
                }
            }

            panic!()
        }
    }*/

}
