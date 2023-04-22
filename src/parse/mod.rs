mod parse_base;
mod parse_expr;
mod parse_helper;
mod parse_spec;
mod parse_tools;
mod parse_type;

use std::collections::HashSet;

pub use parse_base::*;
pub use parse_expr::*;
pub use parse_helper::*;
pub use parse_spec::*;
pub use parse_tools::*;
pub use parse_type::*;

use crate::errors::{add_error, ErrorPrinter};
//use crate::helper::lex_wrap::LookaheadStream;
//use crate::helper::lex_wrap::{CodeLocation, ParseResultError};
use crate::helper::interner::*;

use crate::lex::{CodeLocation, LookaheadHandle, ParseResultError};

use crate::lex::Token;

use colored::*;

use crate::compile::file_tree::*;

//use std::io::{self, Write};

pub struct Parser<'lexer> {
    lex: LookaheadHandle<'lexer>,
    errors: Vec<ParseResultError>,
    next: Vec<Token>,
    scope: Vec<IStr>,
}

pub struct SyncSliceHandle {
    pub start: usize,
    //end: usize,
}

impl<'lexer> Parser<'lexer> {
    pub fn new(lex: LookaheadHandle<'lexer>, scope: Vec<IStr>) -> Parser<'lexer> {
        Parser {
            lex,
            scope,
            errors: Vec::new(),
            next: Vec::new(),
        }
    }

    pub fn err<T>(&mut self, err: ParseResultError) -> Result<T, ParseResultError> {
        self.errors.push(err.clone());

        Err(err)
    }

    pub fn report_err(&mut self, err: ParseResultError) {
        //println!("REPORTED AN ERROR");
        self.errors.push(err);
    }

    pub fn cpe<T>(&mut self, r: Result<T, ParseResultError>) -> Result<T, ParseResultError> {
        let r = match r {
            Err(e) => {
                self.errors.push(e.clone());

                Err(e)
            }
            Ok(other) => Ok(other),
        };

        r
    }

    /*pub fn build_line_map(&self, input: &'a str) -> rangemap::RangeMap<usize, (usize, &'a str, usize)> {
        let mut index = 0;

        let mut map = rangemap::RangeMap::new();

        for line in input.lines() {
            let start = index;
            index += line.len() + 1;
            let end = index;

            map.insert(start..end, (start, line, end));
        }

        map
    }*/

    fn print_err(&self, e: &ParseResultError, lines: &Vec<&str>, filename: &str) {
        let ep = ErrorPrinter {};
        match e {
            ParseResultError::InternalParseIssue => {}
            ParseResultError::EndOfFile => {
                eprintln!("Unexpected End of File");
            }
            ParseResultError::NotYetParsed => {
                panic!("Programming error, unexplored region of ast has error for us?");
            }
            ParseResultError::UnexpectedToken(t, expected, msg) => {
                ep.print_context(t.start, t.end, &lines, filename);
                eprintln!("Got unexpected token of type {:?}. Expected one of {:?}. Token with slice \"{}\" was encountered around ({}, {})",
                        t.token,
                        expected,
                        t.slice.resolve(),
                        t.start,
                        t.end,
                    );
                match msg {
                    Some(msg) => eprintln!("Hint: {}", msg),
                    None => {}
                };
            }
            ParseResultError::SemanticIssue(issue, start, end) => {
                //self.print_context(*start, *end, &lines);
                ep.print_context(*start, *end, &lines, filename);
                eprintln!("Encountered a semantic issue: {}. This issue was realized around the character range ({}, {})",
                        issue,
                        start,
                        end,
                    );
            }
            ParseResultError::ErrorWithHint { hint, original } => {
                for e in original.iter() {
                    self.print_err(e, lines, filename)
                }
                eprintln!("Hint: {}", hint);
            }
        }
    }

    pub fn print_errors(&self, file_handle: FileHandle) {
        let ep = ErrorPrinter {};
        //let linemap = self.build_line_map(input);
        //let input: std::sync::Arc<String> = handle.get().unwrap();
        //let input = handle.slice().unwrap();
        let handle = file_handle.contents().unwrap();

        let contents = &handle.0;
        let path = &handle.1;

        let input = contents.as_str().unwrap();
        let lines_iter = input.lines();
        //let v: Vec<&str> = lines.collect();
        let lines: Vec<&str> = lines_iter.collect();
        //let _read_borrow = path_handle.read().unwrap();
        let stdout = std::io::stdout();
        //let handle = stdout.lock();

        /*
        println!();
        println!("File: {}", file_handle.path().to_string_lossy());
        println!("{}", "Errors:".red());
        if self.errors.len() == 0 {
            println!("{}", "No errors reported".blue());
        }

        ep.print_bar();

        println!();
        */
        let errors: HashSet<ParseResultError> = self.errors.clone().into_iter().collect();

        let mut errors: Vec<ParseResultError> = errors.into_iter().collect();

        //errors.sort_by_key(|e| e.)

        errors.sort_by_key(|e| {
            e.start()
                .map(|cl| match cl {
                    CodeLocation::Builtin => (0, 0),
                    CodeLocation::Parsed(v) => (v.line, v.offset),
                })
                .unwrap_or((0, 0))
        });

        for e in errors.iter() {
            //println!();

            add_error(e.clone().wrapped(handle.clone()))

            //self.print_err(e, &lines, path.to_str().expect("filename shenaniganery"));
            //println!();

            //ep.print_bar();

            //println!();
        }
        //println!();

        //std::mem::drop(handle);
    }
}
