mod parse_base;
mod parse_expr;
mod parse_helper;

pub use parse_base::*;
pub use parse_expr::*;
pub use parse_helper::*;

use crate::helper::lex_wrap::TokenWrapper;
use crate::helper::lex_wrap::ParseResultError;
use crate::helper::lex_wrap::LookaheadStream;

use colored::*;

pub struct Parser<'b, 'a> where 'b: 'a {
    lex: &'b mut LookaheadStream<'a>,
    errors: Vec<ParseResultError<'a>>,
}

impl<'b, 'a> Parser<'b, 'a> {
    pub fn new(lex: &'b mut LookaheadStream<'a>) -> Parser<'b, 'a> {
        Parser { lex, errors: Vec::new() }
    }

    pub fn err<T>(&mut self, err: ParseResultError<'a>) -> Result<T, ParseResultError<'a>> {
        self.errors.push(err.clone());

        Err(err)
    }

    pub fn report_err(&mut self, err: ParseResultError<'a>) {
        println!("REPORTED AN ERROR");
        self.errors.push(err);
    }

    pub fn cpe<T>(&mut self, r: Result<T, ParseResultError<'a>>) -> Result<T, ParseResultError<'a>> {
        let r = match r {
            Err(e) => {
                self.errors.push(e.clone());

                Err(e)
            }
            Ok(other) => Ok(other),
        };

        r
    }

    pub fn print_context(&self, start: usize, end: usize, input: &'a str) {
            //let newline_indices = 
        let mut newline_before = 0;
        for (index, character) in input.char_indices() {
            match character {
                '\n' => {
                    if index > start {
                        break;
                    } else {
                        newline_before = index + 1;
                        continue;
                    }
                },
                _ => continue,
            }
        }

        let mut newline_after = input.len() - 1;
        for (index, character) in input.char_indices().rev() {
            match character {
                '\n' => {
                    if index < end {
                        break;
                    } else {
                        newline_after = index;
                        continue;
                    }
                },
                _ => continue,
            }
        }

        //let mut desired_start = start;
        //let desired_start = (start as isize - 10).max(0) as usize;
        //let desired_end = (end as isize + 10) as usize;
        //let context_start = (input.len() - 1).min(desired_start);
        //let context_end = (input.len() - 1).min(desired_end);
        let context_start = newline_before;
        let context_end = newline_after;
        //println!("cs and ce are {} and {}", context_start, context_end);
        let slice = &input[context_start..context_end];

        //println!("{}", "Error context:".bold());
        //println!();
        println!("{}", slice.blue().bold());

        for i in context_start..context_end {
            if i < end && i >= start {
                print!("{}", "^".red());
            } else {
                print!(" ");
            }
        }
        println!();
    }

    pub fn print_errors(&self, input: &'a str) {
        println!();
        println!("{}", "Errors:".red());
        if self.errors.len() == 0 {
            println!("{}", "No errors reported".blue());
        }
        println!();
        for e in self.errors.iter() {
            match e {
                ParseResultError::EndOfFile => {
                    eprintln!("Unexpected End of File");
                },
                ParseResultError::NotYetParsed => {
                    panic!("Programming error, unexplored region of ast has error for us?");
                },
                ParseResultError::UnexpectedToken(t, expected) => {
                    self.print_context(t.start, t.end, input);
                    eprintln!("Got unexpected token of type {:?}. Expected one of {:?}. Token with slice \"{}\" was encountered around ({}, {})",
                        t.token,
                        expected,
                        t.slice,
                        t.start,
                        t.end,
                    );
                },
                ParseResultError::SemanticIssue(issue, start, end) => {
                    self.print_context(*start, *end, input);
                    eprintln!("Encountered a semantic issue: {}. This issue was realized around the character range ({}, {})",
                        issue,
                        start,
                        end,
                    );
                }
            }
            println!();
            println!();
        }
        println!();
    }
}
