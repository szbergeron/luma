mod parse_base;
mod parse_expr;
mod parse_helper;

pub use parse_base::*;
pub use parse_expr::*;
pub use parse_helper::*;

use crate::helper::lex_wrap::LookaheadStream;
use crate::helper::lex_wrap::{CodeLocation, ParseResultError};
use crate::helper::*;

use colored::*;

pub struct Parser<'b, 'a>
where
    'b: 'a,
{
    lex: &'b mut LookaheadStream<'a>,
    errors: Vec<ParseResultError<'a>>,
}

impl<'b, 'a> Parser<'b, 'a> {
    pub fn new(lex: &'b mut LookaheadStream<'a>) -> Parser<'b, 'a> {
        Parser {
            lex,
            errors: Vec::new(),
        }
    }

    pub fn err<T>(&mut self, err: ParseResultError<'a>) -> Result<T, ParseResultError<'a>> {
        self.errors.push(err.clone());

        Err(err)
    }

    pub fn report_err(&mut self, err: ParseResultError<'a>) {
        //println!("REPORTED AN ERROR");
        self.errors.push(err);
    }

    pub fn cpe<T>(
        &mut self,
        r: Result<T, ParseResultError<'a>>,
    ) -> Result<T, ParseResultError<'a>> {
        let r = match r {
            Err(e) => {
                self.errors.push(e.clone());

                Err(e)
            }
            Ok(other) => Ok(other),
        };

        r
    }

    pub fn print_fmt_line(&self, line_num: isize, pad: usize, line: &str, highlight: Option<(usize, usize)>) {
        // do indentation
        println!();
        print!(" {line:<pad$} | ", line = line_num, pad = pad);
        print!("{}", line.blue().bold());
        println!();

        print!(" {line:pad$} | ", line = "", pad = pad);
        //print!("{}", line.blue().bold());
        if let Some((start, end)) = highlight {
            for i in 0..line.len() {
                if i >= start && i < end {
                    print!("{}", "^".red());
                } else {
                    //print!("{}", "―".red());
                    print!(" ");
                }
            }
        }
    }

    //pub fn print_context<'map>(&self, start: usize, end: usize, map: &'map rangemap::RangeMap<usize, (usize, &'a str, usize)>) {
    pub fn print_context(&self, start: CodeLocation, end: CodeLocation, lines: &Vec<&'a str>) {
        
        // print context lines before

        match (start, end) {
            (CodeLocation::Parsed(start), CodeLocation::Parsed(end)) => {
                let start_line = (start.line - 2).max(1);
                let end_line = (end.line + 2).min(lines.len() as isize - 1);

                let mut pad = 0;
                for line_num in start_line..(end_line + 1) {
                    //let s: String = line_num.into();
                    //let s = String::from(line_num as i64);
                    let s = line_num.to_string();
                    pad = pad.max(s.len());
                }

                pad = pad + 4;

                for line_num in start_line..(end_line + 1) {
                    let line = lines.get(line_num as usize - 1).unwrap_or(&"");
                    /*println!("|{}", line.blue().bold());

                    //print!("|{
                    for i in 0..line.len() {
                        if (i >= start.offset as usize || line_num > start.line)
                            && (i < end.offset as usize || line_num < end.line)
                        {
                            print!("{}", "^".red());
                        } else {
                            print!("{}", "―".red());
                        }
                    }*/
                    let hl = if line_num >= start.line && line_num <= end.line {
                        let start = if line_num > start.line { 0 } else { start.offset as usize };
                        let end = if line_num < end.line { line.len() } else { end.offset as usize };
                        Some((start, end))
                        //Some((0, 0))
                    } else {
                        None
                    };

                    self.print_fmt_line(line_num, pad, line, hl);
                }
                println!();
            }
            _ => {}
        }
    }

    pub fn print_bar(&self) {
        println!();
        if let Some((w, _)) = term_size::dimensions() {
            for _ in 0..w {
                print!("{}", "―".cyan());
            }
        }
        println!();
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

    pub fn print_errors(&self, input: &'a str, path_handle: PathIdMapHandle) {
        //let linemap = self.build_line_map(input);
        let lines: Vec<&'a str> = input.lines().collect();
        let read_borrow = path_handle.read().unwrap();

        println!();
        println!("{}", "Errors:".red());
        if self.errors.len() == 0 {
            println!("{}", "No errors reported".blue());
        }
        self.print_bar();
        println!();
        for e in self.errors.iter() {
            println!();
            match e {
                ParseResultError::EndOfFile => {
                    eprintln!("Unexpected End of File");
                }
                ParseResultError::NotYetParsed => {
                    panic!("Programming error, unexplored region of ast has error for us?");
                }
                ParseResultError::UnexpectedToken(t, expected) => {
                    self.print_context(t.start, t.end, &lines);
                    eprintln!("Got unexpected token of type {:?}. Expected one of {:?}. Token with slice \"{}\" was encountered around ({}, {})",
                        t.token,
                        expected,
                        t.slice,
                        t.start,
                        t.end,
                    );
                }
                ParseResultError::SemanticIssue(issue, start, end) => {
                    self.print_context(*start, *end, &lines);
                    eprintln!("Encountered a semantic issue: {}. This issue was realized around the character range ({}, {})",
                        issue,
                        start,
                        end,
                    );
                }
            }
            println!();

            self.print_bar();

            println!();
        }
        println!();
    }
}
