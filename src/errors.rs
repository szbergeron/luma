use colored::*;
use itertools::Itertools;

#[derive(Clone, Debug)]
pub enum CompilationError {
    TypeError(TypeUnificationError),
}

#[derive(Clone, Debug)]
pub struct TypeUnificationError {
    pub from: TypeID,
    pub from_peers: Vec<TypeID>,

    pub into: TypeID,
    pub into_peers: Vec<TypeID>,

    pub for_expression: NodeInfo,

    pub context: Vec<String>,

    pub reason_for_unification: IStr,
}

impl CompilationError {
    pub fn with_file_context(self, files: &FileRegistry) {
        let ep = ErrorPrinter {};

        match self {
            CompilationError::TypeError(te) => {
                let TypeUnificationError {
                    from,
                    from_peers,
                    into,
                    into_peers,
                    for_expression,
                    context,
                    reason_for_unification,
                } = te;

                let from_printed = ep.contextualize(
                    from.span(),
                    files,
                    "Tried to take a value that was produced here".intern(),
                );

                let into_printed = ep.contextualize(
                    into.span(),
                    files,
                    "And tried to put it into a value that came from this constraint:".intern(),
                );

                ep.note_line(format!("The reason the unification was attempted is: {reason_for_unification}"));
            }
        }
    }
}

pub struct ErrorPrinter {}

use crate::{
    compile::file_tree::FileRegistry,
    cst::NodeInfo,
    helper::interner::{IStr, Internable, SpurHelper},
    lex::CodeLocation,
    mir::quark::TypeID,
};

impl ErrorPrinter {
    pub fn note_line(&self, line: String) {
        println!("{}", "This additional information was given for solving the issue:".yellow().bold());
        println!("   {} {}", ">".bright_blue().bold(), line.bright_yellow().bold());
    }
    pub fn contextualize(&self, near: NodeInfo, files: &FileRegistry, if_solve: IStr) -> bool {
        if let NodeInfo::Parsed(p) = near && let Some(f) = p.span.start.file_id() {
            let handle = files.open_id(f).expect("couldn't open a file");

            let contents = &handle.0;
            let file = &handle.1;

            let lines = contents.as_str().unwrap().lines().collect_vec();

            println!("{}: {}:", "error".bold().bright_red(), if_solve.resolve().bold().bright_yellow());

            self.print_context(p.span.start, p.span.end, &lines, file.to_str().expect("weird"));

            true
        } else {
            false
        }
    }

    pub fn print_context(&self, start: CodeLocation, end: CodeLocation, lines: &Vec<&str>, filename: &str) {
        // print context lines before

        match (start, end) {
            (CodeLocation::Parsed(start), CodeLocation::Parsed(end)) => {
                let start_line = (start.line - 2).max(1);
                let end_line = (end.line + 2).min(lines.len() as isize);
                //println!("start line: {}, end line: {}", start_line, end_line);

                let start_line = start.line;
                let start_char = start.offset;

                let filename = filename.bold();
                println!("  {} {filename}  starting at {start_line}:{start_char}", "-->".blue().bold());

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
                    let line = line.bold();
                    let line = format!("{line}");
                    let line = line.as_str();
                    //let line = 
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
                        let start = if line_num > start.line {
                            0
                        } else {
                            start.offset as usize
                        };
                        let end = if line_num < end.line {
                            line.len()
                        } else {
                            end.offset as usize
                        };
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

    pub fn print_fmt_line(
        &self,
        line_num: isize,
        pad: usize,
        line: &str,
        highlight: Option<(usize, usize)>,
    ) {
        // do indentation
        println!();
        print!(" {line:<pad$} | ", line = line_num, pad = pad);
        print!("{}", line.bold());
        println!();

        print!(" {line:pad$} | ", line = "", pad = pad);
        //print!("{}", line.blue().bold());
        if let Some((start, end)) = highlight {
            for i in 0..line.len() {
                if i >= start && i < end {
                    print!("{}", "^".bright_red());
                } else {
                    //print!("{}", "―".red());
                    print!(" ");
                }
            }
        }
    }

    //pub fn print_context<'map>(&self, start: usize, end: usize, map: &'map rangemap::RangeMap<usize, (usize, &'a str, usize)>) {

    pub fn print_bar(&self) {
        println!();
        if let Some((w, _)) = term_size::dimensions() {
            for _ in 0..w {
                print!("{}", "―".cyan());
            }
        }
        println!();
    }
}
