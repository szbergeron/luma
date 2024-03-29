use std::{collections::HashSet, path::PathBuf, sync::Arc};

use colored::*;
use itertools::Itertools;
use once_cell::sync::OnceCell;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CompilationError {
    TypeError(TypeUnificationError),
    ArgConversionError(ArgConversionError),
    FieldAccessError(FieldAccessError),
    UnrestrictedTypeError(UnrestrictedTypeError),
    FieldResolutionError(FieldResolutionError),
    ParseError(ParseResultError, Invisible<Arc<(Contents, PathBuf)>>),
    UnresolvedSymbol(UnresolvedSymbolError),
    UnknownVariable(UnknownVariableError),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct UnresolvedSymbolError {
    pub symbol: ScopedName,
    pub location: NodeInfo,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct UnknownVariableError {
    pub variable: IStr,
    pub location: NodeInfo,
}

#[derive(Clone)]
pub struct Invisible<T>
where
    T: Clone,
{
    pub v: T,
}

impl<T> std::cmp::PartialEq for Invisible<T>
where
    T: Clone,
{
    fn eq(&self, other: &Self) -> bool {
        true
    }
}

impl<T> std::cmp::Eq for Invisible<T> where T: Clone {}

impl<T> std::fmt::Debug for Invisible<T>
where
    T: Clone,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<invisible")
    }
}

impl<T> std::hash::Hash for Invisible<T>
where
    T: Clone,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // nothing
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FieldResolutionError {
    pub name: IStr,
    pub base: CtxID,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct UnrestrictedTypeError {
    pub note: IStr,
    pub tid: TypeID,
    pub peers: Vec<TypeID>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeUnificationError {
    pub from: TypeID,
    pub from_peers: Vec<TypeID>,

    pub into: TypeID,
    pub into_peers: Vec<TypeID>,

    pub for_expression: NodeInfo,

    pub context: Vec<String>,

    pub reason_for_unification: IStr,

    pub reason_for_failure: IStr,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FieldAccessError {
    //pub base_expression:
    pub base_expr_span: NodeInfo,
    pub field_span: NodeInfo,
    pub error_info: String,
}

pub fn add_error(e: CompilationError) {
    let _ = ERRORS.v.0.send(e);
}

pub fn take_error() -> Option<CompilationError> {
    ERRORS.v.1.try_recv().ok()
}

struct Wrap<T> {
    pub v: T,
}

lazy_static::lazy_static! {
    static ref ERRORS: Wrap<(crossbeam::Sender<CompilationError>, crossbeam::Receiver<CompilationError>)> = {
        Wrap { v: crossbeam::unbounded() }
    };
}

impl CompilationError {
    fn print_parse_err(&self, e: &ParseResultError, lines: &Vec<&str>, filename: &str) {
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
                    self.print_parse_err(e, lines, filename)
                }
                eprintln!("Hint: {}", hint);
            }
        }
    }
    pub fn with_file_context(self, files: &FileRegistry) {
        let ep = ErrorPrinter {};

        match self.clone() {
            CompilationError::UnknownVariable(uv) => {
                let UnknownVariableError { variable, location } = uv;

                ep.new_error("Unknown Variable");

                ep.line(format!("Failed to find variable {variable} within the current scope"));

                ep.contextualize(location, files, "variable was referenced around here vvv".intern());
            }
            CompilationError::UnresolvedSymbol(us) => {
                ep.new_error("Unresolved Symbol");

                let s = us.symbol;
                let s = s.scope.into_iter().join("::");
                ep.line(format!("Failed to import symbol {s}"));

                ep.contextualize(us.location, files, "symbol was referenced around here vvv".intern());
            }
            CompilationError::ParseError(pe, handle) => {
                ep.new_error("Parse Error");
                let h = handle.v.clone();

                let contents = &h.0;
                let path = &h.1;

                let input = contents.as_str().unwrap();
                let lines_iter = input.lines();
                //let v: Vec<&str> = lines.collect();
                let lines: Vec<&str> = lines_iter.collect();

                self.print_parse_err(&pe, &lines, path.to_str().expect("filename shenaniganery"))
            }
            CompilationError::FieldResolutionError(fre) => {
                let FieldResolutionError { name, base } = fre;
                let base = base.resolve().canonical_typeref().resolve().unwrap();

                ep.new_error("Dynamic Field Type Resolution Error");

                ep.line(format!("Could not resolve the type of field {name} on base {base:?}"));
            }
            CompilationError::TypeError(te) => {
                ep.new_error("Type Unification Error");
                let TypeUnificationError {
                    from,
                    from_peers,
                    into,
                    into_peers,
                    for_expression,
                    context,
                    reason_for_unification,
                    reason_for_failure,
                } = te;

                let real_from = if from.is_root() {
                    from
                } else {
                    if let Some(t) = from_peers.iter().find(|t| t.is_root()) {
                        *t
                    } else {
                        from // it isn't a root, but it'll have to do
                    }
                };

                let real_into = if into.is_root() {
                    into
                } else {
                    if let Some(t) = into_peers.iter().find(|t| t.is_root()) {
                        *t
                    } else {
                        into // it isn't a root, but it'll have to do
                    }
                };

                match (real_from.span(), real_into.span()) {
                    (NodeInfo::Builtin, NodeInfo::Builtin) => {
                        ep.new_error("compiler error: failed unifying builtins?");
                    }
                    (NodeInfo::Builtin, p @ NodeInfo::Parsed(_)) => {
                        ep.contextualize(
                            p,
                            files,
                            "Tried to put a literal of the wrong type into this value:".intern(),
                        );
                    }
                    (p @ NodeInfo::Parsed(_), NodeInfo::Builtin) => {
                        ep.contextualize(p, files, "Tried to take this value and unify it with a literal of the wrong type:".intern());
                    }
                    (p1 @ NodeInfo::Parsed(_), p2 @ NodeInfo::Parsed(_)) => {
                        //ep.new_error("Type Unification Failed");

                        ep.contextualize(
                            p1,
                            files,
                            "Tried to take a value that came from here:".intern(),
                        );
                        ep.contextualize(
                            p2,
                            files,
                            "And put it into a value that was resolved near here:".intern(),
                        );
                    }
                }

                let mut already_printed = HashSet::new();

                already_printed.insert(from.span());
                already_printed.insert(into.span());

                for t in from_peers.clone() {
                    if already_printed.insert(t.span()) {
                        if let p @ NodeInfo::Parsed(_) = t.span() {
                            ep.contextualize(
                                p,
                                files,
                                "this was evaluated when forming the 'from' chain".intern(),
                            );
                        }
                    }
                }

                for t in into_peers.clone() {
                    if already_printed.insert(t.span()) {
                        if let p @ NodeInfo::Parsed(_) = t.span() {
                            ep.contextualize(
                                p,
                                files,
                                "this was evaluated when forming the 'into' chain".intern(),
                            );
                        }
                    }
                }

                let mut all = HashSet::new();

                for peer in into_peers {
                    all.insert(peer);
                }

                for peer in from_peers {
                    all.insert(peer);
                }

                all.insert(into);
                all.insert(from);

                for t in all {
                    if already_printed.insert(t.span()) {
                        ep.contextualize(
                            t.span(),
                            files,
                            "this was part of the 'all' chain".intern(),
                        );
                    }

                    let why = t.why();
                    ep.note_line(format!("A tid here had the creation reason: {why}"));
                }

                ep.note_line(format!(
                    "The reason the unification was attempted is: {reason_for_unification}"
                ));
                ep.note_line(format!(
                    "The reason the unification failed was: {reason_for_failure}"
                ));
            }
            CompilationError::ArgConversionError(ace) => {
                ep.new_error("Argument Conversion Error");
                let ArgConversionError {
                    argument,
                    parameter,
                    comment,
                    for_function,
                } = ace;

                ep.quick_function(for_function, files);

                if let Some(v) = argument {
                    ep.contextualize(v.span(), files, "took this argument".intern());
                } else {
                    ep.line("did not take any argument".to_owned());
                }

                if let Some((v, n)) = parameter {
                    ep.contextualize(
                        v.span(),
                        files,
                        "and tried to apply it to this parameter".intern(),
                    );
                } else {
                    ep.line("but did not accept any parameter at that position".to_owned());
                }

                ep.line(format!("That was the cause of this error: {comment}"));
            }
            CompilationError::FieldAccessError(fae) => {
                ep.new_error("Field Access Error");

                let FieldAccessError {
                    base_expr_span,
                    field_span,
                    error_info,
                } = fae;

                ep.contextualize(
                    field_span,
                    files,
                    "This field did not exist on the base expression type:".intern(),
                );

                ep.note_line(error_info);
            }
            CompilationError::UnrestrictedTypeError(ute) => {
                let UnrestrictedTypeError { tid, peers, note } = ute;

                ep.new_error("Unconstrained Type");

                let mut seen = HashSet::new();

                ep.contextualize(
                    tid.span(),
                    files,
                    "This type could not be resolved".intern(),
                );

                seen.insert(tid.span());

                for peer in peers {
                    if seen.insert(peer.span()) {
                        ep.contextualize(
                            peer.span(),
                            files,
                            "This was found in the 'same-as' chain".intern(),
                        );
                    }

                    let why = peer.why();
                    ep.note_line(format!("A tid here had the creation reason: {why}"));
                }

                ep.note_line(
                    "This was potentially caused by a dynamic field not resolving,
                                     so the related instances are of unknown type"
                        .to_owned(),
                );

                ep.note_line(note.to_string());
            }
        }
    }
}

pub struct ErrorPrinter {}

use crate::{
    ast::tree::{CtxID, NodeUnion},
    compile::file_tree::{Contents, FileRegistry},
    cst::{NodeInfo, ScopedName},
    helper::interner::{IStr, Internable, SpurHelper},
    lex::{CodeLocation, ParseResultError},
    mir::{instance::ArgConversionError, quark::TypeID},
};

impl ErrorPrinter {
    pub fn new_error(&self, description: &str) {
        println!();
        println!();
        println!(
            "{}: {}",
            "error".bold().bright_red(),
            description.bold().bright_red()
        );
    }
    pub fn quick_function(&self, ctid: CtxID, files: &FileRegistry) {
        match &ctid.resolve().inner {
            NodeUnion::Function(fd, _) => {
                self.contextualize(fd.header, files, "this function vvvv".intern());
            }
            _ => unreachable!(),
        }
    }

    pub fn note_line(&self, line: String) {
        println!(
            "{}",
            "This additional information was given for solving the issue:".bright_yellow()
        );
        println!("   {} {}", ">".bright_blue().bold(), line.blue().bold());
    }

    pub fn line(&self, line: String) {
        println!("   {} {}", ">".bright_blue().bold(), line.yellow());
    }

    pub fn contextualize(&self, near: NodeInfo, files: &FileRegistry, if_solve: IStr) -> bool {
        if let NodeInfo::Parsed(p) = near && let Some(f) = p.span.start.file_id() {
            let handle = files.open_id(f).expect("couldn't open a file");

            let contents = &handle.0;
            let file = &handle.1;

            let lines = contents.as_str().unwrap().lines().collect_vec();

            println!(" {}:", if_solve.resolve().yellow());

            self.print_context(p.span.start, p.span.end, &lines, file.to_str().expect("weird"));

            true
        } else {
            false
        }
    }

    pub fn print_context(
        &self,
        start: CodeLocation,
        end: CodeLocation,
        lines: &Vec<&str>,
        filename: &str,
    ) {
        // print context lines before

        match (start, end) {
            (CodeLocation::Parsed(start), CodeLocation::Parsed(end)) => {
                //println!("start: {start:?}, end: {end:?}");
                let start_line = (start.line - 1).max(0);
                let end_line = (end.line + 1).min(lines.len() as isize);
                //println!("start line: {}, end line: {}", start_line, end_line);

                //let start_line = start.line;
                let start_char = start.offset;

                let filename = filename.bold();
                println!(
                    "  {} {filename}  starting at {start_line}:{start_char}",
                    "-->".blue().bold()
                );

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
                    //println!("line_num: {line_num}, line: {line}");
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
