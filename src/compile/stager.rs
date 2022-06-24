//use crate::ast;
use crate::helper::*;
use crate::lex::{LookaheadHandle, ParseResultError, TokenStream};
use crate::parse::schema::TokenProvider;
use crate::parse::Parser;
use std::collections::HashSet;

use std::path::{Path, PathBuf};
use std::process::{self, abort};
use crate::cst;

//use crate::mid_repr::ScopeContext;
use std::sync::Arc;

//use crate::ast::*;

use crate::encode::*;

use crate::helper::interner::*;
use tokio::runtime::*;

#[allow(unused_variables, dead_code)]
pub fn parse_source_file<'file>(
    handle: FileHandleRef,
    scope: Vec<IStr>,
    cflags: &CFlags,
) -> Result<cst::OuterScope, ParseResultError> {
    // clone because we don't want to keep lock open, and this should be rather cheap in the scheme
    // of things
    // TODO: eval if this even matters

    let contents = handle.contents;

    let base_path = handle.id;
    let lex = TokenStream::new(contents, base_path);
    let tv = lex.to_vec();
    let scanner = LookaheadHandle::new(&tv);

    let mut parser = Parser::new(scanner.clone(), scope);

    let mut t: TokenProvider = TokenProvider::from_handle(scanner);

    #[allow(irrefutable_let_patterns)]
    let p = if let iguard = interner() {
        let p = parser.entry(&t);
        p
    } else {
        panic!("irrefutable pattern")
    };

    let (v, es, s) = p.open_anyway();

    for e in es {
        println!("Error: {:?}", e);
        let _ = parser.err::<()>(e);
    }

    //e.map(|e| parser.err::<()>(e));

    if !cflags.eflags.silence_errors {
        parser.print_errors(handle);
    }

    match &v {
        Some(punit) => {
            if cflags.dump_tree {
                println!("Input file:");
                for (line_num, line) in contents.lines().enumerate() {
                    println!("{line_num:3}｜ {line}");
                }
                println!();

                println!("Gets AST of: {:#?}", punit);
            }
            if cflags.dump_pretty {
                let mut s = String::new();
                //punit.pretty(&mut s, 0);
                println!("Pretty output:");
                println!("{}", s);
            }
        }
        None => println!("No parse unit was created"),
        /*Err(err) => {
            println!("Failed to parse with error: {:?}", err);
        }*/
    }

    v.ok_or(ParseResultError::InternalParseIssue)
}

#[derive(Default, Copy, Clone)]
pub struct EFlags {
    pub warnings_as_errors: bool,
    pub silence_warnings_below_level: i32,
    pub silence_errors: bool,
}

#[derive(Default, Copy, Clone)]
pub struct CFlags {
    pub arg_parsing_failed: bool,
    pub dump_tree: bool,
    pub dump_pretty: bool,
    pub eflags: EFlags,
    pub thread_count: usize,
}

async fn async_launch(args: ArgResult) {
    //let (error_sender, _error_reciever) = crossbeam::unbounded();

    //let root = super::tree::CompilationRoot::initial(error_sender, args).await;
    //TODO
    abort();
    //let _root_ctx = root.into_ctx().await;
}

pub fn launch(args: &[&str]) {
    // do initial setup actions
    unsafe {
        crate::helper::interner::init_interner();
    }

    let args = parse_args(args).expect("couldn't parse arguments");

    let thread_count = args.flags.thread_count;

    let tokio_rt = Builder::new_multi_thread()
        .worker_threads(thread_count)
        .build()
        .expect("Couldn't initialize an async worker pool, bad args?");

    tokio_rt.block_on(async { async_launch(args).await });
}

#[allow(dead_code)]
pub struct ArgResult {
    pub flags: CFlags,
    pub source_input: HashSet<PathBuf>,
    pub spec_input: HashSet<PathBuf>,
    pub outputs: HashSet<PathBuf>,
}

fn parse_args(args: &[&str]) -> Result<ArgResult, &'static str> {
    enum State {
        ExpectNothing,
        ExpectSourceInput,
        ExpectSpecInput,
        ExpectOutput,
        ExpectThreadCount,
        _ExpectWarnFlags,
        _ExpectErrorFlags,
    }

    let mut state = State::ExpectSourceInput;

    let mut cflags = CFlags {
        thread_count: 1,
        ..CFlags::default() //..Default::default()
    };

    let mut source_files = HashSet::<PathBuf>::new();
    let mut spec_files = HashSet::<PathBuf>::new();
    let mut outputs = HashSet::<PathBuf>::new(); // need to figure out what to do for this one

    for s in args.iter() {
        let slice = &s[..];
        println!("got arg '{}'", slice);
        match slice {
            "-o" | "--output" => state = State::ExpectOutput,
            "-i" | "--input" => state = State::ExpectSourceInput,
            "-r" | "--root" => state = State::ExpectSourceInput,
            "-Cthreads" => state = State::ExpectThreadCount,
            "-Dtree" => cflags.dump_tree = true,
            "-Dpretty" => cflags.dump_pretty = true,
            "-Esilent" => cflags.eflags.silence_errors = true,
            //"-Olib" => cflags.output_library = true,
            //"-Obin" => cflags.output_binary = true,
            other => {
                match state {
                    State::ExpectNothing => {
                        println!("Unexpected arg {other} was provided");
                    },
                    State::ExpectOutput | State::ExpectSourceInput | State::ExpectSpecInput => {
                        // should be a file or directory in this case
                        let path = Path::new(other);
                        let canonicalized = path.canonicalize().unwrap_or_else(|_| {
                            println!(
                                "Invalid compiler argument: got '{}' but was expecting a path",
                                other
                            );
                            process::exit(-1)
                        });
                        let _inserted = match state {
                            State::ExpectSourceInput => source_files.insert(canonicalized),
                            State::ExpectSpecInput => spec_files.insert(canonicalized),
                            State::ExpectOutput => outputs.insert(canonicalized),
                            _ => panic!("state was expecting a path then it wasn't"),
                        }; // maybe check for duplicates here?
                    }
                    State::ExpectThreadCount => {
                        let count: usize = other.parse().unwrap_or_else(|_| {
                            println!("Expected a thread count, instead got '{}'", other);
                            process::exit(-1)
                        });

                        cflags.thread_count = count;
                    }
                    _ => todo!("unimplemented arg state handler"),
                }

                state = State::ExpectNothing;
            }
        }
    }

    for p in source_files.iter() {
        println!("input file with path '{:#?}'", p);
    }

    Ok(ArgResult {
        source_input: source_files,
        spec_input: spec_files,
        outputs,
        flags: cflags,
    })
}
/*
pub fn prepass<'a>(p: &Arc<ScopeContext>) {
    println!("Prepass called on SC: {:#?}", &*p);
}

pub fn analyze<'a>(_p: &Arc<ScopeContext>) {}

pub fn tollvm<'a>(p: &Arc<ScopeContext>, egctx: &EncodeGlobalContext) {
    let mut lctx = EncodeLocalContext::new(egctx);

    lctx.writeln(format!(
        "; start encode for ctx of {:#?}",
        p.scope
            .iter()
            .map(|spur| spur.resolve())
            .collect::<Vec<&str>>()
    ));
    lctx.flush();
}
*/
