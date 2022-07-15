use crate::compile::parse_tree::ParseTreeNode;
//use crate::ast;
use crate::helper::*;
use crate::lex::{LookaheadHandle, ParseResultError, TokenStream, ErrorSet};
use crate::parse::schema::TokenProvider;
use crate::parse::Parser;
use std::collections::HashSet;

use std::path::{Path, PathBuf};
use std::process::{self, abort};
use crate::{cst, ast};

//use crate::mid_repr::ScopeContext;


//use crate::ast::*;



use crate::helper::interner::*;
use tokio::runtime::*;

//use super::file_tree::FileHandle;

use super::file_tree::{FileHandle, FileRegistry, FileRole, SourceFile, SpecFile};
use super::preparse_tree::PreParseTreeNode;


#[allow(unused_variables, dead_code)]
pub fn parse_source_file<'file>(
    handle: FileHandle,
    scope: Vec<IStr>,
    cflags: &CFlags,
) -> (Option<cst::OuterScope>, ErrorSet) {
    // clone because we don't want to keep lock open, and this should be rather cheap in the scheme
    // of things
    // TODO: eval if this even matters

    let contents = handle.contents().unwrap();

    let content_str = contents.as_str().unwrap();

    //let base_path = handle.path();
    let file_id = handle.id();
    let lex = TokenStream::new(content_str, file_id);
    let tv = lex.to_vec();
    let scanner = LookaheadHandle::new(&tv);

    let mut parser = Parser::new(scanner.clone(), scope);

    let t: TokenProvider = TokenProvider::from_handle(scanner);

    #[allow(irrefutable_let_patterns)]
    let p = if let iguard = interner() {
        let p = parser.entry(&t, true);
        p
    } else {
        panic!("irrefutable pattern")
    };

    let (v, es, s) = p.open_anyway();

    println!("v: {v:?}, es: {es:?}");

    for e in es.clone() {
        println!("Error: {:?}", e);
        let _ = parser.err::<()>(e);
    }

    //e.map(|e| parser.err::<()>(e));

    if !cflags.eflags.silence_errors {
        println!("Printing errors");
        parser.print_errors(handle);
    } else {
        println!("Not printing errors");
    }


    match &v {
        Some(punit) => {
            if cflags.dump_tree {
                println!("Input file:");
                for (line_num, line) in content_str.lines().enumerate() {
                    println!("{line_num:3}｜ {line}");
                }
                println!();

                println!("Gets AST of: {:#?}", punit);
            }
            if cflags.dump_pretty {
                let s = String::new();
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
    println!("Returns parse issue");

    //v.ok_or(ParseResultError::InternalParseIssue)
    //v.ok_or(ParseResultError::ErrorWithHint { hint: "file", original: es.into() })
    (v, es)
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

fn args_to_roles(args: &ArgResult) -> Vec<FileRole> {
    let mut roles = Vec::new();

    args.source_input.iter().map(|p| {
        let f = FileRole::Source(SourceFile { location: p.clone() });
        f
    }).for_each(|e| roles.push(e));

    args.spec_input.iter().map(|p| {
        let f = FileRole::Spec(SpecFile { location: p.clone() });
        f
    }).for_each(|e| roles.push(e));

    roles
}

async fn async_launch(args: ArgResult) {
    //let (error_sender, _error_reciever) = crossbeam::unbounded();

    let files: FileRegistry = Default::default();

    let roles = args_to_roles(&args);

    //let root = PreParseTreeNode::new(&files);

    println!("Building node");
    
    let node = crate::compile::preparse_tree::from_roots(&files, roles);

    println!("Built node");

    let node = ParseTreeNode::from_preparse(node, vec![], &args.flags).await;

    println!("{node:#?}");

    let node = ast::tree::Node::from_parse(node, "global".intern(), None, None);

    println!("{:#?}", node.to_ref());

    // now need to parse tree into...more

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

    // get root from our spec files

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
            "-s" | "--spec" => state = State::ExpectSpecInput,
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

pub fn parse<'a>(n: PreParseTreeNode<'a>) {
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
