use crate::ast;
use crate::helper::lex_wrap::{LookaheadStream, ParseResultError, Wrapper};
use crate::helper::*;
use crate::parse::Parser;
use crossbeam::unbounded;
use rayon::prelude::*;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;
use std::thread;

pub fn parse_unit(
    file_scope: Vec<String>,
    base_path: PathId,
    path_handle: PathIdMapHandle,
    cflags: &CFlags,
) {
    let path = path_handle
        .read()
        .unwrap()
        .get_path(base_path)
        .expect("path ID not registered")
        .clone();
    println!(
        "parsing {:?} which has a file scope of {:?}",
        path, file_scope
    );
    // clone because we don't want to keep lock open, and this should be rather cheap in the scheme
    // of things
    // TODO: eval if this even matters

    let contents = &fs::read_to_string(path).expect("could not read contents of target file")[..];
    let mut lex = Wrapper::new(contents, base_path);
    let mut scanner = LookaheadStream::new(&mut lex);

    let mut parser = Parser::new(&mut scanner);

    let p = parser.entry();

    if !cflags.eflags.silence_errors {
        parser.print_errors(contents, path_handle);
    }

    //
    match p {
        Ok(mut punit) => {
            if cflags.dump_tree {
                println!("Gets AST of: {}", punit);
            }

            analyze(&mut punit);
        }
        Err(err) => {
            println!("Failed to parse with error: {:?}", err);
        }
    }
}

/*pub fn parse<'a>(contents: &'a str) -> Result<ast::ParseUnit<'a>, ParseResultError<'a>> {


    r
}*/

#[derive(Default)]
pub struct EFlags {
    pub warnings_as_errors: bool,
    pub silence_warnings_below_level: i32,
    pub silence_errors: bool,
}

#[derive(Default)]
pub struct CFlags {
    pub arg_parsing_failed: bool,
    pub dump_tree: bool,
    pub eflags: EFlags,
    pub thread_count: usize,
}

pub fn launch(args: &[&str]) {
    //let args: Vec<String> = env::args().collect();
    //let args = prefix;

    enum State {
        ExpectInput,
        ExpectOutput,
        ExpectThreadCount,
        ExpectWarnFlags,
        ExpectErrorFlags,
    }

    let mut state = State::ExpectInput;

    let mut cflags = CFlags {
        thread_count: 1,
        ..Default::default()
    };

    let mut inputs = HashSet::<PathBuf>::new();
    let mut outputs = HashSet::<PathBuf>::new(); // need to figure out what to do for this one

    for s in args.iter() {
        let slice = &s[..];
        println!("got arg '{}'", slice);
        match slice {
            "-o" => state = State::ExpectOutput,
            "-i" => state = State::ExpectInput,
            "-Cthreads" => state = State::ExpectThreadCount,
            "-Dtree" => cflags.dump_tree = true,
            "-Esilent" => cflags.eflags.silence_errors = true,
            //"-Olib" => cflags.output_library = true,
            //"-Obin" => cflags.output_binary = true,
            other => {
                match state {
                    State::ExpectOutput | State::ExpectInput => {
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
                            State::ExpectInput => inputs.insert(canonicalized),
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
            }
        }
    }

    for p in inputs.iter() {
        println!("input file with path '{:#?}'", p);
    }

    let (error_sender, error_reciever) = crossbeam::unbounded();


    //static_assertions::assert_impl_all!(&mut [std::option::Option<crate::helper::FileHandle<'_>>]: rayon::iter::IntoParallelIterator);
    //static_assertions::assert_impl_all!(for<'data> &'data mut [Option<FileHandle<'data>>]: rayon::iter::IntoParallelIterator);
    //static_assertions::assert_impl_all!(FileHandle<'_>: Send);

    // start spawning the parser threads
    let pool = rayon::ThreadPoolBuilder::new()
        .num_threads(cflags.thread_count)
        .build()
        .expect("couldn't build thread pool");


    let mut pmap = explore_paths(inputs.into_iter().collect(), error_sender);

    let handles = pmap.get_handles();

    handles.iter_mut().for_each(|file| {
        if let Some(fh) = file {
            let contents = fh.open();
        }
    });

    //std::mem::drop(handles);

    //std::mem::drop(pmap);
}

use crate::ast::*;
use crate::mid_repr::ScopeContext;
use std::sync::{Arc, RwLock};

pub fn explore_paths<'error>(
    paths: Vec<PathBuf>,
    error_sink: crossbeam::Sender<Error<'error>>,
) -> PathIdMap<'error> {
    let mut vd = VecDeque::new();

    let global_context = Arc::new(RwLock::new(ScopeContext::new(
        error_sink.clone(),
        vec![String::from("crate")],
        None,
        None,
    )));

    for p in paths.into_iter() {
        vd.push_back((vec![String::from("crate")], p, global_context.clone()));
    }

    let mut pmap = PathIdMap::new();

    while !vd.is_empty() {
        let (scope, path, parent) = vd
            .pop_front()
            .expect("paths was not empty but had no front");
        let mut base_scope = scope.clone();
        let stem: String = path
            .file_stem()
            .expect("couldn't get file stem for a file")
            .to_string_lossy()
            .into();

        if path.is_file() {
            if let Some(ext) = path.extension() {
                if ext.to_string_lossy().to_string() == "rsh" {
                    let context = Arc::new(RwLock::new(ScopeContext::new(
                        error_sink.clone(),
                        base_scope,
                        Some(Arc::downgrade(&global_context)),
                        Some(Arc::downgrade(&parent)),
                    )));
                    pmap.push_path(path, context);
                }
            }
        } else if path.is_dir() {
            base_scope.push(stem);

            let context = Arc::new(RwLock::new(ScopeContext::new(
                        error_sink.clone(),
                        base_scope.clone(),
                        Some(Arc::downgrade(&global_context)),
                        Some(Arc::downgrade(&parent)),
                        )));


            for subpath in path
                .read_dir()
                .expect("couldn't read a directory in passed trees")
            {
                if let Ok(subpath) = subpath {
                    vd.push_back((base_scope.clone(), subpath.path(), context.clone()));
                }
            }
        }
    }

    pmap
}

pub fn prepass<'a>(p: &mut ast::OuterScope<'a>) {}

pub fn analyze<'a>(p: &mut ast::OuterScope<'a>) {}

pub fn tollvm<'a>(p: &mut ast::OuterScope<'a>, _filename: &str) {}
