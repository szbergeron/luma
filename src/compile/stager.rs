//use crate::ast;
use crate::helper::lex_wrap::{LookaheadStream, ParseResultError, Wrapper};
use crate::helper::*;
use crate::parse::Parser;
use rayon::prelude::*;
use std::collections::HashSet;
use std::collections::VecDeque;

use std::path::{Path, PathBuf};
use std::process;

use crate::mid_repr::ScopeContext;
use std::sync::Arc;

use crate::ast::*;

use crate::encode::*;

use crate::helper::interner::*;
use tokio::runtime::*;


#[allow(unused_variables, dead_code)]
pub fn parse_unit<'file>(
    handle: FileHandleRef,
    //context: &ScopeContext,
    scope: Vec<StringSymbol>,
    cflags: &CFlags,
) -> Result<OuterScope, ParseResultError> {
    // clone because we don't want to keep lock open, and this should be rather cheap in the scheme
    // of things
    // TODO: eval if this even matters

    let contents = handle.contents;

    let base_path = handle.id;
    let mut lex = Wrapper::new(contents, base_path);
    let mut scanner = LookaheadStream::new(&mut lex);

    let mut parser = Parser::new(&mut scanner, scope);

    #[allow(irrefutable_let_patterns)]
    let p = if let iguard = interner() {
        let p = parser.entry();
        p
    } else {
        panic!("irrefutable pattern")
    };

    if !cflags.eflags.silence_errors {
        parser.print_errors(handle);
    }

    match p.as_ref() {
        Ok(punit) => {
            if cflags.dump_tree {
                println!("Gets AST of: {}", punit);
            }
            if cflags.dump_pretty {
                let mut s = String::new();
                punit.pretty(&mut s, 0);
                println!("{}", s);
            }
        }
        Err(err) => {
            println!("Failed to parse with error: {:?}", err);
        }
    }

    p
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
    let (error_sender, error_reciever) = crossbeam::unbounded();

    let root = super::tree::TreeRoot::initial(error_sender, args).await;
}

fn launch_old(args: ArgResult) {


    let mut path_map = PathIdMap::new();
    let mut scope_map = ScopeIdMap::new();

    //let interner: Arc<ThreadedRodeo<LargeSpur>> = Arc::new(ThreadedRodeo::new());

    let (error_sender, _error_reciever) = crossbeam::unbounded();

    // start spawning the parser threads
    let _pool = rayon::ThreadPoolBuilder::new()
        .num_threads(args.flags.thread_count)
        .build()
        .expect("couldn't build thread pool");

    args.inputs.iter().for_each(|path| {
        path_map.push_path(path.clone());
    });

    explore_paths(&mut path_map, &mut scope_map, error_sender);

    println!("going to iter and open files");

    path_map.handles_mut().par_iter_mut().for_each(|file| {
        file.open();
    });

    println!("opened files");

    let mut outers = Vec::new();

    // take files, parse each, and pair (by index) each file path with its AST root
    path_map
        .handles()
        .par_iter()
        .zip(scope_map.handles().par_iter())
        .map(|(file, scope)| {
            if let Some(handle_ref) = file.as_ref() {
                println!("parsing unit");
                // TODO: potentially remove ScopeContext from this and integrate later
                let scope_v = scope.get_scope().to_vec();
                let r = parse_unit(handle_ref, scope_v, &args.flags);
                r
            } else {
                Ok(OuterScope::new(NodeInfo::Builtin, Vec::new()))
            }
        })
        .collect_into_vec(&mut outers);

    outers
        .par_iter()
        .zip(path_map.handles().par_iter())
        .zip(scope_map.handles_mut().par_iter_mut())
        .for_each(|((outer, handle), scope_context)| {
            if handle.path().is_file() {
                if let Ok(root) = outer.as_ref() {
                    println!("root was ok");
                    scope_context.on_root(scope_context.clone(), root);
                } else {
                    println!("root was not ok");
                }
            } else {
                println!("handle was not file");
            }
        });

    println!("context tree:");
    println!("<output disabled>");
    //println!("{}", scope_map.global().unwrap().read().unwrap());

    scope_map
        .handles()
        .par_iter()
        .for_each(|handle| prepass(handle));

    scope_map
        .handles()
        .par_iter()
        .for_each(|handle| analyze(handle));

    let egctx = crate::encode::EncodeGlobalContext::new();

    scope_map
        .handles()
        .par_iter()
        .for_each(|handle| tollvm(handle, &egctx));

    println!("sm has {} handles", scope_map.handles().len());
    println!("egctx: \n{}", egctx);
}

pub fn launch(args: &[&str]) {
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
    pub inputs: HashSet<PathBuf>,
    pub outputs: HashSet<PathBuf>,
}

fn parse_args(args: &[&str]) -> Result<ArgResult, &'static str> {
    enum State {
        ExpectInput,
        ExpectOutput,
        ExpectThreadCount,
        _ExpectWarnFlags,
        _ExpectErrorFlags,
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
            "-Dpretty" => cflags.dump_pretty = true,
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

    Ok(ArgResult {
        inputs,
        outputs,
        flags: cflags,
    })
}

pub fn explore_paths<'context>(
    pidm: &mut PathIdMap,
    sidm: &mut ScopeIdMap,
    error_sink: crossbeam::Sender<Error>,
) -> () {
    let mut vd = VecDeque::new();

    let global_context =
        ScopeContext::new(error_sink.clone(), vec![intern("global")], None, None, None);

    sidm.set_global(global_context.clone());

    //let scopes = ScopeIdMap::new();

    for p in pidm.drain() {
        vd.push_back((vec![intern("crate")], p, global_context.clone()));
    }

    while !vd.is_empty() {
        let (scope, handle, parent) = vd
            .pop_front()
            .expect("paths was not empty but had no front");
        let mut base_scope = scope.clone();
        let path = handle.path().clone();

        let stem: String = path
            .file_stem()
            .expect("couldn't get file stem for a file")
            .to_string_lossy()
            .into();

        if path.is_file() {
            if let Some(ext) = path.extension() {
                if ext.to_string_lossy().to_string() == "rsh" {
                    let context = ScopeContext::new(
                        error_sink.clone(),
                        base_scope,
                        Some(Arc::downgrade(&global_context)),
                        Some(Arc::downgrade(&parent)),
                        None,
                    );
                    pidm.push_path(path);
                    sidm.push_scope(context.clone());

                    parent.add_child_context(context.clone());
                }
            }
        } else if path.is_dir() {
            base_scope.push(intern(stem.as_str()));

            let context = ScopeContext::new(
                error_sink.clone(),
                base_scope.clone(),
                Some(Arc::downgrade(&global_context)),
                Some(Arc::downgrade(&parent)),
                None,
            );

            let mut path = path;

            for subpath in path
                .read_dir()
                .expect("couldn't read a directory in passed trees")
            {
                if let Ok(subpath) = subpath {
                    if subpath
                        .path()
                        .file_name()
                        .expect("couldn't get file name of directory child")
                        .to_string_lossy()
                        .as_parallel_string()
                        == "mod.rsh"
                    {
                        path = subpath.path();
                    } else {
                        let new_handle = FileHandle::new(subpath.path(), None);
                        vd.push_back((base_scope.clone(), new_handle, context.clone()));
                    }
                }
            }

            pidm.push_path(path.clone());
            sidm.push_scope(context.clone());

            parent.add_child_context(context.clone());
        }
    }

    for (id, handle) in pidm.handles_mut().iter_mut().enumerate() {
        handle.set_id(id);
    }
}

pub fn prepass<'a>(p: &Arc<ScopeContext>) {
    println!("Prepass called on SC: {}", &*p);
}

pub fn analyze<'a>(_p: &Arc<ScopeContext>) {}

pub fn tollvm<'a>(p: &Arc<ScopeContext>, egctx: &EncodeGlobalContext) {
    let mut lctx = EncodeLocalContext::new(egctx);

    lctx.writeln(format!(
        "; start encode for ctx of {:?}",
        p.scope
            .iter()
            .map(|spur| spur.resolve())
            .collect::<Vec<&str>>()
    ));
    lctx.flush();
}
