//use crate::ast::resolver::ResolverWorker;
use crate::ast::tree::{Contexts, CtxID};
use crate::compile::parse_tree::ParseTreeNode;
use crate::errors::CompilationError;
//use crate::ast;

use crate::compile::per_module::CompilationUnit;
use crate::lex::{ErrorSet, LookaheadHandle, TokenStream};
use crate::parse::schema::TokenProvider;
use crate::parse::Parser;
use std::collections::HashSet;
use std::str::FromStr;
use std::time::Duration;

use crate::{ast, cst};
use std::path::{Path, PathBuf};
use std::process;

//use crate::mid_repr::ScopeContext;

//use crate::ast::*;

use crate::helper::interner::*;
use itertools::Itertools;
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

    let contents_handle = handle.contents().unwrap();

    let content_str = contents_handle.0.as_str().unwrap();

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

    tracing::debug!("v: {v:?}, es: {es:?}");

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

    args.source_input
        .iter()
        .map(|p| {
            let f = FileRole::Source(SourceFile {
                location: p.clone(),
            });
            f
        })
        .for_each(|e| roles.push(e));

    args.spec_input
        .iter()
        .map(|p| {
            let f = FileRole::Spec(SpecFile {
                location: p.clone(),
            });
            f
        })
        .for_each(|e| roles.push(e));

    roles
}

fn print_from_root(root: CtxID, indent: usize, named: IStr) {
    let indents = (0..(indent * 4)).map(|_| " ").join("");
    let indentsi = indents.clone() + "  +";
    let node = root.resolve();

    let nsuper = node.parent;
    let nglobal = node.global;

    let inner = match &node.inner {
        ast::tree::NodeUnion::Type(t) => {
            let t = t.lock().unwrap();
            let fields = t
                .fields
                .clone()
                .into_iter()
                .map(|fm| {
                    (
                        fm.name,
                        fm.has_type.map(|syntrr| syntrr.resolve().unwrap().clone()),
                    )
                })
                .collect_vec();
            format!("type with fields: {:?}", fields)
        }
        ast::tree::NodeUnion::Function(fd, _) => {
            let name = fd.name;
            let args = fd
                .parameters
                .clone()
                .into_iter()
                .map(|(name, strr)| (name, strr.resolve().unwrap().clone()))
                .collect_vec();
            let rtype = fd.return_type.resolve().unwrap().clone();
            format!("fn {name}({args:?}) -> {rtype:?}")
        }
        ast::tree::NodeUnion::Global(_) => todo!(),
        ast::tree::NodeUnion::Empty() => {
            format!("module")
        }
    };

    println!("{indents}== node {root:?} with name {named}");
    println!("{indentsi} super at {nsuper:?}, global at {nglobal:?}");
    println!("{indentsi} value of: {inner}");
    for child in node.children.iter() {
        print_from_root(*child.value(), indent + 2, *child.key());
    }
}

async fn async_launch(args: ArgResult) {
    let files: FileRegistry = Default::default();

    let roles = args_to_roles(&args);

    //roles.push(FileRole::Source(SourceFile { location: PathBuf::from_str("./std.luma").unwrap() }));

    println!("Building node");

    // build the initial module tree based on file locations
    let usr_node = crate::compile::preparse_tree::from_roots(&files, roles);

    let std_node = crate::compile::preparse_tree::from_roots(
        &files,
        vec![FileRole::Source(SourceFile {
            location: PathBuf::from_str("./std.luma").unwrap(),
        })],
    );

    println!("Built node");

    // parse the files (parallel!)
    let usr_node = ParseTreeNode::from_preparse(usr_node, vec![], &args.flags).await;

    let std_node = ParseTreeNode::from_preparse(std_node, vec![], &args.flags).await;

    println!("usr node: {usr_node:#?}");
    println!("std node: {std_node:#?}");

    let true_root = ast::tree::Node::new(
        "root".intern(),
        Vec::new(),
        None,
        None,
        ast::tree::NodeUnion::Empty(),
        false,
        Vec::new(),
    );
    // convert CST to initial AST
    let usr_root = ast::tree::Node::from_parse(usr_node, "usr".intern(), true_root, true_root);

    let std_root = ast::tree::Node::from_parse(std_node, "std".intern(), true_root, true_root);

    true_root
        .resolve()
        .children
        .insert("std".intern(), std_root);
    true_root
        .resolve()
        .children
        .insert("usr".intern(), usr_root);

    println!("true root is ctx {true_root:?}");

    println!("roots:");
    print_from_root(true_root, 0, "root".intern());

    //panic!();

    let ids = Contexts::instance().get_ids();

    // use AST to resolve type references/import statements
    // at least at a syntactic level (no code analysis, but we do drop
    // down into code to resolve scoped names)
    //ResolverWorker::new(ids).resolve().await; // fix types up
    let cu = CompilationUnit::new(ids);

    let (es, mut er) = tokio::sync::mpsc::unbounded_channel();

    tokio::spawn(async move {
        std::thread::sleep(Duration::from_secs(1));
        let mut so_far = HashSet::new();
        while let Some(v) = er.recv().await {
            let v: CompilationError = v;

            if so_far.insert(v.clone()) {
            // need to break down the error
                let s = v.with_file_context(&files);
            }
        }
    });

    cu.launch(es).await;
    //todo!("spawn all the network stuff");

    // convert AST to MIR

    // do type inference directly on MIR, save inferred solutions aside

    // now that we have combined MIR and type solutions (plus fields),
    //

    //let node = node.into_ast();

    //let (node, errors) = node.typecheck();

    // now need to parse tree into...more

    //let root = super::tree::CompilationRoot::initial(error_sender, args).await;
    //TODO
    //abort();
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
        .enable_all()
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
                    }
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

pub fn parse<'a>(n: PreParseTreeNode<'a>) {}

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
