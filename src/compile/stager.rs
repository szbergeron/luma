use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::thread;
use crate::parse::Parser;
use crate::helper::lex_wrap::{Wrapper, LookaheadStream, ParseResultError};
use crate::ast;
use std::collections::HashSet;
use std::process;
use crossbeam::unbounded;
use std::collections::VecDeque;
use rayon::prelude::*;

pub fn parse_unit(file_scope: Vec<String>, base_path: PathBuf, cflags: &CFlags) {
    let contents = &fs::read_to_string(base_path).expect("could not read contents of target file")[..];
    let mut lex = Wrapper::new(contents);
    let mut scanner = LookaheadStream::new(&mut lex);

    let mut parser = Parser::new(&mut scanner);

    let p = parser.entry();

    parser.print_errors(contents);


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
            //"-Olib" => cflags.output_library = true,
            //"-Obin" => cflags.output_binary = true,
            other => {
                match state {
                    State::ExpectOutput | State::ExpectInput => {
                        // should be a file or directory in this case
                        let path = Path::new(other);
                        let canonicalized = path.canonicalize().unwrap_or_else(|_| {
                            println!("Invalid compiler argument: got '{}' but was expecting a path", other);
                            process::exit(-1)
                        });
                        let _inserted = match state {
                            State::ExpectInput => inputs.insert(canonicalized),
                            State::ExpectOutput => outputs.insert(canonicalized),
                            _ => panic!("state was expecting a path then it wasn't"),
                        }; // maybe check for duplicates here?
                    },
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

    // start spawning the parser threads
    let pool = rayon::ThreadPoolBuilder::new().num_threads(cflags.thread_count).build().expect("couldn't build thread pool");
    let (s1, r) = unbounded();
    thread::spawn(move || path_exp(inputs.into_iter().collect(), s1));

    while let Ok(Some((sn, p))) = r.recv() {
        pool.install(|| {
            parse_unit(sn, p, &cflags);
        });
    }

}

pub fn path_exp(paths: Vec<PathBuf>, s: crossbeam::Sender<Option<(Vec<String>, PathBuf)>>) {
    let mut vd = VecDeque::new();
    for p in paths.into_iter() {
        vd.push_back((Vec::new(), p));
    }
    while !vd.is_empty() {
        let (mut sn, p) = vd.pop_front().expect("paths was not empty but has no front");
        if p.is_file() {
            s.send(Some((sn, p))).expect("couldn't send");
        } else if p.is_dir() {
            sn.push(p.file_stem().expect("couldn't get file stem of entry in directory").to_string_lossy().into());
            for subpath in p.read_dir().expect("couldn't read a directory in passed trees") {
                if let Ok(subpath) = subpath {
                    vd.push_back((sn.iter().cloned().collect(), subpath.path()));
                }
            }
        }
    }

    s.send(None).expect("couldn't send end notify");

}

pub fn prepass<'a>(p: &mut ast::OuterScope<'a>) {
}

pub fn analyze<'a>(p: &mut ast::OuterScope<'a>) {
}

pub fn tollvm<'a>(p: &mut ast::OuterScope<'a>, _filename: &str) {
}
