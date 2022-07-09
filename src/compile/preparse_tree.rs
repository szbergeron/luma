//use std::{cell::UnsafeCell, path::PathBuf, sync::atomic::fence};

/// PROJECT_DEPTH allows setting the point
/// at which we have to store some things not "inline"
/// for module depth. If someone has more than PROJECT_DEPTH nested
/// modules, some SmallVecs will have anything under those as OOL
/// allocations which will cost more for those instances, and may come
/// with a branch prediction penalty across other modules
const PROJECT_DEPTH: usize = 8;

use std::collections::VecDeque;

use dashmap::{DashMap, DashSet};
use smallvec::SmallVec;

use crate::{
    compile::file_tree::{FileHandle, FileRegistry, FileRole},
    helper::interner::IStr,
    lex::LookaheadHandle,
    lex::TokenStream,
    parse::schema::TokenProvider,
    parse::Parser,
};

pub struct PreParseTreeNode<'r> {
    pub files: &'r FileRegistry,

    //children: Vec<PreParseTreeNode<'r>>,
    pub native: DashSet<FileHandle<'r>>,
    pub children: DashMap<IStr, PreParseTreeNode<'r>>,
}

impl<'r> PreParseTreeNode<'r> {
    pub fn add_children<I>(&self, children: I)
    where
        I: Iterator<Item = (IStr, PreParseTreeNode<'r>)>,
    {
        for child in children {
            self.children.insert(child.0, child.1);
        }
    }

    pub fn add_native<I>(&self, files: I)
    where
        I: Iterator<Item = FileHandle<'r>>,
    {
        for file in files {
            self.native.insert(file);
        }
    }

    pub fn new(files: &'r FileRegistry) -> Self {
        Self {
            files,
            native: DashSet::new(),
            children: DashMap::new(),
        }
    }

    pub fn incorporate(&self, other: PreParseTreeNode<'r>) {
        for elem in other.native.into_iter() {
            self.native.insert(elem);
        }

        for (key, elem) in other.children.into_iter() {
            self.children
                .entry(key)
                .or_insert_with(|| PreParseTreeNode::new(self.files))
                .incorporate(elem);
            //self.children.entry(key).and_modify
            //self.children.entry(key).and_modify(|e| e.incorporate())
            //self.children.insert(key, elem);
        }
    }

    pub fn insert(&self, remainder: &[IStr], node: PreParseTreeNode<'r>) {
        match remainder {
            [] => {
                // merge these nodes

                self.incorporate(node);
            }
            [first, more @ ..] => {
                let child = PreParseTreeNode::new(self.files);
                child.insert(more, node);
                self.children.insert(*first, child);
            }
        }
    }

    pub fn perform<F>(&self, remainder: &[IStr], f: F)
    where
        F: FnOnce(&PreParseTreeNode<'r>),
    {
        match remainder {
            [] => f(&self),
            [first, more @ ..] => {
                self.children
                    .entry(*first)
                    .or_insert(Self::new(self.files))
                    .perform(more, f);
                //let val = self.children.entry(*first).or_insert(Self::new(self.files));
                //let entry = self.children.entry(*first)
            }
        }
    }
}

pub fn from_roots<'r>(reg: &'r FileRegistry, files: Vec<FileRole>) -> PreParseTreeNode<'r> {
    let mut remaining_files: VecDeque<(SmallVec<[IStr; PROJECT_DEPTH]>, FileRole)> =
        VecDeque::new();

    for file in files {
        println!("Pushed {file:?}");
        remaining_files.push_back((vec![].into(), file));
    }

    let root = PreParseTreeNode::new(reg);

    while let Some((path, file)) = remaining_files.pop_front() {
        let handle = reg.intern(file.clone());
        match file {
            FileRole::Data(_) | FileRole::Source(_) => {
                root.perform(path.as_slice(), |h| {
                    h.native.insert(handle);
                });
            }
            FileRole::Spec(_) => {
                // open, parse, add dependent files in
                //
                println!("Parsing a spec");

                let contents = handle.contents().unwrap();

                println!("Got contents");
                let content_str = contents.as_str().unwrap();

                let file_id = handle.id();

                let lex = TokenStream::new(content_str, file_id);
                let tv = lex.to_vec();
                let scanner = LookaheadHandle::new(&tv);

                println!("About to create parser");
                let mut parser = Parser::new(scanner.clone(), path.to_vec());

                println!("Creating tokenprovider");
                let t: TokenProvider = TokenProvider::from_handle(scanner);

                println!("About to entry");

                let specs = parser.parse_spec(&t, handle.path().as_path());

                println!("Done parse");

                let (v, es, s) = specs.open_anyway();

                let spec = v.unwrap();

                for (mount, file) in spec.entries {
                    println!("Pushing entry: {file:?}");
                    remaining_files.push_back((mount.into(), file));
                }
            }
        }
    }

    root
}
