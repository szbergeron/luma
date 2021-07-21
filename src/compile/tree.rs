use super::stager::ArgResult;
use super::CFlags;

use crate::ast::OuterScope;
use crate::helper::lex_wrap::ParseResultError;
use crate::helper::Error;
use crate::helper::*;
use crate::types::{GlobalCtx, GlobalCtxNode};
use dashmap::DashMap;

#[allow(unused_imports)]
use rayon::prelude::*;

use async_trait::async_trait;
use std::path::PathBuf;
use std::pin::Pin;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::helper::interner::*;
use futures::future::join_all;

use async_recursion::async_recursion;

type ErrorChannel = crossbeam::Sender<Error>;

pub struct PathNode {
    path: PathBuf,
    module: Vec<StringSymbol>,

    module_file: Option<PathBuf>,
    parsed: Option<OuterScope>,

    cflags: CFlags,
    children: Vec<Node>,
    error_channel: ErrorChannel,
}

pub enum Node {
    PathNode(PathNode),
    ErrorNode(ErrorNode),
}

impl Node {
    pub async fn parse(self, reg: &FileRegistry) -> Node {
        match self {
            Self::PathNode(pn) => pn.parse(reg).await,
            other => other,
        }
    }
}

impl PathNode {
    //#[async_recursion]
    fn new(
        path: PathBuf,
        module: Vec<StringSymbol>,
        args: &ArgResult,
        ec: ErrorChannel,
        children: Vec<Node>,
        module_file: Option<PathBuf>,
    ) -> Node {
        Node::PathNode(PathNode {
            path,
            module,
            module_file,
            children,
            error_channel: ec,
            cflags: args.flags,
            parsed: None,
        })
    }

    #[async_recursion]
    async fn async_err() -> Node {
        Node::ErrorNode(ErrorNode {})
    }

    #[async_recursion]
    async fn from_path(
        path: PathBuf,
        module_prefix: &Vec<StringSymbol>,
        channel: ErrorChannel,
        args: &ArgResult,
    ) -> Node {
        let stem: String = path
            .file_stem()
            .expect("couldn't get stem for a file")
            .to_string_lossy()
            .into();

        let mut self_module = module_prefix.clone();

        let (module_file, children) = match EitherNone::of_bool(path.is_file(), path.is_dir()) {
            EitherNone::A(_) => (Some(path.clone()), Vec::new()),
            EitherNone::B(_) => {


                //let mut child_set = Vec::new();
                let mut mod_file = None;

                let interned_mod = intern(stem.as_str());
                self_module.push(interned_mod);

                let children: Vec<Node> = join_all(path.read_dir()
                    .expect("couldn't read a directory in the passed source tree")
                    .filter_map(|entry| match entry {
                        Ok(entry) => {
                            let entry_path = entry.path();
                            match EitherNone::of_bool(entry_path.is_file(), entry_path.is_dir()) {
                                EitherNone::A(_) => {
                                    let ext = entry_path.extension()?;
                                    match ext.to_string_lossy().to_string().as_str() {
                                        "rsh" => match entry_path.file_stem().expect("There was no file stem?").to_str().expect("file stem was invalid unicode") {
                                            "mod" => {
                                                // NOTE: mut warn, this reaches around to set the "module
                                                // file". Borrowing rules allow this but it's good to note
                                                // where this gets set
                                                mod_file = Some(entry_path);
                                                None
                                            }, // we filter out the mod.rsh file since it is merged into toplevel
                                            _other => Some(Self::from_path(entry_path, &self_module, channel.clone(), args)),
                                        }
                                        _ => None,
                                    }
                                },
                                EitherNone::B(_) => {
                                    Some(Self::from_path(entry_path, &self_module, channel.clone(), args))
                                },
                                _ => panic!("didn't expect this but something is either both a file and a directory or is neither one"),
                            }
                        },
                        Err(_) => Some(Self::async_err()),
                    })).await;

                (mod_file, children)
            },
            other => {
                match other {
                    EitherNone::A(_) => println!("v A"),
                    EitherNone::B(_) => println!("v B"),
                    EitherNone::Both(_, _) => println!("v Both"),
                    EitherNone::Neither() => println!("v Neither"),
                };
                println!("path was {:?}, with is_file {} and is_dir {}", path, path.is_file(), path.is_dir());
                panic!("Can't handle something being both file and dir or neither file nor dir")
            }
        };

        PathNode::new(path, self_module, args, channel, children, module_file)
    }

    #[async_recursion]
    pub async fn parse(self, reg: &FileRegistry) -> Node {
        let children = join_all(self.children.into_iter().map(|child| child.parse(reg))).await;

        let id = reg.register(self.path.clone());
        let fho = reg.read(id).await;

        let mut fh = match fho {
            None => return Node::ErrorNode(ErrorNode {}),
            Some(fh) => fh,
        };

        fh.open();


        let handle = fh.as_ref().expect("File didn't open nicely");

        //let ref =
        let r = super::parse_unit(handle, self.module.clone(), &self.cflags);

        match r {
            Err(_) => Node::ErrorNode(ErrorNode {}),
            Ok(os) => Node::PathNode(PathNode {
                parsed: Some(os),
                children,
                ..self
            }),
        }
    }

    /*#[async_recursion]
    pub async fn parse(self, reg: &FileRegistry) -> TreeNode {
        let children = join_all(self.children.into_iter().map(|child| child.parse(reg))).await;

        /*TreeNode::DirNode(DirNode {
        children, module: self.module, error_channel: self.error_channel })*/
        Self::new(self.module, children, self.error_channel).await
    }*/
}

pub struct FileRegistry {
    //files: Vec<FileHandle>,
    id: AtomicUsize,
    files: DashMap<usize, PathBuf>,
}

impl FileRegistry {
    pub fn new() -> FileRegistry {
        FileRegistry {
            id: AtomicUsize::new(1),
            files: DashMap::new(),
        }
    }

    /// Not async so that this forces sync push before ID escapes
    pub fn register(&self, path: PathBuf) -> usize {
        let new_id = self.id.fetch_add(1, Ordering::Relaxed);

        self.files.insert(new_id, path);

        new_id
    }

    /// returns the contents of the file at <id> as a String, if possible to open
    ///
    /// was originally going to return Option<String>, but currently using FileHandle
    /// so that I'm reusing current parsing machinery
    ///
    /// TODO: reeval ownership/caching or move to simply returning a read String
    /// (or future on a String)
    ///
    /// TODO: changed to async to return a future on an option on FH, but
    /// should still reeval
    pub async fn read(&self, id: usize) -> Option<FileHandle> {
        //let p = self.files.get
        let p = self.files.get(&id)?;

        let fh = FileHandle::new(p.clone(), Some(id));

        Some(fh)
    }
}

#[async_trait]
pub trait IntoCtxNode {
    async fn into_ctx(self) -> Option<Pin<Box<GlobalCtxNode>>>;
}

/*#[async_trait]
pub trait AsCtxNode {
    async fn convert(&mut self) -> Pin<Box<GlobalCtxNode>>;
}*/

#[allow(dead_code)]
pub struct CompilationRoot {
    args: ArgResult,
    children: Vec<Node>,
    files: FileRegistry,
}

impl CompilationRoot {
    pub fn into_ctx(self) -> (ArgResult, FileRegistry, GlobalCtx) {
        let first_children = self
            .children
            .into_iter()
            .map(|treenode| treenode.into_ctx());

        todo!()
    }
}

pub struct ErrorNode {}

#[async_trait]
impl IntoCtxNode for Node {
    async fn into_ctx(self) -> Option<Pin<Box<GlobalCtxNode>>> {
        todo!()
    }
}

/*impl UnknownNode {
    pub async fn explore(self) -> TreeNode {
        unimplemented!()
    }
}*/
async fn first_pass(
    path: PathBuf,
    module_prefix: Vec<StringSymbol>,
    channel: ErrorChannel,
    reg: &FileRegistry,
    args: &ArgResult,
) -> Node {
    let node = PathNode::from_path(path, &module_prefix, channel, args);
    node.await.parse(reg).await
}

impl CompilationRoot {
    pub async fn initial(error_channel: ErrorChannel, args: ArgResult) -> CompilationRoot {
        /*for input in args.inputs {
        }*/
        let reg = FileRegistry::new();

        let res: Vec<Node> = join_all(args.inputs.iter().map(|pb| {
            let v = Vec::new();
            first_pass(pb.clone(), v, error_channel.clone(), &reg, &args)
        }))
        .await;

        CompilationRoot {
            children: res,
            args,
            files: reg,
        }
    }
}
