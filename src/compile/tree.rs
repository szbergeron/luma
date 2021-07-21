use super::stager::ArgResult;
use super::CFlags;

use crate::ast::OuterScope;
use crate::helper::Error;
use crate::helper::*;
use crate::helper::lex_wrap::ParseResultError;
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
    cflags: CFlags,
    children: Vec<Node>,
    error_channel: ErrorChannel,
}

pub enum Node {
    PathNode(PathNode),
    ErrorNode(ErrorNode),
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
        Node::PathNode(
            PathNode {
                path, module, module_file, children, error_channel: ec, cflags: args.flags
            }
        )
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

        let mut module_file = None;

        let mut child_set = Vec::new();

        let interned_mod = intern(stem.as_str());
        self_module.push(interned_mod);

        let children = join_all(path.read_dir()
            .expect("couldn't read a directory in the passed source tree")
            .filter_map(|entry| match entry {
                Ok(entry) => {
                    let path = entry.path();
                    match EitherNone::of_bool(path.is_file(), path.is_dir()) {
                        EitherNone::A(_) => {
                            let ext = path.extension()?;
                            match ext.to_string_lossy().to_string().as_str() {
                                "rsh" => match path.file_stem().expect("There was no file stem?").to_str().expect("file stem was invalid unicode") {
                                    "mod" => None, // we filter out the mod.rsh file since it is merged into toplevel
                                    other => Some(Self::from_path(path, &self_module, channel.clone(), args)),
                                }
                            }
                        },
                        EitherNone::B(_) => {
                            Some(Self::from_path(path, &self_module, channel.clone(), args))
                        },
                        other => panic!("didn't expect this but something is either both a file and a directory or is neither one"),
                    }
                },
                Err(_) => Some(Self::async_err()),
            })).await;

        if path.is_file() {
            if let Some(ext) = path.extension() {
                if ext.to_string_lossy().to_string() == "rsh" {
                    /*let merges_nextlevel =
                    if stem == "mod" {
                        // don't prefix the filename to the path
                        true
                    } else {
                        let interned_mod = intern(stem.as_str());
                        self_module.push(interned_mod);
                        false
                    };*/

                    //TreeNode::FileNode(FileNode { module: self_module, file: path })
                    PathNode::new(path, self_module, args, merges_nextlevel, channel).await
                } else {
                    TreeNode::ErrorNode(ErrorNode {})
                }
            } else {
                TreeNode::ErrorNode(ErrorNode {})
            }
        } else if path.is_dir() {

            let children: Vec<TreeNode> = join_all(
                path.read_dir()
                    .expect("couldn't read a directory in the passed source tree")
                    .filter_map(|entry| match entry {
                        Ok(entry) => Some(from_path(entry.path(), &self_module, channel.clone(), args)),
                        Err(_) => None,
                    }),
            )
            .await;

            //TreeNode::DirNode(DirNode { children, module: self_module })
            DirNode::new(self_module, children, channel).await
        } else {
            Node::ErrorNode(ErrorNode {})
        }

    }
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
    children: Vec<TreeNode>,
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

pub struct DirNode {
    module: Vec<StringSymbol>,
    children: Vec<TreeNode>,
    error_channel: ErrorChannel,
}

#[allow(dead_code)]
pub struct FileNode {
    path: PathBuf,
    module: Vec<StringSymbol>,
    parsed: Option<OuterScope>,
    error_channel: ErrorChannel,
    merges_to_parent: bool,
    cflags: CFlags,
}

/*pub struct ParsedFileNode {
    file: PathBuf,
    module: Vec<StringSymbol>,
    error_channel: ErrorChannel,
    contents: Option<String>,
}*/

impl DirNode {
    pub async fn new(
        //path: PathBuf,
        module: Vec<StringSymbol>,
        children: Vec<TreeNode>,
        ec: ErrorChannel,
    ) -> TreeNode {
        println!("Created a DirNode with mod {:?}", module);
        TreeNode::DirNode(DirNode {
            module,
            children,
            error_channel: ec,
        })
    }

    #[async_recursion]
    pub async fn parse(self, reg: &FileRegistry) -> TreeNode {
        let children = join_all(self.children.into_iter().map(|child| child.parse(reg))).await;

        /*TreeNode::DirNode(DirNode {
        children, module: self.module, error_channel: self.error_channel })*/
        Self::new(self.module, children, self.error_channel).await
    }
}

impl FileNode {
    pub async fn new(
        path: PathBuf,
        module: Vec<StringSymbol>,
        args: &ArgResult,
        merges_to_parent: bool,
        ec: ErrorChannel,
    ) -> TreeNode {
        println!("Created a filenode at {:?}", path);
        TreeNode::FileNode(FileNode {
            parsed: None,
            path,
            error_channel: ec,
            module,
            cflags: args.flags,
            merges_to_parent,
        })
    }

    pub fn substitutes_nextlevel_member(&self) -> bool {
        self.merges_to_parent
    }

    pub async fn parse(self, reg: &FileRegistry) -> TreeNode {
        /*let mut fh: FileHandle = FileHandle::new(self.path.clone(), None);
        fh.open();*/
        let id = reg.register(self.path.clone());
        let fho = reg.read(id).await;

        let mut fh = match fho {
            None => return TreeNode::ErrorNode(ErrorNode {}),
            Some(fh) => fh,
        };

        fh.open();

        let handle = fh.as_ref().expect("File didn't open nicely");

        //let ref =
        let r = super::parse_unit(handle, self.module.clone(), &self.cflags);

        match r {
            Err(_) => TreeNode::ErrorNode(ErrorNode {}),
            Ok(os) => TreeNode::FileNode(
                FileNode {
                    parsed: Some(os),
                    ..self
                })
        }
    }
}

pub struct ErrorNode {}

pub enum TreeNode {
    DirNode(DirNode),
    FileNode(FileNode),
    ErrorNode(ErrorNode),
}

impl TreeNode {
    /// Used currently for detecting when a file is named `mod.rsh`,
    /// and should act as the member directly above.
    ///
    /// If this returns true, then take the result of converting this member
    /// and use it namespaced as a substitute for self
    pub fn substitutes_nextlevel_member(&self) -> bool {
        match self {
            Self::FileNode(f) => f.substitutes_nextlevel_member(),
            _ => false,
        }
    }

    pub fn merge(self) -> Self {
        match self {
            Self::DirNode(dn) => Self::DirNode(dn.merge()),
            other => other,
        }
    }
}

#[async_trait]
impl IntoCtxNode for TreeNode {
    async fn into_ctx(self) -> Option<Pin<Box<GlobalCtxNode>>> {
        todo!()
    }
}

/*impl UnknownNode {
    pub async fn explore(self) -> TreeNode {
        unimplemented!()
    }
}*/

#[async_recursion]
async fn from_path(
    path: PathBuf,
    module_prefix: &Vec<StringSymbol>,
    channel: ErrorChannel,
    args: &ArgResult,
) -> TreeNode {
    let stem: String = path
        .file_stem()
        .expect("couldn't get stem for a file")
        .to_string_lossy()
        .into();

    let mut self_module = module_prefix.clone();

    if path.is_file() {
        if let Some(ext) = path.extension() {
            if ext.to_string_lossy().to_string() == "rsh" {
                let merges_nextlevel =
                if stem == "mod" {
                    // don't prefix the filename to the path
                    true
                } else {
                    let interned_mod = intern(stem.as_str());
                    self_module.push(interned_mod);
                    false
                };

                //TreeNode::FileNode(FileNode { module: self_module, file: path })
                FileNode::new(path, self_module, args, merges_nextlevel, channel).await
            } else {
                TreeNode::ErrorNode(ErrorNode {})
            }
        } else {
            TreeNode::ErrorNode(ErrorNode {})
        }
    } else if path.is_dir() {
        let interned_mod = intern(stem.as_str());
        self_module.push(interned_mod);

        let children: Vec<TreeNode> = join_all(
            path.read_dir()
                .expect("couldn't read a directory in the passed source tree")
                .filter_map(|entry| match entry {
                    Ok(entry) => Some(from_path(entry.path(), &self_module, channel.clone(), args)),
                    Err(_) => None,
                }),
        )
        .await;

        //TreeNode::DirNode(DirNode { children, module: self_module })
        DirNode::new(self_module, children, channel).await
    } else {
        TreeNode::ErrorNode(ErrorNode {})
    }
}

async fn first_pass(
    path: PathBuf,
    module_prefix: Vec<StringSymbol>,
    channel: ErrorChannel,
    reg: &FileRegistry,
    args: &ArgResult,
) -> TreeNode {
    let node = from_path(path, &module_prefix, channel, args);
    node.await.parse(reg).await
}

impl CompilationRoot {
    pub async fn initial(error_channel: ErrorChannel, args: ArgResult) -> CompilationRoot {
        /*for input in args.inputs {
        }*/
        let reg = FileRegistry::new();

        let res: Vec<TreeNode> = join_all(args.inputs.iter().map(|pb| {
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

impl TreeNode {
    pub async fn parse(self, reg: &FileRegistry) -> TreeNode {
        match self {
            Self::DirNode(d) => d.parse(reg).await,
            Self::FileNode(f) => f.parse(reg).await,
            Self::ErrorNode(e) => Self::ErrorNode(e),
        }
    }
}
