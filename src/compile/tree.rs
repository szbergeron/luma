use super::stager::ArgResult;
use super::CFlags;

use crate::helper::Error;
use crate::helper::*;
use dashmap::DashMap;

#[allow(unused_imports)]
use rayon::prelude::*;

use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};



use crate::helper::interner::*;
use futures::future::join_all;

use async_recursion::async_recursion;

type ErrorChannel = crossbeam::Sender<Error>;

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

#[allow(dead_code)]
pub struct CompilationRoot {
    args: ArgResult,
    children: Vec<TreeNode>,
    files: FileRegistry,
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
    error_channel: ErrorChannel,
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
        ec: ErrorChannel,
    ) -> TreeNode {
        println!("Created a filenode at {:?}", path);
        TreeNode::FileNode(FileNode {
            path,
            error_channel: ec,
            module,
            cflags: args.flags,
        })
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

        TreeNode::FileNode(self)
    }
}

pub struct ErrorNode {}

pub enum TreeNode {
    DirNode(DirNode),
    FileNode(FileNode),
    ErrorNode(ErrorNode),
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
                if stem == "mod" {
                    // don't prefix the filename to the path
                } else {
                    let interned_mod = intern(stem.as_str());
                    self_module.push(interned_mod);
                }
                //TreeNode::FileNode(FileNode { module: self_module, file: path })
                FileNode::new(path, self_module, args, channel).await
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
