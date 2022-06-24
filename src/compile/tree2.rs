use std::{
    path::PathBuf, cell::UnsafeCell, sync::atomic::fence,
};


use dashmap::DashMap;


use crate::helper::{interner::IStr, FileRole};

pub struct SpecTree {
    files: Vec<FileRole>,

    children: Vec<SpecTree>,
}

/// Covers the idea of a concrete syntax tree,
/// with specs having done mounting and everything
/// already and fil
#[derive(Debug, Clone)]
pub struct PreParseTreeNode<'registry> {
    self_file: FileRole,

    canonicalized_mount_point: IStr,

    native: Vec<PreParseTreeNode<'registry>>,
    mounted: Vec<(IStr, PreParseTreeNode<'registry>)>,

    path_registry: &'registry DashMap<PathBuf, IStr>,
    //module_registry: &'registry DashMap<IStr, PathBuf>,
}

impl<'r> PreParseTreeNode<'r> {
    pub fn native_path(&self) -> PathBuf {
        Self::destructure_role(&self.self_file).0
    }

    /// takes a stubbed node that was given only a file path and a role,
    /// and "expands" it into a full node
    pub fn open(self) -> Self {
        todo!()
    }

    pub fn destructure_role(r: &FileRole) -> (PathBuf, &'static str) {
        match r {
            FileRole::Data { path } => { (path.clone(), "data") },
            FileRole::Spec { path } => { (path.clone(), "spec") },
            FileRole::Source { path } => { (path.clone(), "source") },
            FileRole::Virtual {  } => { (PathBuf::default(), "virtual") },
        }
    }

    /**
     * self_file: the file this is based on
     * arena: set of all files referenced by this project, so we can check for reimports
     * or import loops
     * mount_point: the fully canonicalized path to where this module is mounted
     **/
    pub fn new(self_file: FileRole, registry: &'r DashMap<PathBuf, IStr>, mount_point: IStr) -> Option<Self> {
        let (path, _) = Self::destructure_role(&self_file);

        match registry.get(&path) {
            Some(m) => {

                let m = m.value();
                println!("File {path:?} was already mounted at mount point {m:?}, when asked to mount it at {mount_point:?}");

                None
            }
            None => {
                registry.insert(path, mount_point);

                Some(Self {
                    self_file,
                    native: Vec::new(),
                    mounted: Vec::new(),
                    path_registry: registry,
                    canonicalized_mount_point: mount_point,
                })
            }
        }
    }

    pub fn combine2(mut self, other: Self) -> Self {
        //let native = self.native;

        self.native.push(other);

        self
    }

    pub fn plumb(&mut self, rem_path: &[IStr], full_path: IStr) {
    }
}




//#[self_referencing]
/// Make sure all roots use the same file_registry!
pub struct PreParseTree<'registry> {
    file_registry: &'registry DashMap<PathBuf, IStr>,
    module_registry: &'registry DashMap<IStr, PathBuf>,

    //#[borrows(file_registry)]
    //#[covariant] // I'm like 99% sure this is true, since a PreParseTreeNode<'a: 'this> is valid for shorter-than/same-as one from 'this
    root: UnsafeCell<Option<PreParseTreeNode<'registry>>>,
}

impl<'registry> PreParseTree<'registry> {
    /// Allows taking two "roots" and joining them recursively
    pub fn merge(mut self, mut other: Self) -> Self {
        fence(std::sync::atomic::Ordering::Acquire);

        let (selfroot, otherroot) = unsafe {
            let sroot = self.root.get_mut().take();
            let oroot = other.root.get_mut().take();

            (sroot, oroot)
        };

        let root = match selfroot {
            Some(mut roota) => {
                match otherroot {
                    Some(rootb) => {
                        roota.native.push(rootb);

                        Some(roota)
                    },
                    None => {
                        Some(roota)
                    }
                }
            },
            None => {
                // we need to make otherroot have the ref of our own file registry now
                //otherroot.iter_mut().for_each(|r| r.path_registry = self.file_registry.get_ref());

                otherroot
            }
        };

        // TODO: figure out if we need to emit warnings here if the same file
        // exists from two roots, it could potentially allow invalid loops
        //self.file_registry.extend(other.file_registry.into_iter());

        let v = Self { root: UnsafeCell::new(root), ..self };

        // make sure that the store to root is visible on all platforms
        fence(std::sync::atomic::Ordering::Release);

        v
    }
}
