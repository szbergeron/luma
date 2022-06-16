use std::{
    collections::{HashMap, HashSet},
    path::PathBuf, pin::Pin, cell::UnsafeCell, sync::atomic::fence,
};

use chashmap::CHashMap;
use dashmap::DashSet;
use once_cell::sync::OnceCell;

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

    native: Vec<PreParseTreeNode<'registry>>,
    mounted: Vec<(IStr, PreParseTreeNode<'registry>)>,

    registry: &'registry DashSet<PathBuf>,
}

impl PartialEq for PreParseTreeNode<'_> {
}

impl<'r> PreParseTreeNode<'r> {
    /// takes a stubbed node that was given only a file path and a role,
    /// and "expands" it into a full node
    pub fn open(self) -> Self {
        todo!()
    }

    pub fn new(self_file: FileRole, within: &'r DashSet<PathBuf>) -> Self {
        Self {
            self_file,
            native: Vec::new(),
            mounted: Vec::new(),
            registry: within,
        }
    }

    /// Take this node and merge both the native mounts and any
    /// submounts recursively if they overlap
    pub fn merge(self, other: Self) -> Self {
        let native = self.native;

        //self.native.insert(self.role);

        if native.contains(&other.role) {
            println!(
                "There was already a {:?} within native for role",
                other.role
            );
        } else {
            native.push(other.role);
        }

        for val in other.native.into_iter() {
            if native.contains(&val) {
                println!(
                    "There was already a {:?} within native for role",
                    other.role
                );
            } else {
                native.push(val);
            }
        }

        for (mpoint, val) in other.mounted.into_iter() {
        }

        todo!()

        //
    }
}

use ouroboros::self_referencing;


#[self_referencing]
pub struct PreParseTree {
    file_registry: Pin<Box<DashSet<PathBuf>>>,

    #[borrows(file_registry)]
    #[covariant] // I'm like 99% sure this is true, since a PreParseTreeNode<'a: 'this> is valid for shorter-than/same-as one from 'this
    root: UnsafeCell<Option<PreParseTreeNode<'this>>>,
}

impl PreParseTree {
    /// Allows taking two "roots" and joining them recursively
    pub fn merge(self, other: Self) -> Self {
        fence(std::sync::atomic::Ordering::Acquire);

        let (selfroot, otherroot) = unsafe {
            let sroot = *self.root.get();
            let oroot = *other.root.get();

            (sroot, oroot)
        };

        let root = match selfroot {
            Some(roota) => {
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
                otherroot.iter_mut().for_each(|r| r.registry = self.file_registry.get_ref());

                otherroot
            }
        };

        // TODO: figure out if we need to emit warnings here if the same file
        // exists from two roots, it could potentially allow invalid loops
        self.file_registry.extend(other.file_registry.into_iter());

        let v = Self { root: UnsafeCell::new(root), ..self };

        // make sure that the store to root is visible on all platforms
        fence(std::sync::atomic::Ordering::Release);

        v
    }
}
