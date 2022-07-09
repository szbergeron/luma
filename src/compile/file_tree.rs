use std::{
    collections::HashMap,
    error::Error,
    hash::Hash,
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

use dashmap::DashMap;

use crate::helper::interner::IStr;

//use crate::helper::interner::IStr;

pub struct Spec {
    pub entries: Vec<(Vec<IStr>, FileRole)>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MountPoint {
    /// Acts as basically an "include" statement,
    /// the spec or source elements from the target
    /// module are dumped directly into the current module
    Here(),

    /// States that the referenced element should be mounted
    /// within a module named <.0>
    Nest(IStr),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum FileRole {
    Data(DataFile),
    Spec(SpecFile),
    Source(SourceFile),
}

impl FileRole {
    pub fn as_path(&self) -> &Path {
        match self {
            Self::Data(DataFile { location })
            | Self::Spec(SpecFile { location })
            | Self::Source(SourceFile { location }) => location.as_path(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct DataFile {
    pub location: PathBuf,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct SpecFile {
    pub location: PathBuf,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct SourceFile {
    pub location: PathBuf,
}

#[derive(Default)]
pub struct FileRegistry {
    //files: HashMap<FileRole, usize>,
    //by_index: Vec<Option<FileRole>>,
    by_path: DashMap<FileRole, usize>,
    stored: DashMap<usize, File>,
}

impl std::fmt::Debug for FileRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FileRegistry").finish()
    }
}

#[derive(Clone)]
pub struct OpenedFile {
    contents: Arc<String>,
}

#[derive(Clone)]
pub struct File {
    id: usize,
    acts_as: FileRole,
    contents: OpenableFile,
}

#[derive(Clone, Copy, Debug)]
pub struct FileHandle<'a> {
    id: usize,
    within: &'a FileRegistry,
}

impl<'a> FileHandle<'a> {
    pub fn path(&self) -> PathBuf {
        self.within
            .stored
            .get(&self.id)
            .unwrap()
            .value()
            .acts_as
            .as_path()
            .to_owned()
    }

    pub fn id(&self) -> usize {
        self.id
    }

    pub fn contents(&self) -> Result<Arc<Contents>, Box<dyn std::error::Error>> {
        self.within.open_id(self.id)
    }

    pub fn close(&self) {
        self.within.close(self.id)
    }
}

impl PartialEq for FileHandle<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for FileHandle<'_> {}

impl Hash for FileHandle<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

//trait ClonableError: Error + Clone {}

//impl<E> ClonableError for E where E: Error + Clone {}

#[derive(Clone)]
enum OpenableFile {
    Closed(Option<String>),
    Open(Arc<Contents>),
}

pub enum Contents {
    String(String),
    Binary(Vec<u8>),
}

impl Contents {
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Self::String(s) => Some(s.as_str()),
            _ => None,
        }
    }
}

impl OpenedFile {
    pub fn open(role: FileRole) -> Self {
        let p = role.as_path();

        let f = std::fs::read_to_string(p);

        match f {
            Ok(content) => Self {
                contents: Arc::new(content),
            },
            _ => todo!(),
        }
    }
}

impl FileRegistry {
    pub fn open_id(&self, id: usize) -> Result<Arc<Contents>, Box<dyn Error>> {
        println!("Entering open_id");
        //let entry = self.stored.get(&id);

        println!("Got entry");

        let res = self.stored.entry(id).and_modify(|e| {
            //let val = e;
            //let val = v.value();
            match &e.contents {
                OpenableFile::Closed(_) => {
                    let v = std::fs::read_to_string(e.acts_as.as_path());
                    match v {
                        Ok(s) => {
                            let v = Arc::new(Contents::String(s));

                            let file = File {
                                contents: OpenableFile::Open(v.clone()),
                                ..e.clone()
                            };

                            *e = file;

                            //let _ = self.stored.insert(id, file);

                            //Ok(v)
                        }
                        Err(err) => {
                            *e = File {
                                contents: OpenableFile::Closed(Some(format!("{err}").into())),
                                ..e.clone()
                            };
                        }
                    };
                }
                OpenableFile::Open(_contents) => {
                    // already open, do nothing
                }
            };
        });

        println!("Did first part");

        let res = res.or_insert_with(|| File {
            contents: OpenableFile::Closed(Some(
                "opaque error trying to open file, ID must not have existed".to_owned(),
            )),
            id,
            acts_as: FileRole::Source(SourceFile {
                location: "/dev/null".into(),
            }),
        });

        let res = res.value().clone();

        match res.contents {
            OpenableFile::Closed(e) => Err(e.unwrap_or("empty error".to_string()).into()),
            OpenableFile::Open(c) => Ok(c),
        }
    }

    pub fn intern(&self, path: FileRole) -> FileHandle {
        if let Some(v) = self.by_path.get(&path) {
            return FileHandle {
                id: *v.value(),
                within: &self,
            }; // short circuit if already exists
        }

        // allows us to only hash the ID for a given filehandle,
        // since they're unique across *all* registries
        static ID: AtomicUsize = AtomicUsize::new(1);

        let id = ID.fetch_add(1, Ordering::Relaxed);

        let _ = self.stored.insert(
            id,
            File {
                id,
                acts_as: path.clone(),
                contents: OpenableFile::Closed(None),
            },
        );

        let entry = self.by_path.entry(path);

        let real_id = *entry.or_insert(id).value();
        if real_id != id {
            self.stored.remove(&id); // clean up, since we already existed
        }

        FileHandle {
            id: real_id,
            within: &self,
        }
    }

    pub fn open(&self, path: FileRole) -> (FileHandle, Result<Arc<Contents>, Box<dyn Error>>) {
        // just double check if we even need to open it

        let handle = self.intern(path);
        (handle, self.open_id(handle.id))
    }

    pub fn close(&self, id: usize) {
        self.stored
            .entry(id)
            .and_modify(|val| val.contents = OpenableFile::Closed(None));
    }
}
