use std::{
    collections::HashMap,
    error::Error,
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

use dashmap::DashMap;

#[derive(Clone, PartialEq, Eq, Hash)]
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

#[derive(Clone, PartialEq, Eq, Hash)]
struct DataFile {
    location: PathBuf,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct SpecFile {
    location: PathBuf,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct SourceFile {
    location: PathBuf,
}

pub struct FileRegistry {
    //files: HashMap<FileRole, usize>,
    //by_index: Vec<Option<FileRole>>,
    by_path: DashMap<FileRole, usize>,
    stored: DashMap<usize, File>,
}

#[derive(Clone)]
struct OpenedFile {
    contents: Arc<String>,
}

#[derive(Clone)]
struct File {
    id: usize,
    acts_as: FileRole,
    contents: OpenableFile,
}

#[derive(Clone)]
enum OpenableFile {
    Closed(),
    Open(Arc<String>),
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
    pub fn open_id(&self, id: usize) -> Result<Arc<String>, Box<dyn Error>> {
        let entry = self.stored.get(&id);

        match entry {
            Some(v) => {
                let val = v.value();
                match val.contents {
                    OpenableFile::Closed() => {
                        let v = std::fs::read_to_string(val.acts_as.as_path());
                        match v {
                            Ok(s) => {
                                let v = Arc::new(s);

                                let file = File {
                                    contents: OpenableFile::Open(v.clone()),
                                    ..val.clone()
                                };

                                let _ = self.stored.insert(id, file);

                                Ok(v)
                            }
                            Err(e) => Err(e.into()),
                        }
                    }
                    OpenableFile::Open(contents) => Ok(contents.clone()),
                }
            }
            None => panic!("tried to open an invalid id"),
        }
    }

    pub fn intern(&self, path: FileRole) -> usize {
        if let Some(v) = self.by_path.get(&path) {
            return *v.value(); // short circuit if already exists
        }

        static ID: AtomicUsize = AtomicUsize::new(1);

        let id = ID.fetch_add(1, Ordering::Relaxed);

        let _ = self.stored.insert(
            id,
            File {
                id,
                acts_as: path.clone(),
                contents: OpenableFile::Closed(),
            },
        );

        let entry = self.by_path.entry(path);

        let real_id = entry.or_insert(id).value();
        if *real_id != id {
            self.stored.remove(&id); // clean up, since we already existed
        }

        *real_id
    }

    pub fn open(&self, path: FileRole) -> (usize, Result<Arc<String>, Box<dyn Error>>) {
        // just double check if we even need to open it

        let id = self.intern(path);
        (id, self.open_id(id))
    }

    pub fn close(&self, id: usize) {
        self.stored.entry(id).and_modify(|val| val.contents = OpenableFile::Closed());
    }
}
