//use crate::lex;

use std::convert::Infallible;
use std::fs;
use std::marker::PhantomData;
use std::ops::ControlFlow;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};

use self::interner::IStr;

pub mod interner {
    use std::{fmt::{Debug, Display}, path::Path};

    #[derive(Copy, Clone, Hash, Eq, PartialEq)]
    pub struct IStr {
        internal: usize,
    }

    unsafe impl lasso::Key for IStr {
        fn into_usize(self) -> usize {
            self.internal
        }

        fn try_from_usize(int: usize) -> Option<Self> {
            Some(IStr { internal: int })
        }
    }

    impl Display for IStr {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.resolve())
        }
    }

    impl Debug for IStr {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "S('{}')", self.resolve())
        }
    }

    pub trait InternedModPath {
        fn into_parts(self) -> Result<Vec<IStr>, IStr>;
    }

    impl InternedModPath for IStr {
        fn into_parts(self) -> Result<Vec<IStr>, IStr> {
            let s = self.resolve();

            //s.chars().find(|c| c.is_alphabetic() || c == ':')

            let mut failed = false;

            let parts = s.split("::").map(|part| {
                if !s.chars().all(|c| c.is_alphabetic()) {
                    failed = true;
                }

                intern(s)
            }).collect();

            match failed {
                false => Ok(parts),
                true => Err(intern(format!("an invalid modpath was passed to into_parts, the given string was: {s}").as_str()))
            }
            //Ok(parts)
        }
    }

    pub enum Rodeo {
        ThreadedRodeo(lasso::ThreadedRodeo<IStr>),
        RodeoReader(lasso::RodeoReader<IStr>),
        RodeoResolver(lasso::RodeoResolver<IStr>),
    }
    lazy_static! {
        static ref INTERNER_OWNING: std::sync::Mutex<Option<std::sync::Arc<std::sync::RwLock<Rodeo>>>> =
            std::sync::Mutex::new(None);
    }

    thread_local! {
        // NOTE: lifetime considered 'unsafe
        static INTERNER_GUARD: std::cell::UnsafeCell<Option<std::sync::RwLockReadGuard<'static, Rodeo>>> = std::cell::UnsafeCell::new(None);

        static INTERNER_READ_COUNT_LOCAL: std::cell::UnsafeCell<i64> = std::cell::UnsafeCell::new(0);
    }

    pub struct InternerReadGuard {
        rodeo: &'static Rodeo,
    }

    // NOTE: this sometimes triggers bugs in rustc, if the compiler starts spitting out errors
    // remove the target/ directory and retry building
    impl InternerReadGuard {
        fn new() -> InternerReadGuard {
            unsafe {
                INTERNER_READ_COUNT_LOCAL.with(|v| {
                    let val = v.get();
                    *val += 1;
                });

                let r: &'static Rodeo = INTERNER_GUARD.with(|v| {
                    let optr = v.get();
                    let inner: &mut Option<std::sync::RwLockReadGuard<'static, Rodeo>> = optr
                        .as_mut()
                        .expect("UnsafeCell didn't turn into non-null ptr");
                    if inner.is_none() {
                        let mg = INTERNER_OWNING.lock().expect("Couldn't lock outer mutex");
                        let ir = mg.clone();
                        let io = ir.expect("Interner was not init'd by the time it was requested");
                        let ig = io.try_read().expect("Couldn't lock interner rwlock");

                        *inner = Some(
                            //ig as std::sync::RwLockReadGuard<'static, Rodeo>,
                            std::mem::transmute(ig),
                        );
                    }

                    &**inner
                        .as_ref()
                        .expect("Just set inner to Some, but was None")
                });

                InternerReadGuard { rodeo: r }
            }
        }

        /// return the value of the internal ref. This is marked unsafe as the lifetime should be
        /// 'unsafe, and is not actually 'static
        pub unsafe fn as_static(&self) -> &'static Rodeo {
            self.rodeo
        }
    }

    impl std::ops::Drop for InternerReadGuard {
        fn drop(&mut self) {
            unsafe {
                let do_dealloc = INTERNER_READ_COUNT_LOCAL.with(|v| {
                    let val = v.get();
                    *val -= 1;
                    *val == 0
                });

                if do_dealloc {
                    INTERNER_GUARD.with(|v| {
                        let optr = v.get();
                        *optr = None;
                    });
                }
            }
        }
    }

    /// Should be called before `interner()` is called, sets the static itself
    /// and issues memory barrier to try to swap the Option atomically
    ///
    /// relies on implicit SeqCst when in a single-threaded context
    pub unsafe fn init_interner() {
        let it = lasso::ThreadedRodeo::new();
        let en = Rodeo::ThreadedRodeo(it);
        let op = Some(std::sync::Arc::new(std::sync::RwLock::new(en)));

        let mut internal = INTERNER_OWNING
            .lock()
            .expect("Couldn't lock outer mutex for interner");
        *internal = op;
    }

    /// INVARIANT: must be called after init_interner has already been called in a single-threaded ONLY
    /// context. If this is called before, or init_interner was called during a data race,
    /// then this may panic as it can not find the interner present.
    pub fn interner() -> InternerReadGuard {
        InternerReadGuard::new()
    }

    pub fn intern(v: &str) -> IStr {
        unsafe {
            match interner().as_static() {
                Rodeo::ThreadedRodeo(tr) => tr.get_or_intern(v),
                _ => panic!("Tried to intern a string when interner was not in a writable state"),
            }
        }
    }

    pub trait Internable {
        fn intern(&self) -> IStr;
    }

    impl Internable for str {
        fn intern(&self) -> IStr {
            intern(&self)
        }
    }

    impl Internable for String {
        fn intern(&self) -> IStr {
            self.as_str().intern()
        }
    }

    impl Internable for Path {
        fn intern(&self) -> IStr {
            self.canonicalize().map(|canonicalized| canonicalized.to_str().map(|s| s.intern())).ok().flatten().unwrap_or(intern(self.to_str().unwrap_or("<unserializable path>")))
        }
    }

    pub fn intern_static(v: &'static str) -> IStr {
        unsafe {
            match interner().as_static() {
                Rodeo::ThreadedRodeo(tr) => tr.get_or_intern_static(v),
                _ => panic!("Tried to intern a string when interner was not in a writable state"),
            }
        }
    }

    pub trait SpurHelper {
        fn resolve(&self) -> &'static str;
        fn try_resolve(&self) -> Option<&'static str>;
    }

    impl SpurHelper for IStr {
        fn resolve(&self) -> &'static str {
            unsafe {
                match interner().as_static() {
                    Rodeo::ThreadedRodeo(tr) => tr.resolve(self),
                    Rodeo::RodeoResolver(rr) => rr.resolve(self),
                    Rodeo::RodeoReader(rr) => rr.resolve(self),
                }
            }
        }

        fn try_resolve(&self) -> Option<&'static str> {
            unsafe {
                match interner().as_static() {
                    Rodeo::ThreadedRodeo(tr) => tr.try_resolve(self),
                    Rodeo::RodeoResolver(rr) => rr.try_resolve(self),
                    Rodeo::RodeoReader(rr) => rr.try_resolve(self),
                }
            }
        }
    }
}

pub enum Either<A, B> {
    A(A),
    B(B),
}

pub enum EitherNone<A, B> {
    A(A),
    B(B),
    Both(A, B),
    Neither(),
}

impl<A, B> EitherNone<A, B> {
    pub fn with_a(self, a: A) -> EitherNone<A, B> {
        match self {
            Self::A(_) | Self::Neither() => Self::A(a),
            Self::B(b) => Self::Both(a, b),
            Self::Both(_, b) => Self::Both(a, b),
        }
    }

    pub fn with_b(self, b: B) -> EitherNone<A, B> {
        match self {
            Self::A(a) => Self::Both(a, b),
            Self::B(_) | Self::Neither() => Self::B(b),
            Self::Both(a, _) => Self::Both(a, b),
        }
    }

    pub fn a(&self) -> Option<&A> {
        match self {
            Self::A(a) => Some(&a),
            Self::B(_) | Self::Neither() => None,
            Self::Both(a, _) => Some(&a),
        }
    }

    pub fn b(&self) -> Option<&B> {
        match self {
            Self::A(_) | Self::Neither() => None,
            Self::B(b) => Some(&b),
            Self::Both(_, b) => Some(&b),
        }
    }

    pub fn of(a: Option<A>, b: Option<B>) -> EitherNone<A, B> {
        match a {
            Some(a) => match b {
                Some(b) => EitherNone::Both(a, b),
                None => EitherNone::A(a),
            },
            None => match b {
                Some(b) => EitherNone::B(b),
                None => EitherNone::Neither(),
            },
        }
    }
}

impl EitherNone<bool, bool> {
    pub fn of_bool(a: bool, b: bool) -> EitherNone<(), ()> {
        EitherNone::of(a.then_some(()), b.then_some(()))
    }
}

pub enum EitherAnd<A, B> {
    A(A),
    B(B),
    Both(A, B),
}

impl<A, B> std::ops::Try for EitherAnd<A, B> {
    type Output = (A, Option<B>);

    type Residual = EitherAnd<Infallible, B>;

    fn from_output(_output: Self::Output) -> Self {
        todo!()
    }

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self {
            Self::A(v) => ControlFlow::Continue((v, None)),
            Self::Both(v1, v2) => ControlFlow::Continue((v1, Some(v2))),
            Self::B(v) => ControlFlow::Break(EitherAnd::B(v)),
        }
    }
}

impl<A, B> std::ops::FromResidual for EitherAnd<A, B> {
    fn from_residual(r: EitherAnd<Infallible, B>) -> EitherAnd<A, B> {
        match r {
            EitherAnd::<Infallible, B>::B(b) => EitherAnd::B(b),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Error {
    DuplicateDefinition {
        duplicate_symbol: Arc<RwLock<!>>,
        existing_symbol: Arc<RwLock<!>>,
    },
}

/*#[derive(Clone, Debug, Eq, PartialEq)]
pub enum FileRole {
    /// A Data file is treated as pure
    /// binary data, and appears as effectively a
    /// vector of bytes, that is embedded within the
    /// data segment of the provided binary
    Data { path: PathBuf },

    /// A Spec file references other files (and potentially mount points)
    /// and allows for adding project structure using directories
    Spec { path: PathBuf },

    /// A Source file is a source code file in the language
    Source { path: PathBuf },

    /// A Virtual file doesn't actually exist, it is used
    /// for nodes within the project tree that are referred to
    /// but are implicitly role-d or are not
    /// actually directly mounted (such as a module within a mount path)
    Virtual {},
}*/

/*#[derive(Clone, Copy, Debug)]
pub enum FileRoleDescriminant {
    Data,
    Spec,
    Source,
}

#[derive(Clone)]
pub struct FileHandleRef<'a> {
    pub id: usize,
    pub contents: &'a str,
    //pub path: Path,
    pub role: FileRole,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct FileHandle2 /* where 'input: 'context */ {
    pub id: Option<usize>,
    //context: Option<std::pin::Pin<Weak<RwLock<ScopeContext<'static>>>>>,
    pub location: PathBuf,
    contents: Option<String>,
    //pub role: FileRole,
    //
    //phantom_str: std::marker::PhantomData<&'input str>,
}*/

/*#[allow(unused_unsafe)]
impl Drop for FileHandle {
    fn drop(&mut self) {
        unsafe {
            self.context = None; // need to drop context first
        }
    }
}*/

/*impl<'input> FileHandle2 {
    pub fn new(
        //role: FileRole,
        p: PathBuf,
        /*scope: Vec<String>,*/
        id: Option<usize>,
        //context: Arc<RwLock<ScopeContext<'context>>>, // should only refer to self here
    ) -> FileHandle2 {
        FileHandle2 {
            //role,
            location: p,
            //scope,
            id,
            //context: Some(context),
            contents: None,
            //content_ref: None,
            //phantom_str: std::marker::PhantomData::default(),
        }
    }

    pub fn open(&mut self) -> bool {
        match self.contents.clone() {
            Some(_contents) => true,
            None => {
                if self.location.is_file() {
                    let content_maybe = fs::read_to_string(self.location.clone());
                    match content_maybe {
                        Ok(contents) => {
                            self.contents = Some(contents);
                            return true;
                        }
                        Err(e) => {
                            println!(
                                "couldn't read to string file by path {:?}: {}",
                                self.location, e
                            );
                            panic!("couldn't read to string a file!");
                            //return false;
                        }
                    }
                } else {
                    //println!("tried to open something that isn't a file");
                    //panic!("tried to open something that isn't a file");
                    //return false;

                    //TODO: decide if allowing folders to be their own empty files is wrong
                    self.contents = Some(String::new());
                    return true;
                }
                //let content_rc = Arc::new(content_string);
                //let result = content_rc.clone();
                //self.contents = Some(content_rc);
            }
        }
    }

    pub fn set_id(&mut self, id: usize) {
        self.id = Some(id);
    }

    pub fn id(&self) -> Option<usize> {
        self.id
    }

    pub fn as_ref<'handle, 'ltself>(&'ltself self) -> Option<FileHandleRef>
    where
        'ltself: 'handle,
    {
        match self.contents.as_ref() {
            Some(s) => Some(FileHandleRef {
                id: self.id.unwrap_or(0),
                contents: s.get(..).unwrap(),
                role: todo!(),
                //role: self.role.clone(),
            }),
            None => None,
        }
    }

    pub fn close(&mut self) {
        self.contents = None; // any remaining Rcs will need to drop before string drops
    }

    pub fn path(&self) -> &PathBuf {
        &self.location
    }
}*/

pub mod lex_wrap {}

pub mod locks {
    thread_local! {
        static THREAD_LOCKS: std::cell::UnsafeCell<std::collections::HashSet<usize>> = std::cell::UnsafeCell::new(std::collections::HashSet::new());
    }

    static LOCKID: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(1);

    pub struct RecursiveRWLockWriteGuard<'a, T: ?Sized + 'a> {
        lock: &'a RecursiveRWLock<T>,
        guard: Option<std::sync::RwLockWriteGuard<'a, ()>>,
    }

    pub struct RecursiveRWLockReadGuard<'a, T: ?Sized + 'a> {
        lock: &'a RecursiveRWLock<T>,
        guard: Option<std::sync::RwLockReadGuard<'a, ()>>,
        //outer_guard: bool,
    }

    #[derive(Debug)]
    pub struct RecursiveRWLock<T: ?Sized> {
        //accessors: std::sync::Mutex<std::collections::HashSet<std::thread::ThreadId>>,
        id: usize,
        wraps: std::sync::RwLock<()>,
        content: std::cell::UnsafeCell<T>,
    }

    impl<T: Sized> RecursiveRWLock<T> {
        pub fn new(data: T) -> RecursiveRWLock<T> {
            RecursiveRWLock {
                id: LOCKID.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
                wraps: std::sync::RwLock::new(()),
                content: std::cell::UnsafeCell::new(data),
            }
        }

        pub fn read(&self) -> Result<RecursiveRWLockReadGuard<T>, ()> {
            //let l = self.accessors.lock().expect("Couldn't lock internal accessors lock");

            THREAD_LOCKS.with(|cell| {
                //if std::thread::current().id()
                unsafe {
                    if (*cell.get()).contains(&self.id) {
                        let g = self.wraps.read().map_err(|_| ())?;
                        Ok(RecursiveRWLockReadGuard {
                            lock: &self,
                            guard: Some(g),
                        })
                    } else {
                        (*cell.get()).insert(self.id);
                        Ok(RecursiveRWLockReadGuard {
                            lock: &self,
                            guard: None,
                        })
                    }
                }
            })

            //std::mem::drop(l);

            //panic!()
        }

        pub fn write(&self) -> Result<RecursiveRWLockWriteGuard<T>, ()> {
            //let l = self.accessors.lock().expect("Couldn't lock internal accessors lock");

            THREAD_LOCKS.with(|cell| unsafe {
                if (*cell.get()).contains(&self.id) {
                    let g = self.wraps.write().map_err(|_| ())?;
                    Ok(RecursiveRWLockWriteGuard {
                        lock: &self,
                        guard: Some(g),
                    })
                } else {
                    (*cell.get()).insert(self.id);
                    Ok(RecursiveRWLockWriteGuard {
                        lock: &self,
                        guard: None,
                    })
                }
            })

            //std::mem::drop(l);

            //panic!()
        }
    }

    unsafe impl<T: ?Sized + Send> Sync for RecursiveRWLock<T> {}

    impl<T: ?Sized> std::ops::Deref for RecursiveRWLockReadGuard<'_, T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            unsafe { &*self.lock.content.get() }
        }
    }

    impl<T: ?Sized> std::ops::Deref for RecursiveRWLockWriteGuard<'_, T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            unsafe { &*self.lock.content.get() }
        }
    }

    impl<T: ?Sized> std::ops::DerefMut for RecursiveRWLockWriteGuard<'_, T> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            unsafe { &mut *self.lock.content.get() }
        }
    }

    impl<T: ?Sized> std::ops::Drop for RecursiveRWLockReadGuard<'_, T> {
        fn drop(&mut self) {
            if let Some(_) = self.guard.take() {
                unsafe {
                    THREAD_LOCKS.with(|cell| (*cell.get()).remove(&self.lock.id));
                }
            }
        }
    }

    impl<T: ?Sized> std::ops::Drop for RecursiveRWLockWriteGuard<'_, T> {
        fn drop(&mut self) {
            if let Some(_) = self.guard.take() {
                unsafe {
                    THREAD_LOCKS.with(|cell| (*cell.get()).remove(&self.lock.id));
                }
            }
        }
    }
}

/// Inefficient stub for now, will implement the multivec algo described in the
/// architecture notes document eventually.
///
/// Interface is designed to be compatible with that as a drop-in

pub trait VecOps {
    type Item;
    fn appended(self, i: Self::Item) -> Self;
    fn merged(self, o: Self) -> Self;
    fn appended_opt(self, i: Option<Self::Item>) -> Self;
    fn indexed_insert(&mut self, val: Self::Item) -> usize;
}

impl<T> VecOps for Vec<T> {
    type Item = T;

    fn appended(mut self, i: Self::Item) -> Self {
        self.push(i);
        self
    }

    fn merged(self, o: Self) -> Self {
        self.append(&mut o);

        self
    }

    fn appended_opt(mut self, i: Option<Self::Item>) -> Self {
        match i {
            Some(i) => self.push(i),
            None => (),
        }

        self
    }

    fn indexed_insert(&mut self, val: Self::Item) -> usize {
        let index = self.len();
        self.push(val);
        index
    }
}

/*pub enum EitherNone<TA, TB> {
    A(TA),
    B(TB),
    Neither(),
    Both(TA, TB),
}

impl<TA, TB> EitherNone<TA, TB> {
    pub fn of(a: Option<TA>, b: Option<TB>) -> EitherNone<TA, TB> {
        match a {
            Some(a) => match b {
                Some(b) => EitherNone::Both(a, b),
                None => EitherNone::A(a),
            },
            None => match b {
                Some(b) => EitherNone::B(b),
                None => EitherNone::Neither(),
            },
        }
    }
}

impl EitherNone<(), ()> {
    pub fn of_bool(a: bool, b: bool) -> EitherNone<(), ()> {
        EitherNone::of(a.then_some(()), b.then_some(()))
    }
}*/

struct BoxcarInterner<T> {
    inner: boxcar::Vec<T>,
    id: usize,
}

struct InternedRefInner<T> {
    for_boxcar: usize,
    index: usize,
    ty: PhantomData<T>,
}

pub struct InternedRef<T> {
    inner: once_cell::sync::OnceCell<InternedRefInner<T>>,
}

impl<T> InternedRef<T> {
}
