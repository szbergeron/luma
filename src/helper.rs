//use crate::lex;
use crate::ast::*;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};
use std::fs;

pub mod interner {
    use std::fmt::{Debug, Display};

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

pub enum EitherAnd<A, B> {
    A(A),
    B(B),
    Both(A, B),
    Neither,
}

impl<A, B> EitherAnd<A, B> {
    pub fn with_a(self, a: A) -> EitherAnd<A, B> {
        match self {
            Self::A(_) | Self::Neither => Self::A(a),
            Self::B(b) => Self::Both(a, b),
            Self::Both(_, b) => Self::Both(a, b),
        }
    }

    pub fn with_b(self, b: B) -> EitherAnd<A, B> {
        match self {
            Self::A(a) => Self::Both(a, b),
            Self::B(_) | Self::Neither => Self::B(b),
            Self::Both(a, _) => Self::Both(a, b),
        }
    }

    pub fn a(&self) -> Option<&A> {
        match self {
            Self::A(a) => Some(&a),
            Self::B(_) | Self::Neither => None,
            Self::Both(a, _) => Some(&a),
        }
    }

    pub fn b(&self) -> Option<&B> {
        match self {
            Self::A(_) | Self::Neither => None,
            Self::B(b) => Some(&b),
            Self::Both(_, b) => Some(&b),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Error {
    DuplicateDefinition {
        duplicate_symbol: Arc<RwLock<SymbolDeclaration>>,
        existing_symbol: Arc<RwLock<SymbolDeclaration>>,
    },
}

#[derive(Clone, Copy)]
pub struct FileHandleRef<'a> {
    pub id: usize,
    pub contents: &'a str,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct FileHandle /* where 'input: 'context */ {
    pub id: Option<usize>,
    //context: Option<std::pin::Pin<Weak<RwLock<ScopeContext<'static>>>>>,
    pub location: PathBuf,
    contents: Option<String>,
    //phantom_str: std::marker::PhantomData<&'input str>,
}

/*#[allow(unused_unsafe)]
impl Drop for FileHandle {
    fn drop(&mut self) {
        unsafe {
            self.context = None; // need to drop context first
        }
    }
}*/

impl<'input> FileHandle {
    pub fn new(
        p: PathBuf,
        /*scope: Vec<String>,*/
        id: Option<usize>,
        //context: Arc<RwLock<ScopeContext<'context>>>, // should only refer to self here
    ) -> FileHandle {
        FileHandle {
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
}

pub mod lex_wrap {
    use crate::{helper::interner::*, lex::Token};
    use logos::Logos;
    use std::rc::Rc;

    type LexResult<'a> = Result<TokenWrapper, ParseResultError>;

    #[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
    pub enum CodeLocation {
        Parsed(Loc),
        Builtin,
    }

    impl CodeLocation {
        pub fn offset_by(&self, line: isize, offset: isize) -> CodeLocation {
            match self {
                Self::Builtin => Self::Builtin,
                Self::Parsed(l) => Self::Parsed(Loc {
                    line: l.line + line,
                    offset: l.offset + offset,
                    file_id: l.file_id,
                }),
            }
        }
    }

    #[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
    pub struct Loc {
        pub line: isize,
        pub offset: isize,
        pub file_id: usize,
    }

    impl std::fmt::Display for CodeLocation {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Parsed(l) => write!(f, "({}:{})", l.line, l.offset),
                Self::Builtin => write!(f, "(builtin)"),
            }
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub struct TokenWrapper {
        pub token: crate::lex::Token,
        pub slice: IStr,
        pub start: CodeLocation,
        pub end: CodeLocation,
    }

    pub enum Error {
        FileError(FileResultError),
        ParseError(ParseResultError),
    }

    pub enum FileResultError {
        FileNotFound { filename: String },
    }

    #[derive(Debug, Clone)]
    pub enum ParseResultError {
        InternalParseIssue,
        EndOfFile,
        NotYetParsed,
        /// The found token (and position), followed by a list of possible tokens here, followed by
        /// a message (if applicable)
        UnexpectedToken(TokenWrapper, Vec<crate::lex::Token>, Option<&'static str>),
        SemanticIssue(&'static str, CodeLocation, CodeLocation),
        ErrorWithHint {
            hint: &'static str,
            original: Box<ParseResultError>,
        },
    }

    impl ParseResultError {
        pub fn add_expect(&mut self, toks: &[crate::lex::Token]) {
            match self {
                Self::UnexpectedToken(_tw, v, None) => {
                    v.extend(toks);
                }
                _ => {}
            }
        }
    }

    pub struct LexerStream<'a> {
        lexer: logos::Lexer<'a, crate::lex::Token>,
        cur: Result<TokenWrapper, ParseResultError>,

        current_line: isize,
        last_newline_absolute: usize,
        file_id: usize,
    }

    impl<'a> LexerStream<'a> {
        pub fn new(input: &'a str, file_id: usize) -> LexerStream<'a> {
            let lex = crate::lex::Token::lexer(input);

            LexerStream {
                lexer: lex,
                cur: Err(ParseResultError::NotYetParsed),
                last_newline_absolute: 0,
                current_line: 1,
                file_id,
            }
        }

        pub fn peek(&mut self) -> LexResult {
            self.cur.clone()
        }

        pub fn advance(&mut self) -> () {
            let tok = self.lexer.next();
            match tok {
                Some(tok) => {
                    let (startloc, endloc) = match tok {
                        crate::lex::Token::Newline => {
                            let sp = self.lexer.span();

                            let start = Loc {
                                line: self.current_line,
                                offset: (sp.start - self.last_newline_absolute) as isize,
                                file_id: self.file_id,
                            };

                            self.current_line += 1;
                            self.last_newline_absolute = sp.end;

                            let end = Loc {
                                line: self.current_line,
                                offset: (sp.end - self.last_newline_absolute) as isize,
                                file_id: self.file_id,
                            };

                            (start, end)
                        }
                        _ => {
                            let sp = self.lexer.span();
                            let start = Loc {
                                line: self.current_line,
                                offset: (sp.start - self.last_newline_absolute) as isize,
                                file_id: self.file_id,
                            };
                            let end = Loc {
                                line: self.current_line,
                                offset: (sp.end - self.last_newline_absolute) as isize,
                                file_id: self.file_id,
                            };
                            (start, end)
                        }
                    };
                    self.cur = Ok(TokenWrapper {
                        token: tok,
                        //slice: interner().get_or_intern(self.lexer.slice()),
                        slice: intern(self.lexer.slice()),
                        start: CodeLocation::Parsed(startloc),
                        end: CodeLocation::Parsed(endloc),
                    })
                }
                None => self.cur = Err(ParseResultError::EndOfFile),
            }
        }

        pub fn next(&mut self) -> LexResult<'a> {
            self.advance();
            self.peek()
        }
    }

    #[derive(Clone)]
    pub struct LookaheadStream {
        tokens: Rc<Vec<TokenWrapper>>,
        index: usize,
        //latest: Option<TokenWrapper<'a>>,
    }

    //use crate::lex::Token;

    impl LookaheadStream {
        pub fn new(w: &mut LexerStream) -> LookaheadStream {
            let mut v = Vec::new();
            let mut comment_level = 0;
            let mut inside_line_comment = false;

            // NOTE: we keep this outside the loop here so that we can simply "truncate" when done
            // with it and keep the allocation for use with later blocks
            //
            // This technically grows unbounded, but it's bounded same as string interner to
            // input size in total * 2 so we don't worry about it hanging around a bit longer
            // during lex
            let mut llvm_rest = String::new();

            while let Ok(mut tw) = w.next() {
                // handle comments
                match tw.token {
                    Token::LLVMOpen => {
                        // do the parsing of the llvm block in its entirety here
                        // the parser will never actually see an LLVMOpen or LLVMClose

                        'llvm_collector: while let Ok(itw) = w.next() {
                            tw.end = itw.end;
                            match itw.token {
                                Token::LLVMClose => {
                                    break 'llvm_collector;
                                }
                                other => {
                                    println!(
                                        "llvm pushes token {:?} with slice {}",
                                        other, itw.slice
                                    );
                                    llvm_rest.push_str(itw.slice.resolve());
                                }
                            }
                        }

                        tw.token = Token::LLVMBlock;
                        tw.slice = intern(llvm_rest.as_str());

                        // need to empty the string since it lives outside the loop
                        // this preserves the allocation, though
                        llvm_rest.truncate(0);
                    }
                    Token::LineCommentStart => {
                        inside_line_comment = true;
                        continue;
                    }
                    Token::Newline => {
                        inside_line_comment = false;
                        continue;
                    }
                    Token::LBlockComment | Token::LDocComment => {
                        comment_level += 1;
                        continue;
                    }
                    Token::RBlockComment | Token::RDocComment => {
                        if comment_level > 0 {
                            comment_level -= 1; // will cause syntax error in else during parse
                        }

                        continue;
                    }
                    Token::Tab | Token::Space => {
                        // these don't have any syntactic meaning so we simply filter them
                        // we keep them in so that they are fed through to llvm, however
                        continue;
                    }
                    Token::Error => {
                        // filter these outside of llvm blocks
                        continue;
                    }
                    _ => {}
                }
                if !inside_line_comment && comment_level == 0 {
                    v.push(tw);
                }
            }

            LookaheadStream {
                tokens: Rc::new(v),
                index: 0,
                //latest: None,
            }
        }

        pub fn seek_to(&mut self, index: usize) {
            self.index = index;
        }

        pub fn seek_by(&mut self, offset: isize) {
            self.index = (self.index as isize + offset) as usize;
        }

        pub fn index(&self) -> usize {
            self.index
        }

        pub fn ffwd(&mut self, other: &LookaheadStream) {
            self.seek_to(other.index());
        }

        pub fn next(&mut self) -> LexResult {
            //self.tokens[self.index]
            let r = self.la(0);
            //self.latest = Some(r);

            //self.index += 1;
            self.advance();

            r
        }

        pub fn prev(&mut self) -> LexResult {
            let r = self.la(0);

            //self.index -= 1;
            self.backtrack();

            r
        }

        pub fn backtrack(&mut self) {
            self.index -= 1;
        }

        pub fn advance(&mut self) {
            self.index += 1;
        }

        pub fn la(&mut self, offset: isize) -> LexResult {
            let index = self.index as isize + offset;
            if index < 0 {
                Err(ParseResultError::NotYetParsed)
            } else {
                let r = self
                    .tokens
                    .get(index as usize)
                    .map_or(Err(ParseResultError::EndOfFile), |&t| Ok(t));

                //println!("la gives result: {:?}", r);

                r
            }
        }
    }
}

pub mod locks {
    /*enum RWLockState {
        Released,
        Shared,
        SharedSingle,
        Exclusive,
    }

    struct RWLockStateMachine {
        state: std::sync::atomic::AtomicUsize,
    }

    impl RWLockStateMachine {
        // if lock was already only being read
        pub fn read_again(&self) -> bool {
        }
    }*/

    /*struct RecursiveRWLockInnerGuard<T> {
        guard: T,
    }

    impl

    impl<T> RecursiveRWLockInnerGuard<T> {
        pub fn new(guard: T) -> Self {
            RecursiveRWLockInnerGuard { guard }
        }
    }*/
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
    fn appended_opt(self, i: Option<Self::Item>) -> Self;
}

impl<T> VecOps for Vec<T> {
    type Item = T;

    fn appended(mut self, i: Self::Item) -> Self {
        self.push(i);
        self
    }

    fn appended_opt(mut self, i: Option<Self::Item>) -> Self {
        match i {
            Some(i) => self.push(i),
            None => (),
        }

        self
    }
}

pub enum EitherNone<TA, TB> {
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
}
