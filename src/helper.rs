//use crate::lex;
use crate::ast::*;

//use atomic_option::AtomicOption;

use std::alloc::Layout;
use std::marker::PhantomData;
//use lock_api::RawRwLockRecursive;
use std::intrinsics::transmute;
use std::mem::{transmute_copy, MaybeUninit};
use std::path::PathBuf;
use std::pin::Pin;
use std::ptr::{addr_of_mut, null, null_mut};
use std::sync::atomic::{AtomicPtr, AtomicUsize};
use std::sync::{Arc, RwLock};
use std::{alloc, fs};

pub mod interner {
    use std::fmt::{Debug, Display};

    //pub type StringSymbol = lasso::LargeSpur;
    #[derive(Copy, Clone, Hash, Eq, PartialEq)]
    pub struct StringSymbol {
        internal: usize,
    }

    unsafe impl lasso::Key for StringSymbol {
        fn into_usize(self) -> usize {
            self.internal
        }

        fn try_from_usize(int: usize) -> Option<Self> {
            Some(StringSymbol { internal: int })
        }
    }

    impl Display for StringSymbol {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.resolve())
        }
    }

    impl Debug for StringSymbol {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "S('{}')", self.resolve())
        }
    }

    pub enum Rodeo {
        ThreadedRodeo(lasso::ThreadedRodeo<StringSymbol>),
        RodeoReader(lasso::RodeoReader<StringSymbol>),
        RodeoResolver(lasso::RodeoResolver<StringSymbol>),
    }
    //static mut INTERNER_PRIV: Option<Rodeo> = None;
    //static mut INTERNER_UNLOCKED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);
    lazy_static! {
        static ref INTERNER_OWNING: std::sync::Mutex<Option<std::sync::Arc<std::sync::RwLock<Rodeo>>>> =
            std::sync::Mutex::new(None);
    }

    thread_local! {
        // NOTE: lifetime considered 'unsafe
        static INTERNER_GUARD: std::cell::UnsafeCell<Option<std::sync::RwLockReadGuard<'static, Rodeo>>> = std::cell::UnsafeCell::new(None);

        static INTERNER_READ_COUNT_LOCAL: std::cell::UnsafeCell<i64> = std::cell::UnsafeCell::new(0);
    }

    /********thread_local! {
        static INTERNER_OWNING: Option<std::sync::Arc<std::sync::RwLock<Rodeo>>> = None;
        static INTERNER_READ_GUARD: Option<Box<std:;sync::RwLockReadGuard<Rodeo>>> = None;
    }*/

    /*lazy_static! {
        static ref INTERNER: lock_api::RawRwLock<Option<Rodeo>> = {
            lock_api::RawRwLock::new(None)
        };
    }*/

    //static INTERNER: lock_api::RwLock<Option<Rodeo>> =
    /*lazy_static! {
        static ref INTERNER: lock_api::RwLock<lock_api::RwLock<Option<Rodeo>>, Option<Rodeo>> = {
            std::sync::RwLock::new()
        };
    }*/

    //unsafe impl !Send for InternerReadGuard;

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

                //let inner = INTERNER_GUARD.get_mut();

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

    /*impl std::ops::Deref for InternerReadGuard {
        type Target = Rodeo;

        fn deref(&self) -> &Self::Target {
            self.rodeo
        }
    }*/

    //unsafe fn open_read() -> InternerReadGuard

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

        //let b = Box::new(it);

        //let ptr = b.into_raw();

        //std::sync::atomic::compiler_fence(std::sync::atomic::Ordering::SeqCst);

        //INTERNER_PRIV.swap(ptr, std::sync::atomic::Ordering::SeqCst);
        //INTERNER_PRIV = op;

        //std::sync::atomic::compiler_fence(std::sync::atomic::Ordering::SeqCst);
    }

    /// INVARIANT: must be called after init_interner has already been called in a single-threaded ONLY
    /// context. If this is called before, or init_interner was called during a data race,
    /// then this may panic as it can not find the interner present.
    pub fn interner() -> InternerReadGuard {
        InternerReadGuard::new()
        /*unsafe {
            INTERNER_GUARD.with(|optref| {
                optref.as_ref().expect("Interner didn't exist yet")
        }*/
    }

    pub fn intern(v: &str) -> StringSymbol {
        unsafe {
            match interner().as_static() {
                Rodeo::ThreadedRodeo(tr) => tr.get_or_intern(v),
                _ => panic!("Tried to intern a string when interner was not in a writable state"),
            }
        }
    }

    pub fn intern_static(v: &'static str) -> StringSymbol {
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

    impl SpurHelper for StringSymbol {
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

use crate::mid_repr::ScopeContext;

pub type PathId = usize;
pub type PathIdMapHandle<'context> = Arc<locks::RecursiveRWLock<PathIdMap>>;

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

    /*pub fn slice(&self) -> Option<&str> {
        /*if self.contents.is_some() {
            //Some(&self.contents.unwrap()[..])
            Some(&self.contents[..])
        } else {
            None
        }*/
        Some(&self.contents[..])
        /*match self.contents.clone() {
            Some(contents) => Some( &contents[..]),
            None => None,
        }*/
    }*/

    pub fn as_ref<'handle, 'ltself>(&'ltself self) -> Option<FileHandleRef>
    where
        'ltself: 'handle,
    {
        //println!("getting a ref for file with path {:?}", self.location);
        match self.contents.as_ref() {
            Some(s) => Some(FileHandleRef {
                id: self.id.unwrap_or(0),
                contents: s.get(..).unwrap(),
            }),
            None => None,
        }
        /*FileHandleRef {
            id: self.id.unwrap_or(0),
            contents: self.contents.as_ref().unwrap().get(..).unwrap(),
            /*contents: unsafe {
                let slice = self.contents.as_ref().unwrap().get(..).unwrap();
                // String is pinned still, so slice is valid for all of 'self

                let p = slice as *const str;

                p.as_ref().unwrap()
                // invariant: p was already nonnull because of cast from ref
            }*/
        }*/
    }

    pub fn close(&mut self) {
        self.contents = None; // any remaining Rcs will need to drop before string drops
    }

    pub fn path(&self) -> &PathBuf {
        &self.location
    }

    /*pub fn context<'context>(&self) -> Arc<RwLock<ScopeContext<'context>>> {
        self.context.as_ref().unwrap().clone()
    }*/
}

pub struct PathIdMap {
    paths: Vec<FileHandle>,
}

pub struct ScopeIdMap {
    global_context: Option<Arc<ScopeContext>>,
    scopes: Vec<Arc<ScopeContext>>,
}

impl ScopeIdMap {
    pub fn new() -> ScopeIdMap {
        ScopeIdMap {
            scopes: Vec::new(),
            global_context: None,
        }
    }

    pub fn handles(&self) -> &[Arc<ScopeContext>] {
        &self.scopes[..]
    }

    pub fn handles_mut(&mut self) -> &mut [Arc<ScopeContext>] {
        &mut self.scopes[..]
    }

    pub fn set_global(&mut self, global: Arc<ScopeContext>) {
        self.global_context = Some(global);
    }

    pub fn global(&self) -> Option<Arc<ScopeContext>> {
        self.global_context.clone()
    }

    pub fn push_scope(&mut self, scope: Arc<ScopeContext>) {
        self.scopes.push(scope);
    }
    //
}

impl<'context> PathIdMap {
    pub fn new_locked() -> Arc<locks::RecursiveRWLock<PathIdMap>> {
        Arc::new(locks::RecursiveRWLock::new(Self::new()))
    }

    pub fn new() -> PathIdMap {
        let v = Vec::new();

        PathIdMap { paths: v }
    }

    pub fn drain(&mut self) -> std::vec::Drain<FileHandle> {
        self.paths.drain(..)
    }

    pub fn push_path(
        &mut self,
        p: PathBuf,
        //*scope: Vec<String>,*/ context: Arc<RwLock<ScopeContext<'context>>>,
    ) -> PathId {
        let id = self.paths.len();
        self.paths.push(FileHandle::new(p, Some(id)));

        id
    }

    pub fn get_path(&self, id: PathId) -> Option<&PathBuf> {
        match self.paths.get(id) {
            Some(p) => Some(p.path()),
            None => None,
        }
    }

    pub fn get_file(&self, id: PathId) -> Option<&FileHandle> {
        match self.paths.get(id) {
            Some(f) => Some(f),
            _ => None,
        }
    }

    pub fn handles(&self) -> &[FileHandle] {
        &self.paths[..]
    }

    pub fn handles_mut(&mut self) -> &mut [FileHandle] {
        &mut self.paths[..]
    }
}

pub mod lex_wrap {
    use crate::{helper::interner::*, lex::Token};
    use logos::Logos;
    use std::rc::Rc;

    type ParseResult<'a> = Result<TokenWrapper, ParseResultError>;

    pub struct Wrapper<'a> {
        lexer: logos::Lexer<'a, crate::lex::Token>,
        cur: Result<TokenWrapper, ParseResultError>,

        current_line: isize,
        last_newline_absolute: usize,
        file_id: usize,
    }

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
        //pub absolute: usize,
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
            //write!(f, "({}:{})", self.line, self.offset)
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub struct TokenWrapper {
        pub token: crate::lex::Token,
        pub slice: StringSymbol,
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
        //ExpectedExpressionNotPresent,
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

    impl<'a> Wrapper<'a> {
        pub fn new(input: &'a str, file_id: usize) -> Wrapper<'a> {
            let lex = crate::lex::Token::lexer(input);

            Wrapper {
                lexer: lex,
                cur: Err(ParseResultError::NotYetParsed),
                last_newline_absolute: 0,
                current_line: 1,
                file_id,
            }
        }

        pub fn peek(&mut self) -> ParseResult {
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

        pub fn next(&mut self) -> ParseResult<'a> {
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
        pub fn new(w: &mut Wrapper) -> LookaheadStream {
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

        pub fn next(&mut self) -> ParseResult {
            //self.tokens[self.index]
            let r = self.la(0);
            //self.latest = Some(r);

            //self.index += 1;
            self.advance();

            r
        }

        pub fn prev(&mut self) -> ParseResult {
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

        pub fn la(&mut self, offset: isize) -> ParseResult {
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

/*pub mod locks {
    pub mod recursive_rwlock {
        pub struct RecursiveRWLock {
        }
    }
}*/

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

const CHUNK_COUNT: usize = 32;
const FIRST_CHUNK_SIZE: usize = 32;

//#[repr(C)]
struct Chunk<T> {
    size: usize,
    cur: AtomicUsize,
    content: [MaybeUninit<T>],
}

impl<T> Chunk<T> {
    /// Based on code provided by Plecra#5251 for fat ptr handling, declared public domain (MIT
    /// compatible)
    pub fn with_capacity(num: usize) -> *mut Self {
        let layout = unsafe {
            alloc::Layout::for_value_raw(
                std::ptr::slice_from_raw_parts(std::ptr::null::<()>(), num) as *const Self,
            )
        };

        let ptr = unsafe { alloc::alloc(layout) };
        if ptr.is_null() {
            alloc::handle_alloc_error(layout);
        }

        let ptr = std::ptr::slice_from_raw_parts_mut(ptr, num) as *mut Self;
        unsafe {
            addr_of_mut!((*ptr).cur).write(AtomicUsize::new(0));
            //Box::from_raw(ptr)
            ptr
        }
    }

    pub unsafe fn make_fat_ptr(r: *mut (), len: usize) -> *mut Self {
        //according to https://doc.rust-lang.org/nightly/core/ptr/trait.Pointee.html, the
        //metadata of the last field is the metadata for the fat ptr used
        std::ptr::from_raw_parts_mut(r, len)
        /*let conv: *mut T = std::mem::transmute(r);
        let r: *mut [T] = core::slice::from_raw_parts_mut(conv, len);
        r as *mut Self*/
    }
}

#[repr(C)]
struct SizedChunk<T> {
    size: usize,
    cur: AtomicUsize,
    content: [MaybeUninit<T>; 1],
}

pub struct AtomicVec<T> {
    self_key: usize,
    chunks: [AtomicPtr<()>; CHUNK_COUNT],
    lengths: [usize; CHUNK_COUNT],
    cur: AtomicUsize,
    phantom: PhantomData<T>, //
                             //content_vec: RwLock<Vec<RwLock<T>>>
}

impl<T> AtomicVec<T> {
    fn make_lengths() -> [usize; CHUNK_COUNT] {
        let mut c = [0; CHUNK_COUNT];

        let mut cur_size = FIRST_CHUNK_SIZE;

        for i in 0..c.len() {
            c[i] = cur_size;
            cur_size *= 2;
        }

        c
    }

    pub fn new() -> AtomicVec<T> {
        static init_key: AtomicUsize = AtomicUsize::new(0);
        //const nptr: AtomicPtr<()> = AtomicPtr::new(null_mut());

        /*let mut chunks = Vec::new();
        for i in 0..32 {
            chunks.push(AtomicPtr::new(null_mut()));
        }

        //let (slice, _, _) = chunks.into_raw_parts();*/

        AtomicVec {
            self_key: init_key.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
            chunks: Default::default(), // null ptrs
            cur: AtomicUsize::new(0),
            phantom: Default::default(),
            lengths: Self::make_lengths(),
        }
    }

    fn get_chunk(&self, idx: usize) -> &Chunk<T> {
        // check first if it was already non-null
        let ptr = self.chunks[idx].load(std::sync::atomic::Ordering::Acquire);

        let r = if ptr.is_null() {
            // do alloc speculatively since it was null at first load
            let size = (2 as usize).pow(idx as u32) * FIRST_CHUNK_SIZE;

            let fat_ptr: *mut Chunk<T> = Chunk::with_capacity(size);

            //core::slice::
            let (ptr, _meta) = fat_ptr.to_raw_parts();

            //let pinned = Pin::new(Chunk::with_capacity(size));
            //

            let res = self
                .chunks[idx]
                // both are AcqRel because I don't have the mental capacity to figure out if
                // anything else is safe right now. Doesn't need SeqCst since threads only care
                // about their own load/store so no "3rd" thread is involved in any interaction
                // here
                .compare_exchange(null_mut(), ptr, std::sync::atomic::Ordering::AcqRel, std::sync::atomic::Ordering::AcqRel);
                //.compare_and_swap(null_mut(), ptr, std::sync::atomic::Ordering::Release);

            match res {
                Ok(p) => {
                    // we got a chance to write our ptr, no other thread did it faster
                    p
                },
                Err(old) => {
                    // we need to dealloc ourself since some other thread already wrote the value
                    unsafe {
                        let b = Box::from_raw(fat_ptr);
                        drop(b);
                    }

                    old
                }
            }
        } else {
            ptr
        };

        unsafe {
            Chunk::make_fat_ptr(r, self.lengths[idx]).as_ref().unwrap()
        }
    }

    fn try_insert_chunk<'c>(
        &self,
        chunk_idx: i32,
        chunk: &'c Chunk<T>,
        val: &mut Option<T>,
    ) -> Option<(AtomicVecIndex, &'c T)> {
        let idx = chunk.cur.fetch_add(1, std::sync::atomic::Ordering::AcqRel); // bump idx speculatively

        if idx >= chunk.size {
            None // couldn't insert into this chunk, size already wouldn't allow it
        } else {
            unsafe {
                let t = val.take().unwrap();

                // this is sound since because of the idx bump we can never
                // have an overlap in cells (no aliasing of actual cells) so long
                // as other guarantees are upheld for how we index into content
                let mchunk: &'c mut Chunk<T> = std::mem::transmute(chunk);

                mchunk.content[idx] = MaybeUninit::new(t);

                let aidx = AtomicVecIndex {
                    idx: idx as i32,
                    chunk: chunk_idx,
                    self_key: self.self_key,
                };
                let r = chunk.content[idx].assume_init_ref();

                Some((aidx, r))
            }
        }
    }

    unsafe fn try_insert_chunk_idx(
        &self,
        idx: usize,
        val: &mut Option<T>,
    ) -> Option<(AtomicVecIndex, &T)> {
        let chunk = self.get_chunk(idx);

        self.try_insert_chunk(idx as i32, chunk, val)
    }

    pub fn push(&self, e: T) -> (AtomicVecIndex, &T) {
        // don't need to check every chunk, can just start with current
        // chunk
        let mut chunk = self.cur.load(std::sync::atomic::Ordering::Acquire);

        let mut val = Some(e);

        while chunk < CHUNK_COUNT {
            unsafe {
                match self.try_insert_chunk_idx(chunk, &mut val) {
                    Some(avi) => return avi,
                    None => chunk += 1,
                }
            }
        }

        panic!("Exceeded AVec size constraint");
    }

    pub fn get(&self, idx: AtomicVecIndex) -> &T {
        if idx.self_key != self.self_key {
            panic!("Tried to use a token from another AVec on self, this could cause unsoundness!");
        } else {
            let chunk = self.get_chunk(idx.chunk as usize);
            unsafe {
                let t = chunk.content[idx.idx as usize].assume_init_ref();
                t
            }
        }
    }
}

pub struct AtomicVecIndex {
    // intentionally not public, should not be possible to construct this type externally
    chunk: i32,
    idx: i32,
    self_key: usize, // used to verify that this index came from the vec it is trying to index into
}

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
