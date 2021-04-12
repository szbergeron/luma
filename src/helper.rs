//use crate::lex;
use crate::ast::*;
use crate::helper::Interner::*;
//use atomic_option::AtomicOption;
use lazy_static::lazy_static;
//use lock_api::RawRwLockRecursive;
use std::fs;
use std::path::PathBuf;
use std::sync::{Arc, RwLock, Weak};
use std::borrow::Borrow;

pub mod Interner {
    pub type StringSymbol = lasso::LargeSpur;

    pub enum Rodeo {
        ThreadedRodeo(lasso::ThreadedRodeo<StringSymbol>),
        RodeoReader(lasso::RodeoReader<StringSymbol>),
        RodeoResolver(lasso::RodeoResolver<StringSymbol>),
    }
    //static mut INTERNER_PRIV: Option<Rodeo> = None;
    //static mut INTERNER_UNLOCKED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);
    lazy_static! {
        static ref INTERNER_OWNING: std::sync::Mutex<Option<std::sync::Arc<std::sync::RwLock<Rodeo>>>> = std::sync::Mutex::new(None);
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
                            std::mem::transmute(ig)
                        );
                    }

                    &**inner.as_ref().expect("Just set inner to Some, but was None")
                });

                InternerReadGuard {
                    rodeo: r,
                }
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
                    *val += 1;
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

        let mut internal = INTERNER_OWNING.lock().expect("Couldn't lock outer mutex for interner");
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
pub type PathIdMapHandle<'context> = Arc<RwLock<PathIdMap>>;

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
    global_context: Option<Arc<RwLock<ScopeContext>>>,
    scopes: Vec<Arc<RwLock<ScopeContext>>>,
}

impl ScopeIdMap {
    pub fn new() -> ScopeIdMap {
        ScopeIdMap {
            scopes: Vec::new(),
            global_context: None,
        }
    }

    pub fn handles(&self) -> &[Arc<RwLock<ScopeContext>>] {
        &self.scopes[..]
    }

    pub fn handles_mut(&mut self) -> &mut [Arc<RwLock<ScopeContext>>] {
        &mut self.scopes[..]
    }

    pub fn set_global(&mut self, global: Arc<RwLock<ScopeContext>>) {
        self.global_context = Some(global);
    }

    pub fn global(&self) -> Option<Arc<RwLock<ScopeContext>>> {
        self.global_context.clone()
    }

    pub fn push_scope(&mut self, scope: Arc<RwLock<ScopeContext>>) {
        self.scopes.push(scope);
    }
    //
}

impl<'context> PathIdMap {
    pub fn new_locked() -> Arc<RwLock<PathIdMap>> {
        Arc::new(RwLock::new(Self::new()))
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
    use crate::helper::Interner::*;
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

    #[derive(Debug, Clone, Copy)]
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

    #[derive(Debug, Clone, Copy)]
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

    impl LookaheadStream {
        pub fn new(w: &mut Wrapper) -> LookaheadStream {
            let mut v = Vec::new();
            let mut comment_level = 0;
            let mut inside_line_comment = false;
            while let Ok(tw) = w.next() {
                // handle comments
                use crate::lex::Token;
                match tw.token {
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

        pub fn la(&self, offset: isize) -> ParseResult {
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
