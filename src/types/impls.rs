use super::{CtxID, FunctionImplementation, TypeCtx, TypeHandle, TypeID, GlobalTypeID};
use crate::ast::Span;

use smallvec::SmallVec;
use std::any::Any;

/// Acknowledgements:
///
/// https://users.rust-lang.org/t/workaround-for-hash-trait-not-being-object-safe/53332/3
///     For implementation hints on construction of hashable Any+Type instances
mod acks {}

/// Realistically very few people will ever use a type with more than
/// this many parameters, so optimizing for this case seems obvious.
///
/// This number can be tweaked later on, and may be reduced to 2 if
/// performance profiling indicates benefit
const TYPE_PARAM_DEFAULT_COUNT: usize = 3;
const FUNCTION_PARAM_DEFAULT_COUNT: usize = 3;

mod type_helpers {
    #[allow(dead_code)]
    pub fn type_id_default() -> super::TypeID {
        super::TypeID(std::u64::MAX)
    }
}

pub trait AsAny {
    fn as_any(&self) -> &dyn Any;
}

impl<T: Any> AsAny for T {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct TypeSignature {
    name: String,
    params: SmallVec<[Option<GlobalTypeID>; TYPE_PARAM_DEFAULT_COUNT]>,
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct FunctionSignature {
    name: String,
    params: SmallVec<[GlobalTypeID; FUNCTION_PARAM_DEFAULT_COUNT]>,
}

pub trait Type: DynHash + DynEq + AsAny {
    fn set_tid(&self, tid: TypeID);
    fn canonicalized_name(&self, within: &TypeCtx) -> &str;
    /*// owned for simpler lifetimes,
    // shouldn't be used often, doesn't appear in src text
    canonicalized_string: String,

    supers: Vec<Arc<RwLock<Type>>>,

    implementation_blocks: Vec<Span>,
    definition_blocks: Vec<Span>,

    is_value_type: bool,

    // */
    //fn supers(&self) -> &[TypeHandle];

    //fn implementation_blocks(&self) -> &[Span];

    fn definition_blocks(&self) -> &[Span];

    fn is_reference_type(&self) -> bool;

    fn is_value_type(&self) -> bool {
        return !self.is_reference_type();
    }

    /// Some(TypeID) if this type allows deref to a type,
    /// with the contents of the Some(_) being the type it derefs to.
    ///
    /// If the type does not deref, returns None
    fn derefs_to(&self) -> Option<TypeID> {
        None
    }

    fn uid(&self) -> TypeID;

    fn encode_reference(&self, within: &TypeCtx) -> String;

    fn encode_definition(&self) -> String;

    fn add_method(&self, method: FunctionImplementation) -> bool;

    //fn dyn_hash<H: std::hash::Hasher>(&self, state: &mut H);
    //fn dyn_hash(&self);

    //fn dyn_eq(&self, other: &dyn Type) -> bool;
}

/*impl std::hash::Hash for dyn Type {
}*/

pub trait DynHash {
    fn dyn_hash(&self, state: &mut dyn std::hash::Hasher);
}

impl<T: std::hash::Hash + ?Sized> DynHash for T {
    fn dyn_hash(&self, mut state: &mut dyn std::hash::Hasher) {
        self.hash(&mut state);
    }
}

pub trait DynEq {
    fn dyn_eq(&self, other: &dyn Any) -> bool;
}

impl<T: Eq + Any> DynEq for T {
    fn dyn_eq(&self, other: &dyn Any) -> bool {
        if let Some(other) = other.downcast_ref::<Self>() {
            self == other
        } else {
            false
        }
    }
}

impl PartialEq for dyn Type {
    fn eq(&self, other: &dyn Type) -> bool {
        DynEq::dyn_eq(self, other.as_any())
    }
}

impl Eq for dyn Type {}

//impl DynHash for Type

/*impl std::hash::Hash for dyn Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        ${0:todo!()}
    }
}*/

#[allow(non_camel_case_types)]
pub struct i32_t_static {
    pub tid: std::sync::atomic::AtomicU64,
}

impl Type for i32_t_static {
    fn canonicalized_name(&self, _: &TypeCtx) -> &str {
        "i32"
    }

    /*fn supers(&self) -> &[TypeHandle] {
        &[]
    }*/

    fn is_reference_type(&self) -> bool {
        false
    }

    fn definition_blocks(&self) -> &[Span] {
        &[]
    }

    /*fn implementation_blocks(&self) -> &[Span] {
        &[]
    }*/

    fn encode_reference(&self, _: &TypeCtx) -> String {
        "<llvm 32 bit int>".to_owned()
    }

    fn encode_definition(&self) -> String {
        "\n".to_owned()
    }

    fn uid(&self) -> super::ctx::TypeID {
        TypeID(self.tid.load(std::sync::atomic::Ordering::SeqCst))
    }

    fn add_method(&self, _method: super::FunctionImplementation) -> bool {
        todo!()
    }

    fn set_tid(&self, tid: TypeID) {
        //self.tid = tid;
        // TODO: check if this can use different ordering restriction, if this is not done
        // after init then may be possible to just use Ordering::Release during
        // set here
        self.tid.store(tid.0, std::sync::atomic::Ordering::SeqCst);
    }
}

impl std::cmp::PartialEq for i32_t_static {
    fn eq(&self, other: &Self) -> bool {
        self.uid() == other.uid()
    }
}

impl std::hash::Hash for i32_t_static {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.uid().hash(state);
    }
}

impl std::cmp::Eq for i32_t_static {}

pub struct ProductType {}

#[allow(non_camel_case_types)]
pub struct ref_t_static {
    pub value_t: Option<TypeID>,
    pub collapsed_canon_name: once_cell::sync::OnceCell<String>,
    pub ctxid: CtxID,
    //pub tid: once_cell::sync::OnceCell<TypeID>,
    pub tid: std::sync::atomic::AtomicU64,
}

impl std::hash::Hash for ref_t_static {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        //state.write(self.value_t)
        self.value_t.hash(state);
        std::any::TypeId::of::<Self>().hash(state);
    }
}

impl std::cmp::PartialEq for ref_t_static {
    fn eq(&self, other: &Self) -> bool {
        self.value_t == other.value_t
    }
}

impl std::cmp::Eq for ref_t_static {}

impl ref_t_static {
    fn build_canon_name(&self, within: &TypeCtx) {
        self.collapsed_canon_name
            .set(
                "&".to_owned()
                    + self
                        .value_t
                        .map(|tid| within.lookup(tid))
                        .flatten()
                        .map(|type_handle| type_handle.canonicalized_name(within).to_owned())
                        //+ self
                        //.map_inner_rw(within, |t: &mut dyn Type| t.canonicalized_name(within).to_owned())
                        .unwrap_or("<unknown>".to_owned())
                        .as_str(),
            )
            .unwrap_or_else(|_| {
                // not fatal if set already, this is just a caching op
            });
    }

    /*fn map_inner_rw<F, T>(&self, within: &TypeCtx, func: F) -> Option<T> where F: Fn(&mut dyn Type) -> T {
        self.value_t.map(|tid| {
            let lookup = within.lookup(tid).expect("apply_inner_rw was given an invalid value_t tid");
            let mut guard = lookup.write().expect("Couldn't lock write lookup on inner value type");
            let v = func(&mut *guard);
            v
        })
    }

    fn map_inner_r<F, T>(&self, within: &TypeCtx, func: F) -> Option<T> where F: Fn(& dyn Type) -> T {
        self.value_t.map(|tid| {
            let lookup = within.lookup(tid).expect("apply_inner_rw was given an invalid value_t tid");
            let guard = lookup.read().expect("Couldn't lock write lookup on inner value type");
            let v = func(& *guard);
            v
        })
    }*/

    /*fn get_inner_rw<'a>(&self, within: &Ctx<'a>) -> Option<std::sync::RwLockWriteGuard<dyn Type>> {
        match self.value_t {
            None => None,
            Some(tid) => Some(within
                .lookup(tid)
                .expect("Type held an invalid tid for value type")
                .write()
                .expect("Couldn't lock write lookup on inner value type"))
        }
    }*/

    /*fn get_inner_r(&self, within: &Ctx) -> Option<std::sync::RwLockReadGuard<dyn Type + 'static>> {
        self.value_t.map(|tid| {
            within
                .lookup(tid)
                .expect("Type held an invalid tid for value type")
                .read()
                .expect("Couldn't lock write lookup on inner value type")
        })
    }*/

    #[allow(dead_code)]
    fn new(tid_inner: TypeID) -> Box<ref_t_static> {
        Box::new(ref_t_static {
            ctxid: CtxID(std::u64::MIN),
            value_t: Some(tid_inner),
            //tid: once_cell::sync::OnceCell::from(super::ctx::generate_typeid()),
            tid: std::sync::atomic::AtomicU64::from(super::ctx::generate_typeid().0),
            collapsed_canon_name: once_cell::sync::OnceCell::default(),
        })
    }
}

impl Type for ref_t_static {
    fn canonicalized_name(&self, within: &TypeCtx) -> &str {
        self.build_canon_name(within);

        self.collapsed_canon_name
            .get()
            .as_ref()
            .map(|s| s)
            .expect("build_canon_name didn't populate inner canon name?")
    }

    /*fn supers(&self) -> &[TypeHandle] {
        todo!()
    }*/

    /*fn implementation_blocks(&self) -> &[Span] {
        todo!()
    }*/

    fn definition_blocks(&self) -> &[Span] {
        todo!()
    }

    fn is_reference_type(&self) -> bool {
        true
    }

    fn uid(&self) -> super::ctx::TypeID {
        //self.tid.get().expect("TID was unset")
        TypeID(self.tid.load(std::sync::atomic::Ordering::SeqCst))
    }

    fn encode_reference(&self, within: &TypeCtx) -> String {
        /*self.map_inner_r(within, |t| t.encode_reference(within).to_owned())
            .expect("No inner type present when trying to encode reference")
            + "*"*/
        //let t = within.lookup(self.value_t).expect("Can't encode reference, inner type does not exist");
        let t = self.value_t.map(|id| within.lookup(id)).flatten().expect("Can't encode reference, inner type does not exist");
        t.encode_reference(within) + "*"
        //self.value_t.as_ref().expect("Tried to encode a reference to a non-existant type").encode_reference(within) + "*"
    }

    /// Definition is implicit within llvm,
    /// as references can act simply as
    fn encode_definition(&self) -> String {
        "\n".to_owned()
    }

    fn add_method(&self, _method: super::FunctionImplementation) -> bool {
        todo!()
    }

    fn derefs_to(&self) -> Option<TypeID> {
        self.value_t
    }

    fn set_tid(&self, tid: TypeID) {
        //self.tid = tid;
        self.tid.store(tid.0, std::sync::atomic::Ordering::SeqCst);
    }
}
