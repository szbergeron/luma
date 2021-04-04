use std::sync::Arc;
use std::sync::RwLock;
use crate::ast::Span;
use super::impls::*;
use dashmap::DashMap;

pub type TypeHandle = Arc<RwLock<dyn Type>>;

pub type TypeID = u64;
pub type CtxID = u64;

pub struct Method {
    return_type: TypeID,
    param_types: Vec<TypeID>,
    param_names: Vec<String>,
}

/*impl Method {
    fn returns(&self) -> TypeID {
        self.return_type
    }

    fn param_names(&self) -> &[String] {
        self.param_names
    }

    fn param_types(&self) -> &[TypeID] {
        self.param_types
    }

    fn name(&self) -> &str {
    }

    /*fn encode_reference(&self, lvals: &[VariableReference]) -> &str {
    }*/
}*/

pub fn generate_typeid() -> TypeID {
    static GENERATOR: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);
    GENERATOR.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

/// Interior mutable container representing a type context
pub struct Ctx<'input> {
    types: DashMap<TypeSignature<'input>, Box<dyn Type>>,
}

impl<'input> Ctx<'input> {
    pub fn define(&self, newtype: Box<dyn Type>) -> TypeID {
        todo!()
    }

    pub fn implement(&self, /* handle */) {}

    pub fn mix(&self, /* handle 1, handle 2 */) -> TypeID {
        todo!()
    }

    //pub fn inherit(&self, /* handle src, handle dst */) {}

    pub fn query(&self, canonicalized_string: String) -> TypeHandle {
        panic!("not implemented")
    }

    pub fn lookup(&self, tid: TypeID) -> Option<TypeHandle> {
        todo!()
    }

    pub fn rlookup(&self, t: &TypeSignature<'input>) -> Option<TypeID> {
        todo!()
    }
}

//impl Send for CtxInner {} // should this have send?
unsafe impl<'input> Sync for Ctx<'input>{}

//pub type Ctx = Arc<CtxInner>;
