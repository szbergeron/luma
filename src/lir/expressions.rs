use std::{rc::Rc, sync::atomic::AtomicUsize};

use crate::{
    helper::interner::{IStr, Internable},
    mir::{expressions::VarID, quark::ResolvedType},
};

pub enum Lower {
    Goto(Label),

    /// call (b) with (c) and store the result into (a)
    /// (a) is an rval, not an lval!
    Call(Variable, Label, Vec<Variable>),

    /// Deref (a) into (b)
    Deref(Variable, Variable),

    /// Take a ptr to (a) and store it in (b)
    PtrTo(Variable, Variable),

    /// Emit a label at the current point, starting a BB
    Label(Label),

    /// Store
    BoxOf(Variable, Variable),

    /// (load_from, fname, store_into)
    /// load_from should be a ptr type, store_into should be a pointer type,
    /// basically does a GEP
    Field(Variable, IStr, Variable),

    Branch(Variable, Label, Label),

    Phi(Vec<(Label, Variable)>),

    Return(Variable),
}

#[derive(Clone)]
pub struct Variable {
    name: IStr,
    typed: Rc<VarType>,
}

#[derive(Copy, Clone)]
pub struct UntypedVar {
    name: IStr,
}

static GEN: AtomicUsize = AtomicUsize::new(1);

impl UntypedVar {
    pub fn temp() -> Self {
        let id = GEN.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        Self {
            name: format!("temp_{id}").intern(),
        }
    }

    pub fn from(v: VarID) -> Self {
        let id = v.0;

        Self {
            name: format!("var_{id}").intern(),
        }
    }
}

impl std::fmt::Display for UntypedVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)
    }
}

impl Variable {
    pub fn of(name: IStr, ty: Rc<VarType>) -> Self {
        Self { name, typed: ty }
    }

    pub fn temp(ty: Rc<VarType>) -> Self {
        todo!()
    }
}

#[derive(Clone)]
pub enum VarType {
    Ptr(Rc<VarType>),
    Just(IStr),
}

impl VarType {
    pub fn deref(&self) -> Rc<Self> {
        match self {
            Self::Ptr(v) => v.clone(),
            Self::Just(v) => Rc::new(VarType::Just(*v)),
        }
    }

    pub fn ptrto(self: &Rc<Self>) -> Rc<Self> {
        Rc::new(VarType::Ptr(self.clone()))
    }

    pub fn just(t: ResolvedType) -> Rc<Self> {
        todo!()
    }
}

pub struct Label {
    name: IStr,
}

impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)
    }
}
