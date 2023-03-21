use std::{sync::RwLock, collections::HashMap};

//use dashmap::lock::RwLock;

use crate::{
    cst::{NodeInfo, ScopedName, SyntacticTypeReferenceInner, SyntacticTypeReferenceRef},
    helper::interner::IStr,
};

use crate::cst;

use super::tree::CtxID;

//use itertoo

use itertools::Itertools;

/// A TypeReference encloses an entire constraint,
/// including any `where` clauses or `+` composition
/// Each `+` is represented by an entry in `bases`
#[derive(Debug)]
pub struct AbstractTypeReference {
    //pub id: AbstractTypeReferenceRef,
    pub inner: RwLock<TypeBase>,
}

impl AbstractTypeReference {
    pub fn from_cst(cst: SyntacticTypeReferenceRef, within_generic_context: &[IStr]) -> Self {
        let cst = cst.resolve().unwrap();
        match &cst.inner {
            SyntacticTypeReferenceInner::Single { name } => Self {
                //id: AbstractTypeReferenceRef::new_nil(),
                inner: RwLock::new(TypeBase::UnResolved(UnResolvedType {
                    from: cst.info,
                    named: name.clone(),
                    generics: vec![],
                })),
            },
            _ => todo!("only handle simple non-generic types for now"),
        }
    }
}

impl Clone for AbstractTypeReference {
    fn clone(&self) -> Self {
        Self {
            inner: RwLock::new(self.inner.read().unwrap().clone()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeBase {
    Generic(GenericType),

    Resolved(ResolvedType),

    UnResolved(UnResolvedType),
}

#[derive(Debug, Clone)]
pub struct GenericType {
    /// Gennerics aren't yet plumbed through
    ni: !,
}

#[derive(Debug, Clone)]
pub struct ResolvedType {
    pub from: NodeInfo,

    pub base: CtxID,

    /// May or may not yet be directly resolved
    pub generics: Vec<SyntacticTypeReferenceRef>,
}

#[derive(Debug, Clone)]
pub struct UnResolvedType {
    /// the ID here is only unique within a single module (or, more specifically, a parse unit)
    /// and serves to allow going back and knowing which one to resolve after
    /// everything has been published/resolved
    //id: usize,
    pub from: NodeInfo,

    pub named: ScopedName,

    pub generics: Vec<SyntacticTypeReferenceRef>,
}

#[derive(Debug, Clone)]
pub enum NodeReference {
    Unresolved(ScopedName),
    Resolved(CtxID),
}

#[derive(Debug, Clone)]
pub struct FieldMember {
    pub name: IStr,
    pub has_type: Option<SyntacticTypeReferenceRef>,
    pub initialization: Option<cst::expressions::ExpressionWrapper>,
}

#[derive(Debug)]
pub struct StructuralDataDefinition {
    pub fields: Vec<FieldMember>,
    pub methods: HashMap<IStr, CtxID>,
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub info: cst::NodeInfo,
    pub name: IStr,

    pub is_method: bool,

    pub parameters: Vec<(IStr, SyntacticTypeReferenceRef)>,
    pub return_type: SyntacticTypeReferenceRef,
    pub implementation: Option<cst::expressions::ExpressionWrapper>, // quark removes this to do
                                                                    // stuff on, drops it "early"
}

impl FunctionDefinition {
    pub fn from_cst(f: cst::FunctionDefinition) -> Self {
        let cst::FunctionDefinition {
            info,
            public,
            name,
            body,
            is_method,
            return_type,
            params,
            generics,
        } = f;

        let generic_names = generics.iter().map(|(name, ty)| *name).collect_vec();

        //let return_type = AbstractTypeReference::from_cst(return_type, generic_names.as_slice());
        //let return_type = OldTypeReference::Syntactic(return_type);

        let parameters = params
            .into_iter()
            //.map(|(name, tr)| (name, AbstractTypeReference::from_cst(tr, generic_names.as_slice())))
            .map(|(name, ty)| (name, ty))
            .collect();

        //[1, 2].into_iter().coll

        //body.to_abstractly_typed(generics.iter().map(|(s, _)| *s).collect_vec().as_slice());

        Self {
            info,
            name,
            is_method,
            implementation: Some(*body),
            return_type,
            parameters,
        }
    }
}

pub struct StructDefinition {
    generics: Vec<(IStr, SyntacticTypeReferenceRef)>,
    name: IStr,

    fields: Vec<(IStr, SyntacticTypeReferenceRef)>,
}
