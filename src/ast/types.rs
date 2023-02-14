use std::sync::RwLock;

//use dashmap::lock::RwLock;

use crate::{
    cst::{ScopedName, SyntacticTypeReferenceInner, NodeInfo, SyntacticTypeReferenceRef, TypeReference},
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

    pub bases: RwLock<Vec<TypeBase>>,
}

impl AbstractTypeReference {
    pub fn from_cst(
        cst: SyntacticTypeReferenceRef,
        within_generic_context: &[IStr],
    ) -> Self {
        let cst = cst.resolve().unwrap();
        match &cst.inner {
            SyntacticTypeReferenceInner::Single { name } => Self {
                //id: AbstractTypeReferenceRef::new_nil(),
                bases: RwLock::new(vec![TypeBase::UnResolved(UnResolvedType {
                    from: cst.info,
                    named: name.clone(),
                    generics: vec![],
                })]),
            },
            _ => todo!("only handle simple non-generic types for now"),
        }
    }

}

impl Clone for AbstractTypeReference {
    fn clone(&self) -> Self {
        Self { bases: RwLock::new(self.bases.read().unwrap().clone()) }
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
    pub generics: Vec<TypeReference>,
}

#[derive(Debug, Clone)]
pub struct UnResolvedType {
    /// the ID here is only unique within a single module (or, more specifically, a parse unit)
    /// and serves to allow going back and knowing which one to resolve after
    /// everything has been published/resolved
    //id: usize,

    pub from: NodeInfo,

    pub named: ScopedName,

    pub generics: Vec<TypeReference>,
}

#[derive(Debug, Clone)]
pub enum NodeReference {
    Unresolved(ScopedName),
    Resolved(CtxID),
}

#[derive(Debug, Clone)]
pub struct FieldMember {
    pub name: IStr,
    pub has_type: Option<TypeReference>,
    pub initialization: Option<cst::expressions::ExpressionWrapper>,
}

#[derive(Debug)]
pub struct StructuralDataDefinition {
    pub fields: Vec<FieldMember>,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub info: cst::NodeInfo,
    pub name: IStr,

    pub parameters: Vec<(IStr, TypeReference)>,
    pub return_type: TypeReference,

    pub implementation: cst::expressions::ExpressionWrapper,
}

impl FunctionDefinition {
    pub fn from_cst(f: cst::FunctionDefinition) -> Self {
        let cst::FunctionDefinition {
            info,
            public,
            name,
            body,
            return_type,
            params,
            generics,
        } = f;

        let generic_names = generics.iter().map(|(name, ty)| *name).collect_vec();

        //let return_type = AbstractTypeReference::from_cst(return_type, generic_names.as_slice());
        let return_type = TypeReference::Syntactic(return_type);

        let parameters = params
            .into_iter()
            //.map(|(name, tr)| (name, AbstractTypeReference::from_cst(tr, generic_names.as_slice())))
            .map(|(name, ty)| (name, TypeReference::Syntactic(ty)))
            .collect();

        //[1, 2].into_iter().coll

        //body.to_abstractly_typed(generics.iter().map(|(s, _)| *s).collect_vec().as_slice());

        Self {
            info,
            name,
            implementation: *body,
            return_type,
            parameters,
        }
    }
}

pub struct StructDefinition {
    generics: Vec<(IStr, TypeReference)>,
    name: IStr,

    fields: Vec<(IStr, TypeReference)>,
}
