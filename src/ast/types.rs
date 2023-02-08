use std::sync::RwLock;

//use dashmap::lock::RwLock;

use crate::{
    cst::{ScopedName, SyntacticTypeReference, SyntacticTypeReferenceInner, NodeInfo},
    helper::interner::IStr,
};

use crate::cst;

use super::tree::CtxID;

/// A TypeReference encloses an entire constraint,
/// including any `where` clauses or `+` composition
/// Each `+` is represented by an entry in `bases`
#[derive(Debug)]
pub struct TypeReference {
    pub bases: RwLock<Vec<TypeBase>>,
}

impl TypeReference {
    pub fn from_cst(
        cst: SyntacticTypeReference,
        generics: &Vec<(IStr, SyntacticTypeReference)>,
    ) -> Self {
        match cst.inner {
            SyntacticTypeReferenceInner::Single { name } => Self {
                bases: RwLock::new(vec![TypeBase::UnResolved(UnResolvedType {
                    from: cst.info,
                    named: name,
                    generics: vec![],
                })]),
            },
            _ => todo!("only handle simple non-generic types for now"),
        }
    }
}

impl Clone for TypeReference {
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
    from: NodeInfo,

    base: CtxID,

    /// May or may not yet be directly resolved
    generics: Vec<TypeReference>,
}

#[derive(Debug, Clone)]
pub struct UnResolvedType {
    /// the ID here is only unique within a single module (or, more specifically, a parse unit)
    /// and serves to allow going back and knowing which one to resolve after
    /// everything has been published/resolved
    //id: usize,

    from: NodeInfo,

    named: ScopedName,

    generics: Vec<TypeReference>,
}

#[derive(Debug)]
pub enum NodeReference {
    Unresolved(ScopedName),
    Resolved(CtxID),
}

#[derive(Debug)]
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
    info: cst::NodeInfo,
    name: IStr,

    parameters: Vec<(IStr, TypeReference)>,
    return_type: TypeReference,

    implementation: cst::expressions::ExpressionWrapper,
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

        let return_type = TypeReference::from_cst(return_type, &generics);

        let parameters = params
            .into_iter()
            .map(|(name, tr)| (name, TypeReference::from_cst(tr, &generics)))
            .collect();

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
