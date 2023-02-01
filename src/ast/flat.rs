use crate::{avec::AtomicVec, cst::IntoCstNode};
use crate::cst::ExpressionWrapper;

use super::expressions::AnyExpression;

/// a Context represents
/// a full scope for an expression, lowered into
/// an n-heap with all expressions flattened
pub struct Ctx {
    pub expressions: AtomicVec<AnyExpression>,
}

impl Ctx {
    pub fn insert(&self, e: crate::cst::ExpressionWrapper) {
        match e {
            ExpressionWrapper::Assignment(e) => {
            },
            ExpressionWrapper::BinaryOperation(_) => todo!(),
            ExpressionWrapper::UnaryOperation(_) => todo!(),
            ExpressionWrapper::Comparison(_) => todo!(),
            ExpressionWrapper::Cast(_) => todo!(),
            ExpressionWrapper::Literal(_) => todo!(),
            ExpressionWrapper::MemberAccess(_) => todo!(),
            ExpressionWrapper::Statement(_) => todo!(),
            ExpressionWrapper::Block(_) => todo!(),
            ExpressionWrapper::IfThenElse(_) => todo!(),
            ExpressionWrapper::While(_) => todo!(),
            ExpressionWrapper::LetExpression(_) => todo!(),
            ExpressionWrapper::Tuple(_) => todo!(),
            ExpressionWrapper::Return(_) => todo!(),
            ExpressionWrapper::Wildcard(_) => todo!(),
            ExpressionWrapper::LLVMLiteral(_) => todo!(),
            ExpressionWrapper::Identifier(_) => todo!(),
            ExpressionWrapper::FunctionCall(_) => todo!(),
            ExpressionWrapper::ImplementationModification(_) => todo!(),
            ExpressionWrapper::DynamicMember(_) => todo!(),
        }
    }
}
