pub mod base;
pub mod expressions;
pub mod outer;
mod types;

pub use base::*;
pub use expressions::*;
pub use outer::*;
pub use types::*;

pub trait AstDisplay {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize);
}

pub trait DynExpression {}
//pub use crate::types::*;
