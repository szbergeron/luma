pub mod base;
pub mod outer;
pub mod types;
pub mod expressions;

pub use base::*;
pub use outer::*;
pub use types::*;
pub use expressions::*;

pub trait AstDisplay {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize);
}

pub trait DynExpression {
}
