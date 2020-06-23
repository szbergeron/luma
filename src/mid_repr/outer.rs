use std::sync::{Arc, RwLock};
use crate::ast::OuterScope;
use std::collections::HashMap;
use crate::ast::*;

pub struct SymbolDB<'a> {
    from: Arc<RwLock<OuterScope<'a>>>,
    pub symbols: Arc<RwLock<HashMap<ScopedName<'a>, Arc<RwLock<Declaration<'a>>>>>>,
}

impl<'a> SymbolDB<'a> {
}

/*pub struct FullyQualifiedScopedName<'a> {
    pub scope: Vec<&'a str>
}*/

pub enum Declaration<'a> {
    Function(FunctionDeclaration<'a>),
    Variable(LetExpression<'a>),
    Struct(StructDeclaration<'a>),
}
