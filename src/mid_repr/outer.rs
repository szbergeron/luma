//use crate::ast::OuterScope;
use crate::ast::*;
use crate::helper::Error;
use chashmap::CHashMap;
//use crossbeam::unbounded;
//use std::collections::HashMap;
use std::sync::{Arc, RwLock, Weak};

/*pub struct SymbolDB<'a> {
    from: Arc<RwLock<OuterScope<'a>>>,
    pub symbols: Arc<RwLock<HashMap<ScopedName<'a>, Arc<RwLock<Declaration<'a>>>>>>,
}

impl<'a> SymbolDB<'a> {
}*/

#[allow(dead_code)]
pub struct ScopeContext<'a> {
    scope: Vec<String>,
    public: bool,
    super_context: Option<Weak<RwLock<ScopeContext<'a>>>>,

    global_context: Option<Weak<RwLock<ScopeContext<'a>>>>,

    inner_contexts: CHashMap<String, Arc<RwLock<ScopeContext<'a>>>>,

    exported_symbols: CHashMap<String, Weak<RwLock<SymbolDeclaration<'a>>>>,

    imported_symbols: CHashMap<String, Weak<RwLock<SymbolDeclaration<'a>>>>,

    defined_symbols: CHashMap<String, Arc<RwLock<SymbolDeclaration<'a>>>>,

    error_sink: crossbeam::Sender<Error<'a>>,
}

use std::mem::drop;

impl<'a> ScopeContext<'a> {
    pub fn new(
        error_sink: crossbeam::Sender<Error<'a>>,
        scope: Vec<String>,
        global: Option<Weak<RwLock<ScopeContext<'a>>>>,
        parent: Option<Weak<RwLock<ScopeContext<'a>>>>,
    ) -> ScopeContext<'a> {
        ScopeContext {
            scope,
            public: true,
            super_context: parent,
            global_context: global,
            exported_symbols: CHashMap::new(),
            imported_symbols: CHashMap::new(),
            defined_symbols: CHashMap::new(),
            inner_contexts: CHashMap::new(),
            error_sink,
        }
    }

    pub fn add_definition(&mut self, d: SymbolDeclaration<'a>) -> bool {
        let is_exported = d.is_public();
        let sname = d
            .symbol_name()
            .expect("No support for unnamed symbol declarations currently");

        let mut result = true;

        let nref = Arc::new(RwLock::new(d));
        if let Some(rg) = self.defined_symbols.get(sname) {
            self.error_sink
                .send(Error::DuplicateDefinition {
                    duplicate_symbol: nref,
                    existing_symbol: rg.clone(),
                })
                .expect("channel closed before error could be sent");

            result = false;
        } else {
            if is_exported {
                self.exported_symbols
                    .insert(String::from(sname), Arc::downgrade(&nref))
                    .expect_none(
                        "should never be replacing an export, would be a duplicate symbol",
                    );
            }
            self.imported_symbols
                .insert(String::from(sname), Arc::downgrade(&nref)); // can silently replace to shadow here
            self.defined_symbols.insert(String::from(sname), nref);
        }

        result
    }

    pub fn set_once_super(&mut self, super_context: Weak<RwLock<ScopeContext<'a>>>) {
        if self.super_context.is_some() {
            panic!("Super context is already some");
        } else {
            self.super_context = Some(super_context);
        }
    }

    pub fn set_once_global(&mut self, global_context: Weak<RwLock<ScopeContext<'a>>>) {
        if self.global_context.is_some() {
            panic!("Global context is already some");
        } else {
            self.global_context = Some(global_context);
        }
    }

    pub fn add_child_context(&mut self, child: Arc<RwLock<ScopeContext<'a>>>) {
        let child_guard = child.read().unwrap();
        let last_string = child_guard
            .scope
            .last()
            .expect("child had no last scope string")
            .clone();
        drop(child_guard);

        self.inner_contexts.insert(last_string, child);
    }

    //pub fn get_weakref(&self,

    pub fn import(&mut self, _sn: ScopedName<'a>) {}
}

/*pub struct FullyQualifiedScopedName<'a> {
    pub scope: Vec<&'a str>
}*/

/*pub enum Declaration<'a> {
    Namespace(ScopeContext<'a>),
    Function(FunctionDeclaration<'a>),
    Variable(LetExpression<'a>),
    Struct(StructDeclaration<'a>),
}*/
