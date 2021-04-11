//use crate::ast::OuterScope;
use crate::ast::*;
use crate::helper::Error;
use chashmap::CHashMap;
//use crossbeam::unbounded;
//use std::collections::HashMap;
use std::sync::{Arc, RwLock, Weak};

use crate::helper::Interner::*;
use crate::StringSymbol;

/*pub struct SymbolDB<'a> {
    from: Arc<RwLock<OuterScope<'a>>>,
    pub symbols: Arc<RwLock<HashMap<ScopedName<'a>, Arc<RwLock<Declaration<'a>>>>>>,
}

impl<'a> SymbolDB<'a> {
}*/

#[derive(Debug)]
#[allow(dead_code)]
pub struct ScopeContext {
    scope: Vec<StringSymbol>,
    public: bool,
    from: Option<Arc<RwLock<SymbolDeclaration>>>,

    super_context: Option<Weak<RwLock<ScopeContext>>>,

    global_context: Option<Weak<RwLock<ScopeContext>>>,

    inner_contexts: CHashMap<StringSymbol, Arc<RwLock<ScopeContext>>>,

    exported_symbols: CHashMap<StringSymbol, Weak<RwLock<SymbolDeclaration>>>,

    imported_symbols: CHashMap<StringSymbol, Weak<RwLock<SymbolDeclaration>>>,

    defined_symbols: CHashMap<StringSymbol, Arc<RwLock<SymbolDeclaration>>>,

    error_sink: crossbeam::Sender<Error>,
}

use std::mem::drop;

impl AstNode for ScopeContext {
    fn node_info(&self) -> NodeInfo {
        match self.from.as_ref() {
            None => NodeInfo::Builtin,
            Some(ns_lock) => {
                let ns_guard = ns_lock.read().unwrap();
                ns_guard.as_node().node_info()
            }
        }
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(f, "{}ScopeContext at scope{:?}", indent(depth), self.scope);
        writeln!(f, "{}From:", indent(depth + 1)).unwrap();
        self.from
            .as_ref()
            .map(|from_lock| from_lock.read().unwrap().display(f, depth + 2));
        writeln!(f, "{}Inner contexts:", indent(depth + 1)).unwrap();
        self.inner_contexts
            .clone()
            .into_iter()
            .for_each(|(_, wref)| wref.read().unwrap().display(f, depth + 2));
        writeln!(f, "{}Exported symbols:", indent(depth + 1)).unwrap();
        self.exported_symbols
            .clone()
            .into_iter()
            .for_each(|(_, wref)| {
                wref.upgrade()
                    .unwrap()
                    .read()
                    .unwrap()
                    .display(f, depth + 2)
            });
        writeln!(f, "{}Imported symbols:", indent(depth + 1)).unwrap();
        self.imported_symbols
            .clone()
            .into_iter()
            .for_each(|(_, wref)| {
                wref.upgrade()
                    .unwrap()
                    .read()
                    .unwrap()
                    .display(f, depth + 2)
            });
        writeln!(f, "{}Defined symbols:", indent(depth + 1)).unwrap();
        self.defined_symbols
            .clone()
            .into_iter()
            .for_each(|(_, wref)| wref.read().unwrap().display(f, depth + 2));
    }
}

impl std::fmt::Display for ScopeContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display(f, 0);

        write!(f, "")
    }
}

impl ScopeContext {
    pub fn new(
        error_sink: crossbeam::Sender<Error>,
        scope: Vec<StringSymbol>,
        global: Option<Weak<RwLock<ScopeContext>>>,
        parent: Option<Weak<RwLock<ScopeContext>>>,
        from: Option<Arc<RwLock<SymbolDeclaration>>>,
    ) -> Arc<RwLock<ScopeContext>> {
        let scope = ScopeContext {
            scope,
            public: true,
            super_context: parent,
            global_context: global,
            exported_symbols: CHashMap::new(),
            imported_symbols: CHashMap::new(),
            defined_symbols: CHashMap::new(),
            inner_contexts: CHashMap::new(),
            error_sink,
            from: None,
        };

        let scope_locked = Arc::new(RwLock::new(scope));

        let mut scope_guard = scope_locked.write().unwrap();

        if let Some(ns) = from.as_ref() {
            let ns_guard = ns.read().unwrap();
            for dec in ns_guard.symbols() {
                scope_guard.add_definition(scope_locked.clone(), dec.clone());
            }
            /*if let Ok(outer) = ns_guard.contents.as_ref() {
                for dec in outer.declarations.iter() {
                    scope.add_definition(dec.clone());
                }
            }*/

            scope_guard.from = Some(ns.clone());
        }

        std::mem::drop(scope_guard);

        scope_locked
    }

    pub fn on_root(
        &mut self,
        self_rc: Arc<RwLock<ScopeContext>>,
        outer: &OuterScope,
    ) {
        for dec in outer.declarations.iter() {
            //let mut self_guard = self_rc.write().unwrap();
            self.add_definition(self_rc.clone(), dec.clone());
        }
    }

    pub fn add_context(
        &mut self,
        self_rc: Arc<RwLock<ScopeContext>>,
        ns: Arc<RwLock<SymbolDeclaration>>,
    ) {
        //let mut self_guard = self_rc.write().unwrap();

        let ctx_guard = ns.read().unwrap();

        let mut scope = self.scope.clone();
        //scope.push(String::from(ctx_guard.symbol_name().unwrap_or("")));
        //scope.push(interner().resolve(interner().get(ctx_guard.symbol_name().unwrap_or(&interner().get_or_intern_static("")))).to_owned());
        //scope.push(interner().get_or_intern(ctx_guard.symbol_name().unwrap_or()));
        scope.push(ctx_guard.symbol_name().unwrap_or(interner().get_or_intern_static("")));

        let error = self.error_sink.clone();

        let global = self.global_context.as_ref().unwrap().clone();

        let parent = self_rc.clone();

        let scope = Self::new(
            error,
            scope,
            Some(global),
            Some(Arc::downgrade(&parent)),
            Some(ns.clone()),
        );
        //let scope = Arc::new(RwLock::new(scope));

        self.add_child_context(scope);
    }

    pub fn add_definition(
        &mut self,
        self_rc: Arc<RwLock<ScopeContext>>,
        decl: Arc<RwLock<SymbolDeclaration>>,
    ) -> bool {
        let decl_guard = decl.read().unwrap();
        let is_exported = decl_guard.is_public();
        let sname = if let Some(sname) = decl_guard.symbol_name() {
            sname
        } else {
            println!("symbol declaration not implemented for unnamed symbols");
            return false;
        };

        let is_context = decl_guard.is_context();

        println!("adding a definition");
        println!("symbol name is {}", sname.resolve());

        std::mem::drop(decl_guard);

        let mut result = true;

        let nref = decl.clone();

        if self.defined_symbols.get(&sname).is_some() {
            let rg = self.defined_symbols.get(&sname).unwrap();
            println!("duplicate symbol detected: {}", interner().resolve(&sname));
            self.error_sink
                .send(Error::DuplicateDefinition {
                    duplicate_symbol: nref,
                    existing_symbol: rg.clone(),
                })
                .expect("channel closed before error could be sent");

            result = false;
        } else {
            println!("was not duplicate, so inserting symbol");
            if is_exported {
                self.exported_symbols
                    .insert(sname, Arc::downgrade(&nref))
                    .expect_none(
                        "should never be replacing an export, would be a duplicate symbol",
                    );
            }

            self.imported_symbols
                .insert(sname, Arc::downgrade(&nref)); // can silently replace to shadow here
            self.defined_symbols.insert(sname, nref);

            if is_context {
                self.add_context(self_rc.clone(), decl.clone());
            }
        }

        result
    }

    /*pub fn set_once_super(&mut self, super_context: Weak<RwLock<ScopeContext<'a>>>) {
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
    }*/

    pub fn add_child_context(&mut self, child: Arc<RwLock<ScopeContext>>) {
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

    //pub fn import(&mut self, _sn: ScopedName<'a>) {}
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
