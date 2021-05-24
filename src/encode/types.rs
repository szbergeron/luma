pub struct Variable {
    pub vname: String,
    pub vtype: String,
    pub vtid: Option<crate::types::TypeID>,
}

impl Variable {
    pub fn phi_local(&self, other: &Variable, within: &mut EncodeLocalContext) -> Variable {
        assert!(self.vtype == other.vtype);

        within.writeln(format!("; phi node with variables {} and {}", self.vname, other.vname));

        let new_var = within.next_local("phi_local_", &self.vtype);

        //within.writeln(format!("phi 
        todo!("Phi nodes not yet complete")
    }
}

/*mod Tools {
    thread_local! {
        static LOCAL_CTX: Option<super::EncodeLocalContext<'static>> = None;
    }

    static GLOBAL_CTX: std::sync::Mutex<Option<super::EncodeGlobalContext>> = std::sync::Mutex::new(None);

    pub fn get_local_ctx() -> &'static mut super::EncodeLocalContext<'static> {
        todo!()
    }

    pub fn get_global_ctx() -> &'static mut super::EncodeGlobalContext {
        todo!()
    }
}*/

// !Sync, !Send
pub struct EncodeLocalContext<'gbl_ctx> {
    local_counter: usize,
    global_context: &'gbl_ctx EncodeGlobalContext,
    //contents: std::cell::UnsafeCell<Vec<String>>,
    contents: Vec<String>,
}

impl<'gbl_ctx> EncodeLocalContext<'gbl_ctx> {
    pub fn new(egctx: &EncodeGlobalContext) -> EncodeLocalContext {
        EncodeLocalContext {
            local_counter: 0,
            global_context: egctx,
            contents: Vec::new()
        }
    }

    /// flushes the contents of this context into the global context.
    /// only use when the ordering of the current block compared to any future pushed blocks
    /// is not sequentially important, and when the llvm parser will be in a "global context"
    /// state, not within any structure parse
    pub fn flush(&mut self) {
        self.global_context.swap_and_flush(&mut self.contents);
    }

    pub fn writeln<T: Into<String>>(&mut self, line: T) {
        // TODO: refactor to remove unsafe/unsafecell if actually decide against using global
        // refs for EncodeLocalContext
        self.contents.push(line.into());
    }

    pub fn write<T: Into<String>>(&mut self, line: T) {
        match self.contents.len() {
            0 => {
                self.contents.push(line.into());
            },
            _more => {
                self.contents.last_mut().expect("self.contents was size > 0 but last_mut returned None").push_str(&line.into()[..]);
            }
        }
    }

    pub fn next_local(&mut self, prefix: &str, vtype: &str) -> Variable {
        //let next_id = self.local_counter.

        self.local_counter += 1;

        let vname = format!("{}global_{}", prefix, self.local_counter);
        let vtype = vtype.to_string();

        Variable { vname, vtype, vtid: None }
    }

    pub fn next_global(&self, prefix: &str, vtype: &str) -> Variable {
        self.global_context.next_global(prefix, vtype)
    }
}

pub struct EncodeGlobalContext {
    global_counter: std::sync::atomic::AtomicU64,
    buffer: std::sync::Mutex<Vec<String>>,
}

impl EncodeGlobalContext {
    pub fn new() -> EncodeGlobalContext {
        EncodeGlobalContext {
            global_counter: std::sync::atomic::AtomicU64::new(0),
            buffer: std::sync::Mutex::new(Vec::new()),
        }
    }

    /// Takes contents of the given ibuffer of llvm line strings and appends them atomically
    /// to the global string buffer.
    pub fn swap_and_flush(&self, ibuffer: &mut Vec<String>) {
        self.buffer.lock().map(|mut guard| {
            guard.append(ibuffer);
        }).map_err(|_err| {
            panic!("Could not lock EncodeGlobalContext buffer guard, something must have panicked while lock was held. See err: {:?}", _err)
        }).unwrap();
    }

    pub fn next_global(&self, prefix: &str, vtype: &str) -> Variable {
        // change to Ordering::SeqCst for monotonically increasing atomic ops per-thread? Not sure
        // if necessary for that to occur, Relaxed may be sufficient (and will definitely provide
        // at least *unique* IDs
        let next_id = self.global_counter.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        let vname = format!("{}global_{}", prefix, next_id);
        let vtype = vtype.to_string();

        Variable { vname, vtype, vtid: None }
    }
}

impl std::fmt::Display for EncodeGlobalContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "; encode output start")?;
        let l = self.buffer.lock().expect("EncodeGlobalContext couldn't lock internal buffer during output");

        for line in l.iter() {
            writeln!(f, "{}", line)?;
        }

        writeln!(f, "; encode output end")?;

        Ok(())
    }
}
