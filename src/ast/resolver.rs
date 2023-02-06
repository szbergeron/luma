//! This module is responsible for handling imports and type references at a "surface level"
//!
//! It doesn't do type inference or anything "fancy" like that, it simply
//! tries its best to resolve references and detect any import loops
//!
//! Eventually, to handle recursive or deadlocking imports,
//! we can add a watchdog that checks if everyone is sleeping
//! for a significant period of time

use std::{
    collections::{HashMap, VecDeque},
    thread::panicking,
};

use crate::{
    cst::UseDeclaration,
    helper::{
        interner::{IStr, Internable, SpurHelper},
        SwapWith,
    },
};

use super::tree::{Contexts, CtxID};

struct ResolverWorker {
    /// we are solely responsible
    /// for dealing with the nodes within our domain.
    ///
    /// domains try to be clustered so that individual workers
    /// don't trample over each other, and so that
    /// when they "talk" to each other it is at a logical boundary at a parent spot in the tree
    domain: Vec<CtxID>,
    //waiting_questions: HashMap<>
}

/// A PostalWorker handles sending messages between Resolvers :)
struct Postal {}

impl Postal {
    pub fn send_reply(&self, from: CtxID, to: CtxID, reply: Reply) {}

    pub fn send_ask(&self, from: CtxID, to: CtxID, request: Request) {}

    pub fn instance() -> &'static Postal {
        todo!()
    }
}

enum Request {
    AskFor {
        reply_to: CtxID,
        conversation: usize,
        symbol: IStr,
        within: CtxID,
    },
}

enum Reply {
    /// If a symbol is exported by a node, then it must itself
    /// be a node, and this says what node it was found at
    ///
    /// Conversation is only unique for the "other end", the starting party
    Redirect {
        reply_from: CtxID,
        conversation: usize,
        symbol: IStr,
        within: CtxID,
        is_at: CtxID,
    },

    /// If a symbol is not exported by a node at all, then a NotFound is
    /// sent back to the asker
    NotFound {
        reply_from: CtxID,
        conversation: usize,
        symbol: IStr,
        within: CtxID,
    },
}

struct ImportError {
    symbol_name: IStr,
    error_reason: String,
}

struct Resolver {
    self_ctx: CtxID,

    convo_id_gen: usize,

    /// When we send out a question, we store the
    /// rest of the scope here that will need to be resolved when we
    /// get the next reply for a given conversation
    ///
    /// When we get a reply, we can reuse the existing
    /// conversation or delete and create a new one
    waiting_to_resolve: HashMap<usize, Vec<IStr>>,

    /// For every symbol that we try to export,
    /// there will be one entry in this hashmap. If we haven't
    /// yet resolved the export yet, then the entry is (name, None).
    /// If we have resolved it to something definite, the entry is
    /// (name, Some(Ok(ID))). If we failed to resolve it (the
    /// import that was exported couldn't be resolved) then the entry is
    /// (name, Some(Err()))
    publishes: HashMap<IStr, Option<Result<CtxID, ImportError>>>,

    waiting_to_answer: HashMap<IStr, Vec<Request>>,
}

struct ConversationContext {
    publish_as: IStr,

    remaining_scope: Vec<IStr>,

    searching_within: CtxID,

    public: bool,
}

impl Resolver {
    fn on_root(root: CtxID) {}

    fn next_convo(&mut self) -> usize {
        self.convo_id_gen += 1;

        self.convo_id_gen
    }

    /// When we want to step resolving something, we have an idea of where we
    /// "start" the resolve. If we are just now starting resolving a `use` statement,
    /// then that `within` node is the current 'self' node
    /// If the use statement is `use super::a::b`, then at the start of the second step,
    /// `within` is 'self.parent'. When we get to the point that `to_resolve == []`,
    /// `within` is the node we've been searching for the whole time. In that example,
    /// it will be node 'b'. At that point, we apply whatever alias we were instructed to
    /// and then publish the symbol
    fn step_resolve(&mut self, context: ConversationContext) {
        //match to_resolve.as_slice()
        match context.remaining_scope.as_slice() {
            [first, rest @ ..] => match first.resolve() {
                "super" => {}
                "package" => {}
                _ => {}
            },
            // if nothing left to resolve, then `within` is the target for the alias
            [] => {
                if ["super".intern(), "package".intern()].contains(&context.publish_as) {
                    panic!("user tried to alias some symbol as 'super' or 'package' (reserved module names)");
                } else {
                    self.publish(context.searching_within, context.publish_as);
                }
            }
        }
    }

    /// Take a given node and state that it serves as the given alias
    /// when exported
    fn publish(&mut self, id: CtxID, alias: IStr) {
        let to_answer = self
            .waiting_to_answer
            .entry(alias)
            .or_insert(Vec::new())
            .swap_with(Vec::new());
        let postal = Postal::instance();

        for request in to_answer {
            match request {
                Request::AskFor {
                    conversation,
                    symbol,
                    within,
                    reply_to,
                } => postal.send_reply(
                    self.self_ctx,
                    reply_to,
                    Reply::Redirect {
                        reply_from: self.self_ctx,
                        conversation,
                        symbol: alias,
                        within: self.self_ctx,
                        is_at: id,
                    },
                ),
            }
        }
    }

    fn thread(&mut self) {
        let statements: Vec<UseDeclaration> = Vec::new();

        let mut may_provide = HashMap::<IStr, Option<Result<CtxID, ImportError>>>::new();

        let self_node = Contexts::instance().get(&self.self_ctx);

        let mut next_convo: usize = 1;

        let need_resolve = todo!();

        let steps: HashMap<usize, Vec<IStr>> = HashMap::new();

        for stmt in statements.into_iter() {
            let UseDeclaration {
                node_info,
                public,
                scope,
                alias,
            } = stmt;

            if public {
                let symbol_name = alias.unwrap_or(*scope.last().expect("empty scope???"));

                may_provide
                    .insert(symbol_name, None)
                    .map(|v| panic!("duplicate symbol?"));
            }

            let convo = next_convo;

            next_convo += 1;

            if let [first, rest @ ..] = scope.as_slice() {
                match first.resolve() {
                    "super" => {
                        // ask our parent
                    }
                }
            } else {
                unreachable!()
            }

            //Postal::instance().send_ask(self.self_ctx, to, request)

            //

            //Postal::instance().send_ask(, to, request)
        }

        loop {
            // go through our use statements one-by-one
        }
    }

    /// Makes a note to self that the given CtxID must provide
    /// the asked-for symbol name
    ///
    /// If the symbol does exist, tell `for_node`  and restart solve on for_node
    fn require(&self, name: IStr, from_node: CtxID, for_node: CtxID) {}
}
