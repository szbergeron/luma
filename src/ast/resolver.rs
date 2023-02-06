//! This module is responsible for handling imports and type references at a "surface level"
//!
//! It doesn't do type inference or anything "fancy" like that, it simply
//! tries its best to resolve references and detect any import loops
//!
//! Eventually, to handle recursive or deadlocking imports,
//! we can add a watchdog that checks if everyone is sleeping
//! for a significant period of time

use std::collections::HashMap;

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
        cause: Option<ImportError>,
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
    fn for_node(root: CtxID) -> Self {
        todo!()
    }

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
    async fn step_resolve(&mut self, context: ConversationContext) {
        //match to_resolve.as_slice()
        match context.remaining_scope.as_slice() {
            [first, rest @ ..] => {
                let conversation = self.next_convo();

                // start by asking the target node (which could be ourself!)
                // to get back to us with where the import is
                Postal::instance().send_ask(
                    self.self_ctx,
                    context.searching_within,
                    Request::AskFor {
                        reply_to: self.self_ctx,
                        conversation,
                        symbol: *first,
                        within: context.searching_within,
                    },
                );

                // now that we've sent the ask, we store back the context for later
                self.waiting_to_resolve.insert(conversation, rest.to_vec());
            }
            // if nothing left to resolve, then `within` is the target for the alias
            [] => {
                if ["super".intern(), "package".intern()].contains(&context.publish_as) {
                    panic!("user tried to alias some symbol as 'super' or 'package' (reserved module names)");
                } else {
                    // ignore `public` modifiers for now, just interpret
                    // everything as exported for the time being
                    self.publish(context.searching_within, context.publish_as);
                }
            }
        }
    }

    async fn handle_question(&mut self, question: Request) {
        match question {
            Request::AskFor {
                reply_to,
                conversation,
                symbol,
                within,
            } => {
                let go_ask = match symbol.resolve() {
                    "super" => Some(self.self_ctx.resolve().parent.ok_or(ImportError {
                        symbol_name: "super".intern(),
                        error_reason: format!("the root of the compilation unit has no 'super'"),
                    })),
                    "global" => Some(Ok(self
                        .self_ctx
                        .resolve()
                        .global
                        .expect("global is unset for a node??"))),
                    _ => {
                        todo!("requests and children");
                        // if it wasn't a direct child, then we need to wait for our own use
                        // statements for it to resolve
                        None
                    }
                };

                match go_ask {
                    Some(Ok(id)) => Postal::instance().send_reply(
                        self.self_ctx,
                        reply_to,
                        Reply::Redirect {
                            reply_from: self.self_ctx,
                            conversation,
                            symbol,
                            within: self.self_ctx,
                            is_at: id,
                        },
                    ),
                    Some(Err(e)) => Postal::instance().send_reply(
                        self.self_ctx,
                        reply_to,
                        Reply::NotFound {
                            reply_from: self.self_ctx,
                            conversation,
                            symbol,
                            within: self.self_ctx,
                            cause: Some(e),
                        },
                    ),
                    None => {
                        // need to save the question for later, since we can't answer
                        // definitively quite yet
                    }
                }
            }
        }
    }

    /// Take a given node and state that it serves as the given alias
    /// when exported
    async fn publish(&mut self, id: CtxID, alias: IStr) {
        //self.publishes.entry(alias).and_modify(|e| e.map(|i| panic!("duplicate symbol: {e}"))).or_insert(None);
        self.publishes
            .remove(&alias)
            .flatten()
            .map(|v| panic!("already published this same alias"));

        self.publishes.insert(alias, Some(Ok(id)));

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

    async fn thread2(&mut self) {
        // first, export every direct child
        //
        // then, ask for every use statement, saving a note to publish
        // any marked public
        //
        // then, ask ourselves/our parent for every reference that
        // we use directly (this handles functions within the same
        // outer module being able to call each other without
        // the super:: qualifier)
        //
        // then, enter the resolver state, stepping each of those
        // ongoing questions until completed
        //
        // eventually, if there are no loops, we will reach
        // a closed state where all requests have either been resolved,
        // or completed with an import error
        //
        // at this point, go back to every type reference and give it a resolution
        // based on those results
    }

    async fn thread(&mut self) {
        let statements: Vec<UseDeclaration> = Vec::new();

        //let mut may_provide = HashMap::<IStr, Option<Result<CtxID, ImportError>>>::new();

        let self_node = Contexts::instance().get(&self.self_ctx);

        //let mut next_convo: usize = 1;

        let need_resolve = todo!();

        let steps: HashMap<usize, Vec<IStr>> = HashMap::new();

        for child in self.self_ctx.resolve().children.iter() {
            // all direct descendent nodes of the current node are
            // exported here
            self.publish(*child.value(), *child.key());
        }

        // ask all the questions we have for our imports

        for stmt in statements.into_iter() {
            let UseDeclaration {
                node_info,
                public,
                scope,
                alias,
            } = stmt;

            let symbol_name = alias.unwrap_or(*scope.last().expect("empty scope???"));

            may_provide
                .insert(symbol_name, None)
                .map(|v| panic!("duplicate symbol?"));

            let convo = next_convo;

            next_convo += 1;

            if let [first, rest @ ..] = scope.as_slice() {
            } else {
                unreachable!()
            }

            //Postal::instance().send_ask(self.self_ctx, to, request)

            //

            //Postal::instance().send_ask(, to, request)
        }

        // ask our direct parent for all of the symbols that
        // we need to have for ourself as a type
        //
        // but don't re-export them, we only need them present

        // check whether, with the given symbols in scope, all of the 

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
