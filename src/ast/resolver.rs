//! This module is responsible for handling imports and type references at a "surface level"
//!
//! It doesn't do type inference or anything "fancy" like that, it simply
//! tries its best to resolve references and detect any import loops
//!
//! Eventually, to handle recursive or deadlocking imports,
//! we can add a watchdog that checks if everyone is sleeping
//! for a significant period of time

use std::collections::HashMap;

use uuid::Uuid;

use crate::{
    cst::{ScopedName, UseDeclaration},
    helper::{
        interner::{IStr, Internable, SpurHelper},
        SwapWith,
    },
};

use super::tree::CtxID;

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

enum Message {
    Request(Request),
    Reply(Reply),
}

/// A PostalWorker handles sending messages between Resolvers :)
struct Postal {
}

impl Postal {
    pub async fn send_reply(&self, from: CtxID, to: CtxID, reply: Reply) {}

    pub async fn send_ask(&self, from: CtxID, to: CtxID, request: Request) {}

    pub async fn wait_next(&self) -> Option<Message> {
        todo!()
    }

    pub fn instance() -> &'static Postal {
        todo!()
    }
}

enum Request {
    AskFor {
        reply_to: CtxID,
        conversation: Uuid,
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
        conversation: Uuid,
        within: CtxID,
        publish: Publish,
    },

    /// If a symbol is not exported by a node at all, then a NotFound is
    /// sent back to the asker
    NotFound {
        reply_from: CtxID,
        conversation: Uuid,
        symbol: IStr,
        within: CtxID,
        cause: Option<ImportError>,
    },
}

#[derive(Clone)]
struct ImportError {
    symbol_name: IStr,
    error_reason: String,
}

struct Resolver {
    self_ctx: CtxID,

    /// When we send out a question, we store the
    /// rest of the scope here that will need to be resolved when we
    /// get the next reply for a given conversation
    ///
    /// When we get a reply, we can reuse the existing
    /// conversation or delete and create a new one
    waiting_to_resolve: HashMap<Uuid, ConversationContext>, // keep the convo context with us

    /// when resolving symbols at the end,
    /// this allows tracking a given ref to the associated publish
    /// (needed for the various Resolved_ variants)
    resolved_refs: HashMap<Uuid, Publish>,

    /// Whenever we want to look up a given scoped name,
    /// we first check if we're already waiting for it.
    /// If it's already in this map, then we don't query for it.
    /// If it's not in this map, then we need to put it in
    /// and ask for the resolution (or start the resolve process)
    name_to_ref: HashMap<ScopedName, Uuid>,

    /// For every symbol that we try to export,
    /// there will be one entry in this hashmap. If we haven't
    /// yet resolved the export yet, then the entry is not in the map.
    /// If we have resolved it to something definite, the entry
    /// (name, Ok(ID)). If we failed to resolve it (the
    /// import that was exported couldn't be resolved) then the entry is
    /// (name, Err())
    publishes: HashMap<IStr, Result<Publish, ImportError>>,

    /// When we get a question we can't yet answer (it
    /// relies on us having a symbol that we can export,
    /// but we haven't yet resolved the import) then we need
    /// to drop it in here, and start a request for it
    /// if there was already an entry in this map
    /// for a symbol, then just add the request to the
    /// map and don't start a new conversation
    waiting_to_answer: HashMap<IStr, Vec<Request>>,
}

struct ConversationContext {
    publish_as: IStr,

    remaining_scope: Vec<IStr>,
    original_scope: ScopedName,

    searching_within: CtxID,

    for_ref_id: Uuid,

    public: bool,
}

#[derive(Clone, Copy)]
struct Publish {
    symbol: IStr,

    is_public: bool,

    go_to: CtxID,

    for_ref_id: Uuid,
}

impl Resolver {
    fn for_node(root: CtxID) -> Self {
        todo!()
    }

    fn next_convo(&mut self) -> Uuid {
        /*self.convo_id_gen += 1;

        self.convo_id_gen*/

        Uuid::new_v4()
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
        match context.remaining_scope.as_slice() {
            [first, rest @ ..] => {
                let conversation = context.for_ref_id;

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
                ).await;

                let mut unres = self
                    .waiting_to_resolve
                    .get_mut(&context.for_ref_id)
                    .unwrap();

                // when we hear back, we should continue resolving from [rest],
                // since we've then resolved symbol `first`
                unres.remaining_scope = rest.to_vec();
            }

            // if nothing left to resolve, then `within` is the target for the alias
            [] => {
                if ["super", "package"].contains(&context.publish_as.resolve()) {
                    panic!("user tried to alias some symbol as 'super' or 'package' (reserved module names)");
                } else {
                    self.publish(Publish {
                        symbol: context.publish_as,
                        is_public: context.public,
                        go_to: context.searching_within,
                        for_ref_id: context.for_ref_id,
                    })
                    .await;
                }
            }
        }
    }

    async fn start_resolve(&mut self, scoped: ScopedName) {
        match self.name_to_ref.get(&scoped) {
            Some(prior) => {
                // already looking up for this scope
            }
            None => {
                // not already looking up, so need to string together the
                // refs and generate a UUID and start the request
                let id = self.next_convo();

                self.name_to_ref.insert(scoped.clone(), id);

                let cc = ConversationContext {
                    publish_as: todo!(),
                    remaining_scope: todo!(),
                    original_scope: todo!(),
                    searching_within: todo!(),
                    for_ref_id: todo!(),
                    public: todo!(),
                };

                self.waiting_to_resolve.insert(id, cc);
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
                let p = match symbol.resolve() {
                    "super" => Some(
                        self.self_ctx
                            .resolve()
                            .parent
                            .map(|cid| Publish {
                                symbol,
                                is_public: true,
                                go_to: cid,
                                for_ref_id: conversation,
                            })
                            .ok_or(ImportError {
                                symbol_name: "super".intern(),
                                error_reason: format!(
                                    "the root of the compilation unit has no 'super'"
                                ),
                            }),
                    ),
                    "global" => Some(Ok(self
                        .self_ctx
                        .resolve()
                        .global
                        .map(|cid| Publish {
                            symbol,
                            is_public: true,
                            go_to: cid,
                            for_ref_id: conversation,
                        })
                        .expect("global is unset for a node??"))),
                    _ => {
                        // if it wasn't a direct child, then we need to wait for our own use
                        // statements for it to resolve
                        //
                        // those may have already resolved, so we check now
                        match self.publishes.get(&symbol) {
                            None => {
                                // we will need to go ask someone before we can reply
                                None
                            }
                            Some(p) => {
                                // we've resolved the symbol within this scope
                                // at least once before, so we can point them in the right
                                // direction right now
                                //
                                // either we resolved a use already, or we have a
                                // direct child by that name
                                match p {
                                    Ok(v) => Some(Ok(*v)),
                                    Err(e) => Some(Err(e.clone())),
                                }
                            }
                        }
                    }
                };

                match p {
                    Some(Ok(val)) => Postal::instance().send_reply(
                        self.self_ctx,
                        reply_to,
                        Reply::Redirect {
                            reply_from: self.self_ctx,
                            conversation,
                            within: self.self_ctx,
                            publish: todo!(),
                        },
                    ).await,
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
                    ).await,
                    None => {
                        // need to save the question for later, since we can't answer
                        // definitively quite yet
                        let mut need_to_ask = false;
                        self.waiting_to_answer.entry(symbol).or_insert_with(|| {
                            need_to_ask = true;
                            vec![question]
                        });

                        if need_to_ask {}
                    }
                }
            }
        }
    }

    /// Take a given node and state that it serves as the given alias
    /// when exported
    async fn publish(&mut self, p: Publish) {
        self.publishes
            .remove(&p.symbol)
            .map(|v| panic!("already published this same alias"));

        if !p.for_ref_id.is_nil() {
            // this should correspond with some resolution we should publish
            self.resolved_refs.insert(p.for_ref_id, p.clone());
        }

        // we use publishes as basically a memoization technique
        // so that we don't have to re-look-up prior queries
        self.publishes.insert(p.symbol, Ok(p));

        let to_answer = self
            .waiting_to_answer
            .entry(p.symbol)
            .or_insert(Vec::new())
            .swap_with(Vec::new()); // take all the things out of the entry, leave it empty

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
                        within: self.self_ctx,
                        publish: p,
                    },
                ).await,
            }
        }
    }

    async fn save_resolve(&mut self, id: Uuid, resolves_to: CtxID) {
    }

    async fn thread2(&mut self) {
        // first, export every direct child with their public value
        for child in self.self_ctx.resolve().children.iter() {
            // all direct descendent nodes of the current node are
            // exported here
            let (&symbol, &ctx_id) = child.pair();
            let is_public = ctx_id.resolve().public;
            self.publish(Publish {
                symbol,
                is_public,
                go_to: ctx_id,
                for_ref_id: todo!(),
            })
            .await;
        }

        // then, ask for every use statement, saving a note to publish
        // any marked public
        for stmt in vec![todo!()] {
            let UseDeclaration {
                node_info,
                public,
                scope,
                alias,
            } = stmt;

            let convo_id = self.next_convo();

            let cc = ConversationContext {
                publish_as: alias
                    .unwrap_or_else(|| *scope.last().expect("empty scope, with no alias")),
                remaining_scope: scope,
                searching_within: self.self_ctx,
                public,
                original_scope: ScopedName {
                    scope: scope.clone(),
                },
                for_ref_id: convo_id,
            };

            self.step_resolve(cc).await; // ask the question, not waiting for an answer yet
        }
        // then, ask ourselves/our parent for every reference that
        // we use directly (this handles functions within the same
        // outer module being able to call each other without
        // the super:: qualifier)
        match self.self_ctx.resolve().inner {
            super::tree::NodeUnion::Type(t) => {
                // we have field types to ask for
                for field in t.fields {
                    let typ = field.has_type.expect("all fields must have a typeref");

                    let typ_bases = typ.bases.read().unwrap();

                    for base in typ_bases.iter() {
                    }
                }
            }
            super::tree::NodeUnion::Function(_) => {
                // we have a return type as well as parameter types
            }
            super::tree::NodeUnion::Global(_) => {
                // we don't support globals yet
                todo!()
            }
            super::tree::NodeUnion::Empty() => {
                // we're just a module, so we don't have any type references directly
            }
        }
        // then, enter the resolver state, stepping each of those
        // ongoing questions until completed
        //
        // eventually, if there are no loops, we will reach
        // a closed state where all requests have either been resolved,
        // or completed with an import error
        while let Some(v) = Postal::instance().wait_next().await {
            match v {
                Message::Request(_) => todo!(),
                Message::Reply(_) => todo!(),
            }
        }
        // at this point, go back to every type reference and give it a resolution
        // based on those results

        match self.self_ctx.resolve().inner {
            super::tree::NodeUnion::Type(t) => {
                // things
                todo!()
            }
            super::tree::NodeUnion::Function(_) => todo!(),
            super::tree::NodeUnion::Global(_) => todo!(),
            super::tree::NodeUnion::Empty() => todo!(),
        }
    }

    /// Makes a note to self that the given CtxID must provide
    /// the asked-for symbol name
    ///
    /// If the symbol does exist, tell `for_node`  and restart solve on for_node
    fn require(&self, name: IStr, from_node: CtxID, for_node: CtxID) {}
}
