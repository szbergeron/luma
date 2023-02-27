use std::collections::{HashMap, HashSet};

use futures_intrusive::channel::{LocalOneshotChannel, LocalOneshotBroadcastChannel};
use smallvec::ToSmallVec;
use uuid::Uuid;

use crate::{
    compile::per_module::{Content, Destination, Earpiece, Message},
    cst::{ScopedName, UseDeclaration},
    helper::interner::IStr,
};

use super::tree::CtxID;

#[derive(Debug, Clone)]
pub enum NameResolutionMessage {
    /// Ask the given node, within the context of
    /// its own location in the module tree, what
    /// node the given ScopedName refers to
    WhatIs {
        composite_symbol: ScopedName,
        given_root: CtxID,
    },

    /// Asks if a node has a given symbol as a direct refer
    DoYouHave { symbol: IStr, within: CtxID },

    /// A reply to a DoYouHave saying we don't
    IDontHave { symbol: IStr, within: CtxID },

    /// A reply to a DoYouHave that says that symbol is at the given CtxID
    /// Also states whether it is public
    ItIsAt {
        symbol: IStr,
        within: CtxID,
        is_at: CtxID,
        is_public: bool,
    },

    /// Says that the given ScopedName refers
    /// to the given CtxID
    RefersTo {
        composite_symbol: ScopedName,
        is_at: CtxID,
        given_root: CtxID,
    },

    /// An announcement/reply from a node stating that
    /// the given ScopedName does not resolve given self
    /// as a root.
    ///
    /// This also sends back the longest prefix
    /// of the scope that *could* be resolved
    /// before the next step failed
    HasNoResolution {
        composite_symbol: ScopedName,
        longest_prefix: ScopedName,
        prefix_ends_at: CtxID,
        given_root: CtxID,
    },

    CausesCircularImport {
        v: !,
        //composite:
    },

    AddImport {
        scope: ScopedName,
        alias: Option<IStr>,
    },
}

enum ConversationFrame {
    UseStatement {
        /// The original scope for this use
        source_scope: ScopedName,

        /// What this use statement should alias as
        alias: IStr,

        /// whether this use statement is reexported as
        /// the given name
        is_public: bool,
    },

    Composite {
        currently_resolving: usize,
        original_scope: ScopedName,
    },

    Single {
        symbol: IStr,
        within: CtxID,
    },
}

struct Conversation {
    id: Uuid,
    frames: Vec<ConversationFrame>,
}

pub struct Resolver {
    /// All of the ongoing conversations
    /// that this resolver is handling
    ///
    /// Conversations that have closed (we issue a term signal,
    /// or get one from the other end) can be removed from here
    //conversations: HashMap<Uuid, Conversation>,
    //resolutions: HashMap<(IStr, CtxID), Result<Resolution, ImportError>>,

    /// The list of conversations that
    /// are currently waiting for a local symbol in order to resolve
    ///
    /// If we have resolved all of our imports and
    /// published all of our local symbols,
    /// then we should tell everyone in this list we
    /// couldn't find the given symbol
    //waiting_for_resolution: HashMap<(IStr, CtxID), Vec<ConversationState>>,

    resolutions: HashMap<(IStr, CtxID), LocalOneshotBroadcastChannel<Result<SimpleResolution, ImportError>>>,

    composite_resolutions: HashMap<(ScopedName, CtxID), LocalOneshotBroadcastChannel<Result<CompositeResolution, ImportError>>>,

    conversations: HashMap<Uuid, LocalOneshotChannel<Message>>,

    /// If an alias exists within this map at the time of publishing,
    /// then the entry should be removed from this map and the symbol
    /// should instead be published locally as the name matching the
    /// value of the entry
    aliases: HashMap<ScopedName, IStr>,

    /// The context that we are handling resolution in the context of
    for_ctx: CtxID,

    earpiece: Earpiece,
}

impl Resolver {
    async fn do_use(&mut self, ud: UseDeclaration) {
        let resolution = self.do_composite(ud.scope, self.for_ctx);
    }

    async fn do_composite(&mut self, composite: ScopedName, within: CtxID) -> Result<SimpleResolution, ImportError> {
        let mut cur_within = within;
        for elem in composite.scope {
            let res = self.do_single(elem, cur_within).await;

            match res {
                Ok(v) => {
                }
            }
        }

        todo!()
    }

    async fn do_single(&mut self, single: IStr, within: CtxID) -> Result<SimpleResolution, ImportError> {
        let conversation = Uuid::new_v4();

        let mut message_to_send: Option<Message> = None;

        let recv = self.resolutions.entry((single, within)).or_insert_with(|| {
            // need to send a message and have it waiting for resolve
            // it's going to be our job to complete it after this
            todo!()

            // if there was already something in there, then we recv it and return
            // that existing resolution immediately
        }).receive();

        // send message if we need to, and await it
        // once we get back, complete the oneshot with it
        if let Some(v) = message_to_send {
            self.earpiece.send(v);
            self.conversations.entry(conversation).or_insert(LocalOneshotChannel::new()).receive().await;
        }
        

        let val = recv.await;

        val.expect("channel was dropped or closed while we needed something")
    }

    /*fn step_conversation(&mut self, id: Uuid) {
        let conversation: Conversation = self.conversations.get(id);

        let current = conversation.frames.pop();

        match current {
            None => {
                // there were no remaining anything to do for this conversation, so delete it
                self.conversations.remove(id);
            }
            Some(v) => match v {
                ConversationFrame::UseStatement {
                    source_scope,
                    alias,
                    is_public,
                } => {
                }
                ConversationFrame::Composite {
                    currently_resolving,
                    original_scope,
                } => todo!(),
                ConversationFrame::Single { symbol, within } => todo!(),
            },
        }
    }*/

    fn loopback(&mut self, nr: NameResolutionMessage, conversation: Option<Uuid>) {
        self.earpiece.send(Message {
            to: self.as_dest(),
            from: self.as_dest(),
            send_reply_to: self.as_dest(),
            conversation: conversation.unwrap_or(Uuid::new_v4()),
            content: Content::NameResolution(nr),
        })
    }

    pub fn as_dest(&self) -> Destination {
        Destination {
            node: self.for_ctx,
            service: crate::compile::per_module::Service::Resolver(),
        }
    }

    pub fn publish_children(&mut self) {
        for (name, child) in self.for_ctx.resolve().children {
            // just treat children all as public for now
            self.signal_resolved(
                (name, self.for_ctx),
                Ok(SimpleResolution {
                    is_public: child.resolve().public,
                    is_at: child,
                }),
            );
        }
    }

    pub fn iter_use_stmts(&mut self) {
        for ud in self.for_ctx.resolve().use_statements.iter() {
            self.loopback(todo!(), todo!());
        }
    }

    pub fn handle_message(&mut self, msg: Message) {
        match msg.content {
            Content::Control(_) => todo!(),
            Content::Announce(_) => todo!(),
            Content::Quark(_) => todo!(),
            Content::NameResolution(nr) => match nr {
                NameResolutionMessage::WhatIs {
                    composite_symbol,
                    given_root,
                } => {
                    self.waiting_for_resolution.insert(
                        (composite_symbol.scope[0], given_root),
                        vec![ConversationState {
                            reply_to: msg.send_reply_to,
                            conversation: msg.conversation,
                            inner: ConversationStateInner::Composite(CompositeState {
                                full_scope: composite_symbol,
                                resolved_elements: 0,
                                current_symbol: composite_symbol.scope[0],
                                currently_within: self.for_ctx,
                                originally_within: self.for_ctx,
                            }),
                        }],
                    );

                    self.loopback(
                        NameResolutionMessage::DoYouHave {
                            symbol: composite_symbol.scope[0],
                            within: self.for_ctx,
                        },
                        Some(msg.conversation),
                    )
                }
                NameResolutionMessage::DoYouHave { symbol, within } => {
                    self.waiting_for_resolution
                        .entry((symbol, within))
                        .or_insert(Vec::new())
                        .push(ConversationState {
                            reply_to: msg.send_reply_to,
                            conversation: msg.conversation,
                            inner: ConversationStateInner::Single(SingleState { symbol, within }),
                        });

                    if let Some(v) = self.resolutions.get(&(symbol, within)) {
                        self.signal_resolved((symbol, within), v.clone());
                    } else {
                        // we're still waiting for this symbol to (maybe) populate locally
                    }
                }
                NameResolutionMessage::IDontHave { symbol, within } => {
                    self.signal_resolved(
                        (symbol, within),
                        Err(ImportError {
                            symbol_name: symbol,
                            from_ctx: msg.from.node,
                            error_reason: format!("could not import the symbol"),
                        }),
                    );
                }
                NameResolutionMessage::ItIsAt {
                    symbol,
                    within,
                    is_at,
                    is_public,
                } => {
                    self.signal_resolved((symbol, within), Ok(SimpleResolution { is_public, is_at }));
                }
                NameResolutionMessage::RefersTo {
                    composite_symbol,
                    is_at,
                    given_root,
                } => {
                    todo!() // this wouldn't really come back to us specifically,
                            // but would be sent to other things
                }
                NameResolutionMessage::HasNoResolution {
                    composite_symbol,
                    longest_prefix,
                    prefix_ends_at,
                    given_root,
                } => {
                    todo!() // we don't really ask ourselves this one,
                            // that's something others ask us
                }
                NameResolutionMessage::CausesCircularImport { v } => {
                    todo!()
                }
            },
        }
    }

    pub fn signal_resolved(
        &mut self,
        (symbol, within): (IStr, CtxID),
        to: Result<SimpleResolution, ImportError>,
    ) {
        for val in self
            .waiting_for_resolution
            .remove(&(symbol, within))
            .unwrap_or(Vec::new())
        {
            match val.inner {
                ConversationStateInner::Composite(c) => {
                    match to {
                        Ok(resolution) => {
                            // either step the composite,
                            // or maybe we've finished it
                            // so we can send a message saying so
                            assert!(c.full_scope.scope[c.resolved_elements] == symbol);

                            c.resolved_elements += 1;

                            if c.resolved_elements == c.full_scope.scope.len() {
                                // we've fully resolved it, and can send the message and throw away
                                // the conversation
                                let msg = NameResolutionMessage::RefersTo {
                                    composite_symbol: c.full_scope,
                                    is_at: resolution.is_at,
                                    given_root: c.originally_within,
                                };

                                let msg = Message {
                                    to: val.reply_to,
                                    from: self.as_dest(),
                                    send_reply_to: Destination::nil(),
                                    conversation: val.conversation,
                                    content: Content::NameResolution(msg),
                                };

                                self.earpiece.send(msg);
                            } else {
                                // need to continue stepping the resolution

                                let cstates = self
                                    .waiting_for_resolution
                                    .entry((
                                        c.full_scope.scope[c.resolved_elements - 1],
                                        resolution.is_at,
                                    ))
                                    .or_insert(Vec::new());

                                c.currently_within = resolution.is_at;
                                c.current_symbol = symbol;

                                cstates.push(ConversationState {
                                    reply_to: val.reply_to,
                                    conversation: val.conversation,
                                    inner: ConversationStateInner::Composite(c),
                                });
                            }
                        }
                        Err(ie) => {
                            // import error, so we immediately send back
                            // an IE
                            let prefix = c.full_scope.scope[0..c.resolved_elements].to_smallvec();

                            let msg = NameResolutionMessage::HasNoResolution {
                                composite_symbol: c.full_scope,
                                longest_prefix: ScopedName { scope: prefix },
                                prefix_ends_at: c.currently_within,
                                given_root: c.originally_within,
                            };

                            self.earpiece.send(Message {
                                to: val.reply_to,
                                from: self.as_dest(),

                                // we don't expect further
                                // communication
                                send_reply_to: Destination::nil(),
                                conversation: val.conversation,
                                content: Content::NameResolution(msg),
                            });
                        }
                    }
                }
                ConversationStateInner::Single(s) => {
                    let msg = match to {
                        Ok(v) => {
                            let msg = NameResolutionMessage::ItIsAt {
                                symbol,
                                is_at: v.is_at,
                                is_public: v.is_public,
                                within,
                            };
                            msg
                        }
                        Err(ie) => {
                            let msg = NameResolutionMessage::IDontHave { symbol, within };
                            msg
                        }
                    };

                    let msg = Message {
                        content: Content::NameResolution(msg),
                        to: val.reply_to,
                        from: self.as_dest(),
                        send_reply_to: Destination::nil(),
                        conversation: val.conversation,
                    };

                    self.earpiece.send(msg); // forget about the convo from here, it's done
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
struct ImportError {
    symbol_name: IStr,
    from_ctx: CtxID,
    error_reason: String,
}

pub struct CompositeResolver {
    ongoing_composites: HashMap<ScopedName, Uuid>,

    /// Values are of the form (remaining_scope, full_scope)
    /// we only reply when either we fail to resolve a step
    /// or when remaining_scope is []
    next_symbol_for: HashMap<(CtxID, IStr), Vec<CompositeState>>,
}

#[derive(Clone, Debug)]
pub struct ConversationState {
    reply_to: Destination,
    conversation: Uuid,
    inner: ConversationStateInner,
}

#[derive(Clone, Debug)]
enum ConversationStateInner {
    Composite(CompositeState),
    Single(SingleState),
}

#[derive(Clone, Debug)]
struct SingleState {
    symbol: IStr,
    within: CtxID,
}

#[derive(Clone, Debug)]
struct CompositeState {
    full_scope: ScopedName,
    resolved_elements: usize,

    current_symbol: IStr,
    currently_within: CtxID,
    originally_within: CtxID,
}

impl CompositeResolver {
    pub fn handle_resolve(&mut self, symbol: IStr, within: CtxID, resolution: Option<SimpleResolution>) {
        match self.next_symbol_for.remove(&(within, symbol)) {
            Some(v) => {
                for sn in v {
                    self.resolve_with(sn, resolution);
                }
            }
            None => {
                // do nothing here, since we didn't have any components that
                // were waiting for that symbol as a local composite
            }
        }
    }

    pub fn resolve_with(&mut self, current: CompositeState, resolution: Option<SimpleResolution>) {
        match resolution {
            Some(v) => {
                //
            }
            None => {}
        }
    }
}

impl Resolver {
    /// We have resolved a local symbol, now figure out
    /// any conversations that can be answered because of it
    ///
    /// If resolution is None, then the symbol was definitively not resolvable
    fn publish(&mut self, symbol: IStr, resolution: Option<SimpleResolution>) {}

    /*fn address_composites(&mut self, newly_resolved_symbols: Vec<(IStr, Resolution)>) {
        self.
    }*/
}

#[derive(Clone, Debug)]
pub struct SimpleResolution {
    is_public: bool,
    symbol: IStr,
    is_at: CtxID,
}

pub struct CompositeResolution {
    is_public: bool,
    name: ScopedName,
    is_at: CtxID,
}

/*#[derive(Clone, Debug)]
pub struct Conversation {
    id: Uuid,
    messages: Vec<Message>,
}*/
