use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

use async_executor::LocalExecutor;
use futures_intrusive::channel::{LocalOneshotBroadcastChannel, LocalOneshotChannel};
use smallvec::ToSmallVec;
use tokio::task::LocalSet;
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

pub struct ResInner {
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
    resolutions:
        HashMap<(IStr, CtxID), LocalOneshotBroadcastChannel<Result<SimpleResolution, ImportError>>>,

    composite_resolutions: HashMap<
        (ScopedName, CtxID),
        LocalOneshotBroadcastChannel<Result<CompositeResolution, ImportError>>,
    >,

    conversations: HashMap<Uuid, LocalOneshotChannel<Message>>,

    /// If an alias exists within this map at the time of publishing,
    /// then the entry should be removed from this map and the symbol
    /// should instead be published locally as the name matching the
    /// value of the entry
    aliases: HashMap<ScopedName, IStr>,

    /// The context that we are handling resolution in the context of
    earpiece: Earpiece,
}

impl ResInner {}

pub struct Resolver {
    inner: RefCell<ResInner>,

    for_ctx: CtxID,
}

impl Resolver {
    fn with_inner<'a, F, T>(&'a self, f: F) -> T
    where
        F: FnOnce(&'a mut ResInner) -> T,
    {
        todo!()
    }

    async fn do_use(&self, ud: UseDeclaration) {
        let resolution = self.do_composite(ud.scope, self.for_ctx);
    }

    async fn do_composite(
        &self,
        composite: ScopedName,
        within: CtxID,
    ) -> Result<SimpleResolution, ImportError> {
        let mut cur_within = within;
        for elem in composite.scope {
            let res = self.do_single(elem, cur_within).await;

            match res {
                Ok(v) => {}
                Err(e) => {}
            }
        }

        todo!()
    }

    async fn do_single(
        &self,
        single: IStr,
        within: CtxID,
    ) -> Result<SimpleResolution, ImportError> {
        let conversation = Uuid::new_v4();

        let mut message_to_send: Option<Message> = None;

        let recv = self.with_inner(|inner| {
            let recv = inner
                .resolutions
                .entry((single, within))
                .or_insert_with(|| {
                    // need to send a message and have it waiting for resolve
                    // it's going to be our job to complete it after this
                    todo!()

                    // if there was already something in there, then we recv it and return
                    // that existing resolution immediately
                })
                .receive();

            recv
        });

        // send message if we need to, and await it
        // once we get back, complete the oneshot with it
        if let Some(v) = message_to_send {
            self.with_inner(|inner| inner.earpiece.send(v));

            self.with_inner(|inner| {
                inner
                    .conversations
                    .entry(conversation)
                    .or_insert(LocalOneshotChannel::new())
                    .receive()
            })
            .await;
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
        self.with_inner(|inner| {
            inner.earpiece.send(Message {
                to: self.as_dest(),
                from: self.as_dest(),
                send_reply_to: self.as_dest(),
                conversation: conversation.unwrap_or(Uuid::new_v4()),
                content: Content::NameResolution(nr),
            })
        });
    }

    pub fn as_dest(&self) -> Destination {
        Destination {
            node: self.for_ctx,
            service: crate::compile::per_module::Service::Resolver(),
        }
    }

    /// 'static here isn't actually static, only long enough that we stop executing
    /// the localset for the futures spawned by self
    pub async unsafe fn thread(&'static self) {}

    pub unsafe fn install<'a>(&'a self, into: &mut LocalSet) {
        let as_static: &'static Self = std::mem::transmute(self);
        //let into = LocalExecutor::njew();

        for ud in self.for_ctx.resolve().use_statements.clone().into_iter() {
            let fut = async { as_static.do_use(ud).await; };

            let _task = into.spawn_local(fut);
        }

        //into
    }

    pub fn for_node(node: CtxID, ep: Earpiece) -> Self {
        todo!()
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
    pub fn handle_resolve(
        &mut self,
        symbol: IStr,
        within: CtxID,
        resolution: Option<SimpleResolution>,
    ) {
        match self.next_symbol_for.remove(&(within, symbol)) {
            Some(v) => {
                for sn in v {
                    self.resolve_with(sn, resolution.clone());
                }
            }
            None => {
                // do nothing here, since we didn't have any components that
                // were waiting for that symbol as a local composite
            }
        }
    }

    fn resolve_with(&mut self, current: CompositeState, resolution: Option<SimpleResolution>) {
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
