use std::{
    cell::{RefCell, UnsafeCell},
    collections::{HashMap, HashSet},
    pin::Pin,
};

use async_executor::LocalExecutor;
use futures_intrusive::channel::{LocalOneshotBroadcastChannel, LocalOneshotChannel, GenericOneshotBroadcastChannel};
use smallvec::ToSmallVec;
use tokio::task::LocalSet;
use tracing::{info, warn};
use uuid::Uuid;

use crate::{
    compile::per_module::{Content, Destination, Earpiece, Message, Service},
    cst::{ScopedName, UseDeclaration},
    helper::interner::{IStr, Internable},
};

use super::{executor::Executor, tree::CtxID};

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
    resolutions: HashMap<
        (IStr, CtxID),
        *mut LocalOneshotBroadcastChannel<Result<SimpleResolution, ImportError>>,
    >,

    composite_resolutions: HashMap<
        (ScopedName, CtxID),
        *mut LocalOneshotBroadcastChannel<Result<CompositeResolution, ImportError>>,
    >,

    conversations: HashMap<Uuid, *mut LocalOneshotChannel<Message>>,

    /// If an alias exists within this map at the time of publishing,
    /// then the entry should be removed from this map and the symbol
    /// should instead be published locally as the name matching the
    /// value of the entry
    aliases: HashMap<ScopedName, IStr>,
}

impl ResInner {}

pub struct Resolver {
    inner: RefCell<ResInner>,
    earpiece: UnsafeCell<Earpiece>,

    /// The context that we are handling resolution in the context of
    for_ctx: CtxID,
}

impl Resolver {
    /// Returns true if we created a new conversation, false if it already existed
    fn conversation_open(&self, conversation: Uuid) -> bool {
        let mut r = true;
        self.inner
            .borrow_mut()
            .conversations
            .entry(conversation)
            .or_insert_with(|| {
                r = false;
                Box::leak(Box::new(LocalOneshotChannel::new())) as *mut LocalOneshotChannel<_>
            });

        r
    }

    /// need to make very *very* sure that the conversation
    /// we're closing isn't one being actively awaited
    unsafe fn conversation_close(&self, conversation: Uuid) {
        match self.inner.borrow_mut().conversations.remove(&conversation) {
            Some(v) => unsafe { std::mem::drop(Box::from_raw(v)) },
            None => warn!("tried to remove conversation by id {conversation} which did not exist"),
        }
    }

    /// Before we wait, make sure nothing is going to delete this conversation
    /// while we're awaiting
    ///
    /// The thing awaiting should be the same future that eventually deletes
    async unsafe fn conversation_wait_reply(&self, conversation: Uuid) -> Option<Message> {
        let conv = *self
            .inner
            .borrow_mut()
            .conversations
            .get(&conversation)
            .expect("tried to wait on a conversation that didn't yet exist");

        // we then drop the borrow, keep the ptr

        conv.as_ref().unwrap().receive().await
    }

    /// returns true if successfully pushed the message,
    fn conversation_notify_reply(&self, cnversation: Uuid, message: Message) -> bool {
        let conv = *self
            .inner
            .borrow_mut()
            .conversations
            .get(&cnversation)
            .expect("tried to send to a conversation that no longer exists");

        unsafe { conv.as_ref().unwrap().send(message) }.is_ok()
    }

    fn with_inner<F, T>(&self, f: F) -> T
    where
        F: FnOnce(&mut ResInner) -> T,
    {
        let mut b = self.inner.borrow_mut();

        f(&mut *b)
        //f(self.inner.borrow_mut())
    }

    async fn do_use(&self, ud: UseDeclaration) {
        warn!("start resolving use: {ud:?}");

        let resolution = self.do_composite(ud.scope, self.for_ctx).await;
    }

    async fn do_composite(
        &self,
        composite: ScopedName,
        within: CtxID,
    ) -> Result<SimpleResolution, ImportError> {
        let mut cur_within = within;
        for elem in composite.scope {
            info!("checking for elem {elem} within {cur_within:?} in composite res");
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
        warn!("do_single is looking for {single} within {within:?}");
        let mut message_to_send: Option<Message> = None;

        let conversation = Uuid::new_v4();

        let recv = self.with_inner(|inner| {
            let recv = unsafe {
                inner
                    .resolutions
                    .entry((single, within))
                    .or_insert_with(|| {
                        // need to send a message and have it waiting for resolve
                        // it's going to be our job to complete it after this
                        Box::leak(Box::new(LocalOneshotBroadcastChannel::new()))
                            as *mut LocalOneshotBroadcastChannel<_>

                        // if there was already something in there, then we recv it and return
                        // that existing resolution immediately
                    })
                    .as_mut()
                    .unwrap()
                    .receive()
            };

            // if we'd be asking ourself, then we shouldn't send a message, just wait for a
            // resolution. This is the "base case"
            if within == self.for_ctx {
                // just do nothing
            } else {
                // we're asking someone else and just keeping a
                // cache, so we need to start a convo

                // we needed to create the entry, so we're the ones who have to ask
                let m = Message {
                    to: Destination {
                        node: within,
                        service: Service::Resolver(),
                    },
                    from: self.as_dest(),
                    send_reply_to: self.as_dest(),
                    conversation,
                    content: Content::NameResolution(NameResolutionMessage::DoYouHave {
                        symbol: single,
                        within,
                    }),
                };

                message_to_send = Some(m);
            }

            recv
        });

        // send message if we need to, and await it
        // once we get back, complete the oneshot with it
        if let Some(v) = message_to_send {
            self.conversation_open(conversation);

            unsafe {
                self.earpiece.get().as_mut().unwrap().send(v);
            }
            warn!("sent a message to our earpiece");
            //self.conversation_notify_reply(conversation, v);

            let resp = unsafe { self.conversation_wait_reply(conversation).await };

            let resp = resp.expect("we asked, so we should get a response damnit");

            if let Content::NameResolution(nr) = resp.content {
                match nr {
                    NameResolutionMessage::IDontHave { symbol, within } => {
                        todo!("didn't find it, they didn't have the symbol");
                    }
                    NameResolutionMessage::ItIsAt {
                        symbol,
                        within,
                        is_at,
                        is_public,
                    } => {
                        todo!("I found it!");
                    }
                    nr => {
                        tracing::error!(
                            "got some other kind of nr in response to a single nr request: {nr:?}"
                        );
                    }
                }
            } else {
                tracing::error!("we got something other than an NR in response to an NR ask");
            }

            unsafe {
                self.conversation_close(conversation);
            }
        }

        let val = recv.await;

        // need to delete the conversation channel

        let val = val.expect("channel was dropped or closed while we needed something");

        warn!("got val from simple of {val:?}");

        val
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
        unsafe {
            self.earpiece.get().as_mut().unwrap().send(Message {
                to: self.as_dest(),
                from: self.as_dest(),
                send_reply_to: self.as_dest(),
                conversation: conversation.unwrap_or(Uuid::new_v4()),
                content: Content::NameResolution(nr),
            });
        }
    }

    pub fn as_dest(&self) -> Destination {
        Destination {
            node: self.for_ctx,
            service: crate::compile::per_module::Service::Resolver(),
        }
    }

    /// 'static here isn't actually static, only long enough that we stop executing
    /// the localset for the futures spawned by self
    pub async unsafe fn thread(&self) {
        info!("starts thread for resolver");
        while let Ok(v) = self.earpiece.get().as_mut().unwrap().wait().await {
            warn!("got a message: {v:?}");

            match v.content {
                Content::NameResolution(nr) => match nr {
                    _ => todo!("nothin...")
                },
                _ => todo!("don't have others yet"),
            }
        }
        //self.with_inner(|inner| inner.earpiece.wait().await);
    }

    pub unsafe fn install<'a>(&'a self, into: &Executor) {
        info!("hit install for resolver {:?}", self.for_ctx);
        let as_static: &'static Self = std::mem::transmute(self);
        //let into = LocalExecutor::njew();

        let nref = self.for_ctx.resolve();

        // make all the resolutions for local symbols happen
        for child in nref.children.iter() {
            let (symbol, is_at) = child.pair();
            let is_public = is_at.resolve().public;

            self.announce_have(*symbol, *is_at, is_public);
        }

        if let Some(parent) = nref.parent {
            self.announce_have("super".intern(), parent, true);
        }

        if let Some(global) = nref.global {
            self.announce_have("global".intern(), global, true);
        }



        for ud in self.for_ctx.resolve().use_statements.clone().into_iter() {
            info!("starting resolve of ud {ud:?}");
            let fut = async {
                as_static.do_use(ud).await;
            };

            let _task = into.install(fut);
        }


        //into
    }

    pub unsafe fn announce_have(&self, symbol: IStr, is_at: CtxID, is_public: bool) {
        //warn!("node {:?} is saying we have symbol {symbol} at {is_at:?}", self.for_ctx);
        self.inner.borrow_mut().resolutions.entry((symbol, is_at)).or_insert_with(|| {
            Box::leak(Box::new(LocalOneshotBroadcastChannel::new())) as *mut LocalOneshotBroadcastChannel<_>
        }).as_mut().unwrap().send(Ok(SimpleResolution { is_public, symbol, is_at })).expect("couldn't push resolution");
    }

    pub fn for_node(node: CtxID, ep: Earpiece) -> Self {
        Self {
            inner: RefCell::new(ResInner {
                resolutions: HashMap::new(),
                composite_resolutions: HashMap::new(),
                conversations: HashMap::new(),
                aliases: HashMap::new(),
            }),
            for_ctx: node,
            earpiece: UnsafeCell::new(ep),
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
