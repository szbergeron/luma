use std::{
    cell::UnsafeCell,
    collections::{HashMap, HashSet},
    rc::Rc,
    sync::{atomic::AtomicUsize, Mutex},
};

use tracing::{info, warn};
use uuid::Uuid;

use crate::{
    compile::per_module::{
        Content, ControlMessage, ConversationContext, Destination, Earpiece, Message, Service,
    },
    cst::{NodeInfo, ScopedName, UseDeclaration},
    helper::interner::{IStr, Internable},
};

use super::{
    executor::{Executor, UnsafeAsyncCompletable},
    tree::CtxID,
};

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

    ListLocals {
        in_node: CtxID
    },

    LocalsAre {
        for_node: CtxID,
        locals: Vec<(IStr, CtxID)>,
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
    /// The list of conversations that
    /// are currently waiting for a local symbol in order to resolve
    ///
    /// If we have resolved all of our imports and
    /// published all of our local symbols,
    /// then we should tell everyone in this list we
    /// couldn't find the given symbol
    resolutions:
        HashMap<(IStr, CtxID), Rc<UnsafeAsyncCompletable<Result<SimpleResolution, ImportError>>>>,

    possibles: HashSet<IStr>,

    // for now, we're not gonna try to complicate things
    // further by caching both
    //
    // just cache one and then have the do_single calls be
    // cheap and cached
    /// When we fuse this node (freeze its imports),
    /// we can know for certain that everything that will
    /// be exported will be by-name in this vec
    fused_exports: Option<Vec<IStr>>,

    is_fused: bool,
}

impl ResInner {}

pub struct Resolver {
    inner: Mutex<ResInner>,
    earpiece: UnsafeCell<Earpiece>,

    executor: &'static Executor,
    conversations: ConversationContext,

    /// The context that we are handling resolution in the context of
    for_ctx: CtxID,
}

impl Resolver {
    fn with_inner<F, T>(&self, f: F) -> T
    where
        F: FnOnce(&mut ResInner) -> T,
    {
        let mut b = self.inner.lock().unwrap();

        f(&mut *b)
        //f(self.inner.borrow_mut())
    }

    async fn do_use(&self, ud: UseDeclaration) {
        warn!("start resolving use: {ud:?}");

        let last = ud
            .scope
            .scope
            .last()
            .expect("there was no last symbol in a use statement");
        let alias = ud.alias.unwrap_or(*last);

        //self.with_inner(|inner| inner.possibles.insert(alias));

        let resolution = self.do_composite(ud.scope, self.for_ctx).await;

        match resolution {
            Ok(res) => unsafe {
                self.announce_resolution(alias, res.is_at, self.for_ctx, res.is_public)
            },
            Err(ie) => {}
        }
    }

    async fn do_composite(
        &self,
        composite: ScopedName,
        within: CtxID,
    ) -> Result<CompositeResolution, ImportError> {
        let mut cur_within = within;
        let mut public = true;

        for (index, elem) in composite.clone().scope.into_iter().enumerate() {
            info!("checking for elem {elem} within {cur_within:?} in composite res");
            let res = self.do_single(elem, cur_within).await;

            info!("got back a result from do_single");

            match res {
                Ok(v) => {
                    tracing::warn!("got a resolution for a single");
                    cur_within = v.is_at;
                    public = v.is_public;
                }
                Err(e) => {
                    return Err(ImportError { symbol_name: elem, from_ctx: cur_within, error_reason: format!("failed to import symbol from within composite, was at position {index} within composite") })
                }
            }
        }

        warn!("finishes a composite resolution!");

        Ok(CompositeResolution {
            is_public: public,
            name: composite,
            is_at: cur_within,
        })
    }

    async fn do_single(
        &self,
        single: IStr,
        within: CtxID,
    ) -> Result<SimpleResolution, ImportError> {
        warn!(
            "do_single is looking for {single} within {within:?}, while self node is {:?}",
            self.for_ctx
        );
        let mut message_to_send: Option<Message> = None;

        tracing::info!("query for do_single to node, has children:");

        for child in self.for_ctx.resolve().children.iter() {
            tracing::info!("child ({}, {:?})", child.key(), child.value());
        }

        tracing::info!("possible is: {:?}", self.inner.lock().unwrap().possibles);

        if within == self.for_ctx && !self.with_inner(|inner| inner.possibles.contains(&single)) {
            tracing::error!("returns a failure immediately, as it wasn't even a possible");
            return Err(ImportError {
                symbol_name: single,
                from_ctx: within,
                error_reason: "the symbol was not within the possibles set for the current node"
                    .to_owned(),
            });
        }

        let conversation = Uuid::new_v4();

        //tracing::debug!("=== single inner res ptr is: {:?}", &self.inner.lock().unwrap().resolutions as *const _);
        tracing::debug!(
            "entry in there: {}",
            self.inner
                .lock()
                .unwrap()
                .resolutions
                .get(&(single, within))
                .is_some()
        );

        let recv = self.with_inner(|inner| {
            for ((name, within), gbc) in inner.resolutions.iter() {
                warn!("current node is {:?}, current symbol: ({name}, {within:?})", self.for_ctx);
            };

            let recv = unsafe {
                inner
                    .resolutions
                    .entry((single, within))
                    .or_insert_with(|| {
                        warn!("we're the first to wait for the symbol, symbol is {single}, within is {within:?}, we are {:?}, the istr is {}",
                              self.for_ctx, single.id());
                        // need to send a message and have it waiting for resolve
                        // it's going to be our job to complete it after this
                        //Box::leak(Box::new(LocalOneshotBroadcastChannel::new()))
                        //    as *mut LocalOneshotBroadcastChannel<_>
                        let usc = UnsafeAsyncCompletable::new();

                        usc

                        // if there was already something in there, then we recv it and return
                        // that existing resolution immediately
                    })
                    .clone()
                    .wait()
            };

            // if we'd be asking ourself, then we shouldn't send a message, just wait for a
            // resolution. This is the "base case"
            if within == self.for_ctx {
                tracing::debug!("just waiting for the symbol locally");
                // we're expecting the symbol at some point, since we
                // passed the fused_exports check, but
                // we're still waiting for it to be resolved, so just
                // leave it for now
                    if inner.is_fused {
                        unsafe {
                            let useme = inner.resolutions
                                .get(&(single, within))
                                .unwrap()
                                .complete(Err(ImportError {
                                    symbol_name: single,
                                    from_ctx: within,
                                    error_reason: "couldn't import the symbol since we're fused and it wasn't in imports".to_owned() }));
                        }
                    }
            } else {
                // we're asking someone else and just keeping a
                // cache, so we need to start a convo

                // we needed to create the entry, so we're the ones who have to ask
                tracing::debug!("will ask for a DoYouHave for {single}");
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
                tracing::debug!("asked for a DoYouHave for {single}");

                message_to_send = Some(m);
            }

            recv
        });

        // send message if we need to, and await it
        // once we get back, complete the oneshot with it
        if let Some(v) = message_to_send {
            //self.conversation_open(conversation);
            //self.conversations.

            warn!("sent a message to our earpiece");
            //self.conversation_notify_reply(conversation, v);

            let resp = self.conversations.wait_for(v).await;

            //let resp = resp.expect("we asked, so we should get a response damnit");

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
                    } => unsafe {
                        info!("finishes a resolution");
                        self.announce_resolution(symbol, is_at, within, is_public);
                    },
                    nr => {
                        tracing::error!(
                            "got some other kind of nr in response to a single nr request: {nr:?}"
                        );
                    }
                }
            } else {
                tracing::error!("we got something other than an NR in response to an NR ask");
            }
        }

        let val = recv.await;

        warn!("got val from simple of {val:?}");

        val
    }

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
    pub async unsafe fn thread(&'static self) {
        info!("starts thread for resolver");
        while let Ok(v) = self.earpiece.get().as_mut().unwrap().wait().await {
            warn!("got a message: {v:#?}");
            let we_are = &self.for_ctx.resolve().inner;
            warn!("we are: {:?}", we_are);
            //let they_are = &v.from.node.resolve().inner;
            //warn!("they are: {:?}", they_are);

            match v.content.clone() {
                Content::NameResolution(nr) => {
                    match nr {
                        NameResolutionMessage::WhatIs {
                            composite_symbol,
                            given_root,
                        } => {
                            unsafe {
                                let named = format!("do_composite task looking for {composite_symbol:?} given root {given_root:?}");
                                self.executor.install(
                                    async move {
                                        let res =
                                            self.do_composite(composite_symbol.clone(), given_root).await;

                                        let content = match res {
                                            Ok(r) => NameResolutionMessage::RefersTo {
                                                composite_symbol: r.name, is_at: r.is_at, given_root },
                                            Err(e) => {
                                                eprintln!("import error: {e:?}, while looking for: {composite_symbol:?}");
                                                std::process::exit(-1);
                                                NameResolutionMessage::HasNoResolution {
                                                composite_symbol,
                                                longest_prefix: todo!(),
                                                prefix_ends_at: todo!(),
                                                given_root: todo!()
                                            }
                                            }
                                        };

                                        let msg = Message {
                                            to: v.send_reply_to,
                                            from: self.as_dest(),
                                            send_reply_to: Destination::nil(),
                                            conversation: v.conversation,
                                            content: Content::NameResolution(content),
                                        };

                                        self.earpiece.get().as_mut().unwrap().send(msg);
                                    },
                                    named,
                                )
                            };
                        }
                        NameResolutionMessage::DoYouHave { symbol, within } => unsafe {
                            self.executor.install(async move {
                                warn!("received a doyouhave for {symbol} within {within:?}");
                                let res = self.do_single(symbol, within).await;
                                let content = match res {
                                    Ok(SimpleResolution {
                                        is_public,
                                        symbol,
                                        is_at,
                                    }) => NameResolutionMessage::ItIsAt {
                                        symbol,
                                        within,
                                        is_at,
                                        is_public,
                                    },
                                    Err(e) => todo!("handle import errors"),
                                };

                                warn!("resolved a doyouhave for {symbol} which resolved to {res:?}");

                                self.earpiece.get().as_mut().unwrap().send(Message {
                                    to: v.send_reply_to,
                                    from: self.as_dest(),
                                    send_reply_to: Destination::nil(),
                                    conversation: v.conversation,
                                    content: Content::NameResolution(content),
                                })
                            }, format!("do_single task looking for {symbol} within {within:?}"))
                        },
                        other => {
                            if let Some(v) = self.conversations.dispatch(v) {
                                panic!("unhandled message for conversations");
                            }
                        }
                    }
                }
                Content::Control(ControlMessage::ShouldFuseNames()) => {
                    self.fuse();
                }
                _ => todo!("don't have others yet"),
            }
        }
    }

    pub unsafe fn install<'a>(&'static self, into: &Executor) {
        info!("hit install for resolver {:?}", self.for_ctx);
        let as_static: &'static Self = std::mem::transmute(self);

        let nref = self.for_ctx.resolve();

        /*let additional_imports = match &nref.inner {
            crate::ast::tree::NodeUnion::Type(t) => {
                vec![UseDeclaration {
                    node_info: crate::cst::NodeInfo::Builtin,
                    public: false,
                    scope: ScopedName::from_many("super::__locals"),
                    alias: None,
                }]
            }
            crate::ast::tree::NodeUnion::Function(fd, _e) => {
                if fd.is_method {
                    // the symbols of the parent-parent scope should be imported
                    vec![UseDeclaration {
                        node_info: NodeInfo::Builtin,
                        public: false,
                        scope: ScopedName::from_many("super::super::__locals"),
                        alias: None,
                    }]
                } else {
                    vec![UseDeclaration {
                        node_info: NodeInfo::Builtin,
                        public: false,
                        scope: ScopedName::from_many("super::__locals"),
                        alias: None,
                    }]
                    // we're a regular function, so just use symbols from
                    // direct parent scope
                }
            }
            crate::ast::tree::NodeUnion::Global(_) => vec![],
            crate::ast::tree::NodeUnion::Empty() => vec![],
        };*/

        let additional_imports = vec![];

        let _ = {
            let mut inner = self.inner.lock().unwrap();

            for s in ["global", "std", "super"] {
                inner.possibles.insert(s.intern());
            }

            // we can reference symbols from our surrounding scope
            for child in self.for_ctx.resolve().children.iter() {
                inner.possibles.insert(*child.key());

                //self.announce_resolution(child.key(), child.value(), self.for_ctx, false);
            }

            for ud in nref.use_statements.iter() {
                let alias = ud.alias.unwrap_or(*ud.scope.scope.last().unwrap());

                inner.possibles.insert(alias);
            }
        };

        // make all the resolutions for local symbols happen
        for child in nref.children.iter() {
            let (symbol, is_at) = child.pair();
            let is_public = is_at.resolve().public;

            tracing::debug!("puts symbol {symbol} in self");
            self.announce_resolution(*symbol, *is_at, self.for_ctx, is_public);
        }

        if let Some(parent) = nref.parent {
            tracing::debug!("installs super symbol within {:?}", self.for_ctx);
            self.announce_resolution("super".intern(), parent, self.for_ctx, true);
        } else {
            tracing::debug!("no super symbol within {:?}", self.for_ctx);
        }

        if let Some(global) = nref.global {
            self.announce_resolution("global".intern(), global, self.for_ctx, true);

            self.announce_resolution(
                "std".intern(),
                *global
                    .resolve()
                    .children
                    .get(&"std".intern())
                    .unwrap()
                    .value(),
                self.for_ctx,
                false,
            );
        } else {
            // we *are* the global
            self.announce_resolution("global".intern(), self.for_ctx, self.for_ctx, true);

            self.announce_resolution(
                "std".intern(),
                *self
                    .for_ctx
                    .resolve()
                    .children
                    .get(&"std".intern())
                    .unwrap()
                    .value(),
                self.for_ctx,
                false,
            );
            //panic!("There was no global around for {:?}", self.for_ctx);
        }

        let remaining_use_statements = Rc::new(AtomicUsize::new(
            self.for_ctx.resolve().use_statements.len(),
        ));

        for ud in self.for_ctx.resolve().use_statements.clone().into_iter().chain(additional_imports.into_iter()) {
            let named = format!(
                "do_use task that uses UD {ud:?} within node {:?}",
                self.for_ctx
            );

            let remaining_use_statements = remaining_use_statements.clone();

            info!("starting resolve of ud {ud:?}");
            let fut = async move {
                as_static.do_use(ud).await;

                let remaining =
                    remaining_use_statements.fetch_sub(1, std::sync::atomic::Ordering::Relaxed);

                if remaining == 0 {
                    as_static.fuse();
                }
            };

            let _task = into.install(fut, named);
        }

        if remaining_use_statements.load(std::sync::atomic::Ordering::Relaxed) == 0 {
            // if there were no use statements, we just do this now
            as_static.fuse();
        }

        //into
    }

    /// we've resolved every use statement, any symbols looking for things within
    /// "us" will never be resolved any more
    fn fuse(&self) {
        self.with_inner(|inner| {
            for ((symname, within), res) in inner.resolutions.iter() {
                if *within == self.for_ctx {
                    // well, :shrug:
                    let r = unsafe {
                        res.complete(Err(ImportError {
                            symbol_name: *symname,
                            from_ctx: *within,
                            error_reason: format!(
                                "all use statements were completed without this symbol appearing",
                            ),
                        }))
                    };

                    match r {
                        Ok(v) => {
                            warn!("we gave up on resolving {symname} because all use statements finished and it wasn't found");
                        }
                        Err(e) => {
                            info!("use statements were resolved and no unresolved symbols remained");
                        }
                    }
                }
            }
            inner.is_fused = true;

        });
    }

    pub unsafe fn announce_resolution(
        &self,
        symbol: IStr,
        is_at: CtxID,
        within: CtxID,
        is_public: bool,
    ) {
        info!(
            "node {:?} is saying we have resolved, entry is ('{symbol}', {within:?}), the istr is {} with val {is_at:?}",
            self.for_ctx,
            symbol.id()
        );

        //tracing::debug!("=== resolution inner res ptr is: {:?}", &self.inner.lock().unwrap().resolutions as *const _);
        //// wtf? how? why is this doing this? where did the completable go :/

        let mut _useme = self
            .inner
            .lock()
            .unwrap()
            .resolutions
            .entry((symbol, within))
            .or_insert_with(|| {
                warn!("announced that we have {symbol} at {is_at:?}, pushing the box");
                //Box::leak(Box::new(LocalOneshotBroadcastChannel::new()))
                //as *mut LocalOneshotBroadcastChannel<_>
                UnsafeAsyncCompletable::new()
            })
            .complete(Ok(SimpleResolution {
                is_public,
                symbol,
                is_at,
            }));
        //.expect("couldn't push resolution");

        tracing::debug!(
            "The entry in there after resolve: {}",
            self.inner
                .lock()
                .unwrap()
                .resolutions
                .get(&(symbol, within))
                .is_some()
        );

        info!("sent the resolution");
    }

    pub unsafe fn announce_failure(&self, symbol: IStr, within: CtxID, ie: ImportError) {
        self.inner
            .lock()
            .unwrap()
            .resolutions
            .entry((symbol, within))
            .or_insert_with(|| UnsafeAsyncCompletable::new())
            .complete(Err(ie))
            .expect("couldn't push an import failure");
    }

    pub fn for_node(node: CtxID, use_executor: &'static Executor, ep: Earpiece) -> Self {
        let cs = ep.cloned_sender();
        Self {
            inner: Mutex::new(ResInner {
                is_fused: false,
                resolutions: HashMap::new(),
                fused_exports: None,
                possibles: HashSet::new(),
            }),
            for_ctx: node,
            executor: use_executor,
            earpiece: UnsafeCell::new(ep),
            conversations: unsafe { ConversationContext::new(cs) },
        }
    }
}

#[derive(Clone, Debug)]
pub struct ImportError {
    pub symbol_name: IStr,
    pub from_ctx: CtxID,
    pub error_reason: String,
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
    pub is_public: bool,
    pub symbol: IStr,
    pub is_at: CtxID,
}

pub struct CompositeResolution {
    pub is_public: bool,
    pub name: ScopedName,
    pub is_at: CtxID,
}

pub struct NameResolver {
    pub name: ScopedName,
    pub based_in: CtxID,
    pub reply_to: CtxID,
    pub service: Service,
}

impl NameResolver {
    pub fn new(name: ScopedName, based_in: CtxID, reply_to: CtxID, service: Service) -> Self {
        Self {
            name,
            based_in,
            service,
            reply_to,
        }
    }

    pub async fn using_context(self, cc: &ConversationContext) -> Result<CtxID, ImportError> {
        let msg = Message {
            to: Destination::resolver(self.based_in),
            from: Destination {
                node: self.reply_to,
                service: self.service,
            },
            send_reply_to: Destination {
                node: self.reply_to,
                service: self.service,
            },
            conversation: Uuid::new_v4(),
            content: Content::NameResolution(NameResolutionMessage::WhatIs {
                composite_symbol: self.name,
                given_root: self.based_in,
            }),
        };

        let reply = cc.wait_for(msg).await;

        if let Content::NameResolution(nr) = reply.content {
            match nr {
                NameResolutionMessage::RefersTo {
                    composite_symbol,
                    is_at,
                    given_root,
                } => Ok(is_at),
                NameResolutionMessage::HasNoResolution {
                    composite_symbol,
                    longest_prefix,
                    prefix_ends_at,
                    given_root,
                } => todo!(),
                NameResolutionMessage::CausesCircularImport { v } => todo!(),
                _ => panic!("got another weird message?"),
            }
        } else {
            panic!("got a weird message back?")
        }
    }
}
