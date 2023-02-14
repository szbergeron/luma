//! This module is responsible for handling imports and type references at a "surface level"
//!
//! It doesn't do type inference or anything "fancy" like that, it simply
//! tries its best to resolve references and detect any import loops
//!
//! Eventually, to handle recursive or deadlocking imports,
//! we can add a watchdog that checks if everyone is sleeping
//! for a significant period of time

use itertools::Itertools;
//use core::slice::SlicePattern;
use std::{
    collections::HashMap,
    sync::{atomic::AtomicUsize, Arc},
    time::Duration,
};

use futures::future::join_all;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender};
use uuid::Uuid;

use crate::{
    cst::{ExpressionWrapper, ScopedName, UseDeclaration, TypeReference},
    helper::{
        interner::{IStr, Internable, SpurHelper},
        SwapWith,
    },
};

use super::{
    tree::CtxID,
    types::{AbstractTypeReference, ResolvedType, TypeBase},
};

pub struct ResolverWorker {
    /// we are solely responsible
    /// for dealing with the nodes within our domain.
    ///
    /// domains try to be clustered so that individual workers
    /// don't trample over each other, and so that
    /// when they "talk" to each other it is at a logical boundary at a parent spot in the tree
    domain: Vec<CtxID>,
}

impl ResolverWorker {
    /// takes a domain of unresolved contexts and
    /// starts resolvers for them
    pub fn new(domain: Vec<CtxID>) -> Self {
        Self { domain }
    }

    /// consumes self to hopefully
    /// avoid accidental re-resolving of things
    pub async fn resolve(self) {
        let mut senders = HashMap::new();
        let mut receivers = HashMap::new();

        for c in self.domain.iter() {
            let (send, recv) = tokio::sync::mpsc::unbounded_channel();
            senders.insert(*c, send);
            receivers.insert(*c, recv);
        }

        let postal = Arc::new(Postal::new(senders));

        let mut v: Vec<Resolver> = self
            .domain
            .into_iter()
            .map(|cid| {
                let resolver =
                    Resolver::for_node(cid, postal.clone(), receivers.remove(&cid).unwrap());
                resolver
            })
            .collect();

        println!("about to start watchdog");
        tokio::spawn(Watchdog::new(postal.clone()).run());
        println!("started watchdog");

        join_all(v.iter_mut().map(|r| r.thread2())).await;
    }
}

#[derive(Debug)]
enum Message {
    Request(Request),
    Reply(Reply),
    Exit(),
    /// A heartbeat message to check node status
    CheckIn(),
}

struct Watchdog {
    postal: Arc<Postal>,
}

impl Watchdog {
    pub fn new(p: Arc<Postal>) -> Self {
        Self { postal: p }
    }

    pub async fn run(self) {
        println!("about to send heartbeat");
        while self.postal.send_heartbeat() {
            println!("sent heartbeat");
            tokio::time::sleep(Duration::from_millis(500)).await;
        }
    }
}

/// A PostalWorker handles sending messages between Resolvers :)
struct Postal {
    senders: HashMap<CtxID, tokio::sync::mpsc::UnboundedSender<Message>>,
    exited: AtomicUsize,
}

impl Postal {
    fn new(senders: HashMap<CtxID, UnboundedSender<Message>>) -> Self {
        Self {
            senders,
            exited: AtomicUsize::new(0),
        }
    }

    pub fn send_reply(&self, from: CtxID, to: CtxID, reply: Reply) {
        self.senders
            .get(&to)
            .unwrap()
            .send(Message::Reply(reply))
            .unwrap();
    }

    pub fn send_ask(&self, from: CtxID, to: CtxID, request: Request) {
        self.senders
            .get(&to)
            .unwrap()
            .send(Message::Request(request))
            .unwrap();
    }

    pub fn sign_out(&self, id: CtxID) {
        println!("{id:?} signs out");
        let new_v = self
            .exited
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst)
            + 1;

        println!("new signed-in count is {new_v}");

        if new_v == self.senders.len() {
            println!("exiting everyone");
            // everyone has signed out
            for sender in self.senders.values() {
                sender.send(Message::Exit()).unwrap();
            }
        }
    }

    pub fn exited(&self) -> bool {
        self.exited.load(std::sync::atomic::Ordering::SeqCst) == self.senders.len()
    }

    /// returns true if heartbeat was sent successfully,
    /// false if everyone has exited
    pub fn send_heartbeat(&self) -> bool {
        if self.exited() {
            false
        } else {
            for sender in self.senders.values() {
                let _ = sender.send(Message::CheckIn());
            }

            true
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Request {
    AskFor {
        reply_to: CtxID,
        conversation: Uuid,
        symbol: IStr,
        within: CtxID,
    },
}

#[derive(Debug, Clone)]
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

#[derive(Clone, Debug)]
struct ImportError {
    symbol_name: IStr,
    error_reason: String,
}

pub struct Resolver {
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
    /// If we have resolved it to something definite, the entrArcy
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

    /// Allows us to send messages to other nodes
    postal: Arc<Postal>,

    /// Lets us hear messages from other nodes
    listen: UnboundedReceiver<Message>,

    signed_out: bool,
}

#[derive(Debug, Clone)]
struct ConversationContext {
    /// If this is None, then we aren't publishing this symbol in any way
    publish_as: Option<IStr>,

    remaining_scope: Vec<IStr>,
    original_scope: ScopedName,

    searching_within: CtxID,

    for_ref_id: Uuid,

    public: bool,
}

#[derive(Debug, Clone, Copy)]
struct Publish {
    symbol: Option<IStr>,

    is_public: bool,

    go_to: CtxID,

    for_ref_id: Uuid,
}

impl Resolver {
    fn for_node(
        root: CtxID,
        with_postal: Arc<Postal>,
        listen: tokio::sync::mpsc::UnboundedReceiver<Message>,
    ) -> Self {
        Self {
            self_ctx: root,
            waiting_to_resolve: HashMap::new(),
            resolved_refs: HashMap::new(),
            name_to_ref: HashMap::new(),
            publishes: HashMap::new(),
            waiting_to_answer: HashMap::new(),
            postal: with_postal,
            listen,
            signed_out: false,
        }
    }

    fn next_convo(&self) -> Uuid {
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
    fn step_resolve(&mut self, conversation: Uuid) {
        let context_ref = self.waiting_to_resolve.get_mut(&conversation).unwrap();
        println!("Steps resolve of {context_ref:?}");
        match context_ref.remaining_scope.clone().as_slice() {
            [first, rest @ ..] => {
                println!("stepping conversation {conversation}");

                // start by asking the target node (which could be ourself!)
                // to get back to us with where the import is
                self.postal
                    .send_ask(
                        self.self_ctx,
                        context_ref.searching_within,
                        Request::AskFor {
                            reply_to: self.self_ctx,
                            conversation,
                            symbol: *first,
                            within: context_ref.searching_within,
                        },
                    );

                // when we hear back, we should continue resolving from [rest],
                // since we've then resolved symbol `first`
                context_ref.remaining_scope = rest.to_vec();
            }

            // if nothing left to resolve, then `within` is the target for the alias
            [] => {
                println!("stepped, and the scope was empty");
                if ["super", "package"]
                    .contains(&context_ref.publish_as.unwrap_or("".intern()).resolve())
                {
                    panic!("user tried to alias some symbol as 'super' or 'package' (reserved module names)");
                } else {
                    let p = Publish {
                        symbol: context_ref.publish_as,
                        is_public: context_ref.public,
                        go_to: context_ref.searching_within,
                        for_ref_id: context_ref.for_ref_id,
                    };
                    self.publish(p);
                }
            }
        }
    }

    /*async fn start_resolve(&mut self, scoped: ScopedName) {
        println!("Starts resolve {scoped:?}");
        match self.name_to_ref.get(&scoped) {
            Some(prior) => {
                // already looking up for this scope
                println!("already looked up for this scope, prior is {prior}");
            }
            None => {
                // not already looking up, so need to string together the
                // refs and generate a UUID and start the request
                let id = self.next_convo();

                println!("adds using convo id {id}");

                self.name_to_ref.insert(scoped.clone(), id);

                let cc = ConversationContext {
                    publish_as: None, // this isn't an import
                    remaining_scope: r.named.clone().scope,
                    original_scope: r.named.clone(),
                    // we search within parent here because we're a typeref within
                    // a Type, which implicitly looks for things within the parent
                    // scope unless we *explicitly* use the Self qualifier
                    searching_within: self.self_ctx.resolve().parent.unwrap(),
                    for_ref_id: convo_id,
                    public: false,
                };

                self.waiting_to_resolve.insert(id, cc);

                self.step_resolve(cc).await;
            }
        }
    }*/

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
                                symbol: Some(symbol),
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
                            symbol: Some(symbol),
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
                    Some(Ok(val)) => {
                        self.postal
                            .send_reply(
                                self.self_ctx,
                                reply_to,
                                Reply::Redirect {
                                    reply_from: self.self_ctx,
                                    conversation,
                                    within: self.self_ctx,
                                    publish: val,
                                },
                            )
                    }
                    Some(Err(e)) => {
                        self.postal
                            .send_reply(
                                self.self_ctx,
                                reply_to,
                                Reply::NotFound {
                                    reply_from: self.self_ctx,
                                    conversation,
                                    symbol,
                                    within: self.self_ctx,
                                    cause: Some(e),
                                },
                            )
                    }
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
    fn publish(&mut self, p: Publish) {
        println!("publishes {p:?}");
        self.publishes
            .remove(&p.symbol.unwrap_or("".intern())) // use the "" null symbol here
            .map(|v| panic!("already published this same alias"));

        if !p.for_ref_id.is_nil() {
            // this should correspond with some resolution we should publish
            self.resolved_refs.insert(p.for_ref_id, p.clone());
        }

        self.waiting_to_resolve.remove(&p.for_ref_id); // no longer waiting to resolve it

        println!(
            "we now have {} unresolved refs",
            self.waiting_to_resolve.len()
        );

        if let Some(symbol) = p.symbol {
            // we use publishes as basically a memoization technique
            // so that we don't have to re-look-up prior queries
            self.publishes.insert(symbol, Ok(p));

            let to_answer = self
                .waiting_to_answer
                .entry(symbol)
                .or_insert(Vec::new())
                .swap_with(Vec::new()); // take all the things out of the entry, leave it empty

            let postal = &self.postal;

            for request in to_answer {
                match request {
                    Request::AskFor {
                        conversation,
                        symbol,
                        within,
                        reply_to,
                    } => {
                        postal
                            .send_reply(
                                self.self_ctx,
                                reply_to,
                                Reply::Redirect {
                                    reply_from: self.self_ctx,
                                    conversation,
                                    within: self.self_ctx,
                                    publish: p,
                                },
                            )
                    }
                }
            }
        }
    }

    async fn save_resolve(&mut self, id: Uuid, resolves_to: CtxID) {}

    fn start_resolve(&mut self, tb: &mut TypeReference) {
        let abstrakt: &mut AbstractTypeReference = match tb {
            TypeReference::Syntactic(s) => {
                let generic_args = self.self_ctx.resolve().generics.iter().map(|(name, _tr)| *name).collect_vec();
                let abstrakt = s.to_abstract(generic_args.as_slice());

                *tb = TypeReference::Abstract(box abstrakt, *s);

                match tb {
                    TypeReference::Abstract(a, s) => a,
                    _ => unreachable!(),
                }
            }
            TypeReference::Abstract(a, s) => {
                panic!("this shouldn't have already been made abstract yet, that was our job!")
            }
        };

        let typ_bases = abstrakt.bases.read().unwrap();

        for base in typ_bases.iter() {
            match base {
                crate::ast::types::TypeBase::Generic(_) => {
                    todo!("we don't yet handle generics")
                }
                crate::ast::types::TypeBase::Resolved(_) => {
                    //todo!("we shouldn't be trying to handle an already resolved typeref")
                    // do nothing here, since maybe someone else resolved it?
                }
                crate::ast::types::TypeBase::UnResolved(r) => {
                    assert!(r.generics.is_empty()); // we don't yet handle generics

                    match self.name_to_ref.contains_key(&r.named) {
                        true => {
                            // do nothing, already gonna resolve it
                        }
                        false => {
                            let convo_id = self.next_convo();

                            let cc = ConversationContext {
                                publish_as: None, // this isn't an import
                                remaining_scope: r.named.clone().scope,
                                original_scope: r.named.clone(),
                                // we search within parent here because we're a typeref within
                                // a Type, which implicitly looks for things within the parent
                                // scope unless we *explicitly* use the Self qualifier
                                searching_within: self.self_ctx.resolve().parent.unwrap(),
                                for_ref_id: convo_id,
                                public: false,
                            };

                            self.name_to_ref.insert(r.named.clone(), convo_id);
                            self.waiting_to_resolve.insert(convo_id, cc.clone());

                            self.step_resolve(convo_id);
                        }
                    };
                }
            }
        }
    }

    async fn thread2(&mut self) {
        // first, export every direct child with their public value
        for child in self.self_ctx.resolve().children.iter() {
            // all direct descendent nodes of the current node are
            // exported here
            let (&symbol, &ctx_id) = child.pair();
            let is_public = ctx_id.resolve().public;
            self.publish(Publish {
                symbol: Some(symbol),
                is_public,
                go_to: ctx_id,
                for_ref_id: Uuid::nil(), // this has no prior conversation, we don't need to hear
                                         // about any resolution, so we provide nil here
            });
        }

        // then, ask for every use statement, saving a note to publish
        // any marked public
        for stmt in self.self_ctx.resolve().use_statements.clone() {
            let UseDeclaration {
                node_info,
                public,
                scope,
                alias,
            } = stmt;

            let convo_id = self.next_convo();

            let cc = ConversationContext {
                publish_as: Some(
                    alias.unwrap_or_else(|| *scope.last().expect("empty scope, with no alias")),
                ),
                remaining_scope: scope.clone(),
                searching_within: self.self_ctx,
                public,
                original_scope: ScopedName {
                    scope: scope.clone(),
                },
                for_ref_id: convo_id,
            };

            self.waiting_to_resolve.insert(convo_id, cc);

            self.step_resolve(convo_id); // ask the question, not waiting for an answer yet
        }
        // then, ask ourselves/our parent for every reference that
        // we use directly (this handles functions within the same
        // outer module being able to call each other without
        // the super:: qualifier)
        let node = self.self_ctx.resolve();
        match &mut *node.inner.lock().unwrap() {
            super::tree::NodeUnion::Type(t) => {
                // we have field types to ask for
                for field in t.fields.iter_mut() {
                    let mut typ = field.has_type.as_mut().expect("all fields must have a typeref");

                    self.start_resolve(&mut typ);
                }
            }
            super::tree::NodeUnion::Function(f) => {
                // we have a return type as well as parameter types

                for pair in f.parameters.iter_mut() {
                    self.start_resolve(&mut pair.1);
                }

                self.start_resolve(&mut f.return_type);

                // start resolving any of the typerefs inside the core expression

                fn rec_traverse(s: &mut Resolver, root: &mut ExpressionWrapper, generics: &[IStr]) {
                    match root {
                        ExpressionWrapper::Cast(c) => {
                            //s.start_resolve(c.typeref.to_abstract(generics).as_ref());
                            s.start_resolve(&mut c.typeref);
                        }
                        ExpressionWrapper::LetExpression(le) => {
                            s.start_resolve(&mut le.constrained_to);
                        }
                        ExpressionWrapper::FunctionCall(_) => todo!(),
                        other => {
                            for child in other.children_mut() {
                                rec_traverse(s, child, generics);
                            }
                        }
                    }
                }

                rec_traverse(
                    self,
                    &mut f.implementation,
                    node.generics
                        .iter()
                        .map(|(s, _)| *s)
                        .collect_vec()
                        .as_slice(),
                );
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
        while let Some(v) = self.listen.recv().await {
            match v {
                Message::Request(r) => {
                    self.handle_question(r).await;
                }
                Message::Reply(r) => match r {
                    Reply::Redirect {
                        reply_from,
                        conversation,
                        within,
                        publish,
                    } => {
                        let c = self.waiting_to_resolve.get_mut(&conversation).unwrap();

                        if let [_, rest @ ..] = c.remaining_scope.as_slice() {
                            c.remaining_scope = rest.to_vec();
                        }

                        c.searching_within = within;

                        self.step_resolve(conversation);
                    }
                    Reply::NotFound {
                        reply_from,
                        conversation,
                        symbol,
                        within,
                        cause,
                    } => todo!(),
                },
                Message::CheckIn() => {
                    println!("got checkin for self {:?}", self.self_ctx);
                }
                Message::Exit() => break,
            }

            if self.waiting_to_resolve.len() == 0 && !self.signed_out {
                self.signed_out = true;
                self.postal.sign_out(self.self_ctx);
            }

            println!(
                "goes back to waiting for messages... self ctx is {:?}",
                self.self_ctx
            );
        }
        // at this point, go back to every type reference and give it a resolution
        // based on those results

        match &mut *self.self_ctx.resolve().inner.lock().unwrap() {
            super::tree::NodeUnion::Type(t) => {
                // things
                //todo!("need to fix types now that we know what they should be")
                for field in t.fields.iter_mut() {
                    let ty = field.has_type.as_mut().unwrap();

                    self.finish_resolve(ty);
                }
            }
            super::tree::NodeUnion::Function(f) => {
                for (_, tr) in f.parameters.iter_mut() {
                    self.finish_resolve(tr)
                }

                self.finish_resolve(&mut f.return_type);

                fn rec_traverse(s: &mut Resolver, root: &mut ExpressionWrapper, generics: &[IStr]) {
                    match root {
                        ExpressionWrapper::Cast(c) => {
                            //s.start_resolve(c.typeref.to_abstract(generics).as_ref());
                            s.finish_resolve(&mut c.typeref);
                        }
                        ExpressionWrapper::LetExpression(le) => {
                            s.finish_resolve(&mut le.constrained_to);
                        }
                        ExpressionWrapper::FunctionCall(_) => todo!(),
                        other => {
                            for child in other.children_mut() {
                                rec_traverse(s, child, generics);
                            }
                        }
                    }
                }

                rec_traverse(
                    self,
                    &mut f.implementation,
                    node.generics
                        .iter()
                        .map(|(s, _)| *s)
                        .collect_vec()
                        .as_slice(),
                );
            }
            super::tree::NodeUnion::Global(_) => todo!(),
            super::tree::NodeUnion::Empty() => {
                // to nothing here for now
            }
        }
    }

    fn finish_resolve(&mut self, tr: &mut TypeReference) {
        let tr = if let TypeReference::Abstract(a, s) = tr {
            a
        } else {
            unreachable!()
        };

        for base in tr.bases.write().unwrap().iter_mut() {
            let resolved = self.base_to_resolved(base);

            println!("Resolves ref {:?} into ref {:?}", base, resolved);

            *base = resolved
        }
    }

    fn base_to_resolved(&mut self, base: &TypeBase) -> TypeBase {
        match base {
            super::types::TypeBase::Generic(_) => todo!("no generics yet"),
            super::types::TypeBase::Resolved(r) => super::types::TypeBase::Resolved(r.clone()),
            super::types::TypeBase::UnResolved(u) => {
                assert!(u.generics.len() == 0);

                let scope = u.named.clone();

                let id = self.name_to_ref.get(&scope).unwrap();

                let published = self.resolved_refs.get(id).unwrap();

                let goes_to = published.go_to;

                TypeBase::Resolved(ResolvedType {
                    from: u.from,
                    base: goes_to,
                    generics: vec![],
                })
            }
        }
    }

    /// Makes a note to self that the given CtxID must provide
    /// the asked-for symbol name
    ///
    /// If the symbol does exist, tell `for_node`  and restart solve on for_node
    fn require(&self, name: IStr, from_node: CtxID, for_node: CtxID) {}
}
