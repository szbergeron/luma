use futures::future::join_all;
use local_channel::mpsc::{Receiver as LocalReceiver, Sender as LocalSender};
use tokio::{
    sync::mpsc::{UnboundedReceiver, UnboundedSender},
    task::LocalSet,
};
//use std::collec
use std::{
    collections::HashMap,
    sync::{atomic::AtomicUsize, Arc},
    time::Duration,
};

use crate::{
    ast::{resolver::Resolver, tree::CtxID},
    avec::AtomicVecIndex,
    cst::ScopedName,
    helper::interner::IStr,
    mir::quark::Quark,
};

pub struct CompilationUnit {
    /// we are solely responsible
    /// for dealing with the nodes within our domain.
    ///
    /// domains try to be clustered so that individual workers
    /// don't trample over each other, and so that
    /// when they "talk" to each other it is at a logical boundary at a parent spot in the tree
    domain: Vec<CtxID>,
}

impl CompilationUnit {
    pub fn new(domain: Vec<CtxID>) -> Self {
        Self { domain }
    }

    pub async fn launch(self) {
        let mut senders = HashMap::new();
        let mut receivers = HashMap::new();

        for c in self.domain.iter() {
            let (send, recv) = tokio::sync::mpsc::unbounded_channel();
            senders.insert(*c, send);
            receivers.insert(*c, recv);
        }

        let postal = Arc::new(Postal::new(senders));

        let v: Vec<Group> = self
            .domain
            .into_iter()
            .map(|cid| {
                let group = Group::new(receivers.remove(&cid).unwrap(), cid, postal.clone());

                group
            })
            .collect();

        println!("about to start watchdog");
        tokio::spawn(Watchdog::new(postal.clone()).run());
        println!("started watchdog");

        join_all(v.into_iter().map(|r| r.start())).await;
    }
}

pub struct Watchdog {
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
pub struct Postal {
    senders: HashMap<CtxID, tokio::sync::mpsc::UnboundedSender<Message>>,
    exited: AtomicUsize,
}

impl Postal {
    pub fn new(senders: HashMap<CtxID, UnboundedSender<Message>>) -> Self {
        Self {
            senders,
            exited: AtomicUsize::new(0),
        }
    }

    /*pub fn send_reply(&self, from: CtxID, to: CtxID, reply: Reply) {
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
    }*/

    pub fn send(&self, msg: Message) {
        todo!()
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
            for (ctxid, sender) in self.senders.iter_mut() {
                //sender.send(Message::Exit()).unwrap();
                sender.send(Message {
                    to: Destination {
                        node: *ctxid,
                        service: Service::Broadcast(),
                    },
                    from: Destination::narrator(),
                    reply_to: Destination::narrator(),
                    content: Content::Control(todo!()),
                });
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
            for (&ctxid, sender) in self.senders.iter_mut() {
                let _ = sender.send(Message {
                    to: Destination {
                        node: ctxid,
                        service: Service::Broadcast(),
                    },
                    from: Destination::narrator(),
                    reply_to: Destination::narrator(),
                    content: Content::Control(ControlMessage::CheckIn()),
                });
            }

            true
        }
    }
}

pub struct Group {
    listen_to: UnboundedReceiver<Message>,
    for_node: CtxID,
    with_postal: Arc<Postal>,
}

impl Group {
    pub fn new(
        listen_to: UnboundedReceiver<Message>,
        for_node: CtxID,
        with_postal: Arc<Postal>,
    ) -> Self {
        Self {
            listen_to,
            for_node,
            with_postal,
        }
    }
    pub async fn start(self) {
        let local = LocalSet::new();

        let (res_s, res_r) = local_channel::mpsc::channel();
        let (qk_s, qk_r) = local_channel::mpsc::channel();
        let (rtr_s, rtr_r) = local_channel::mpsc::channel();
        //let (br_s, br_r) = local_channel::mpsc::channel();
        let resolver = Resolver::for_node(
            self.for_node,
            Earpiece::new(rtr_s.clone(), res_r, self.for_node),
        );
        let quark = Quark::for_node(
            self.for_node,
            Earpiece::new(rtr_s.clone(), qk_r, self.for_node),
        );
        let bridge = Bridge::new(self.listen_to, rtr_s.clone());
        let router = Router::new(
            rtr_r,
            vec![
                (Service::Resolver(), res_s),
                (Service::Quark(), qk_s),
                (Service::Router(), rtr_s.clone()),
            ],
            self.with_postal,
            self.for_node,
        );

        local.spawn_local(resolver.thread());
        local.spawn_local(quark.thread());
        local.spawn_local(router.thread());

        local.spawn_local(bridge.thread());

        local.await
    }

    /*pub fn send(&self, msg: &Message) {}

    pub fn recv(&self, me: &Destination) -> Result<Message, ()> {
        todo!()
    }*/
}

pub struct Bridge {
    listen_to: UnboundedReceiver<Message>,
    send_to: LocalSender<Message>,
}

impl Bridge {
    pub async fn thread(mut self) {
        while let Some(v) = self.listen_to.recv().await {
            let _ = self.send_to.send(v);
        }
    }

    pub fn new(listen_to: UnboundedReceiver<Message>, send_to: LocalSender<Message>) -> Self {
        Self { listen_to, send_to }
    }
}

pub struct Router {
    listen_to: LocalReceiver<Message>,
    send_on: Vec<(Service, LocalSender<Message>)>,
    send_out: Arc<Postal>,
    for_node: CtxID,
}

impl Router {
    pub fn new(
        mut listen_to: LocalReceiver<Message>,
        mut send_on: Vec<(Service, LocalSender<Message>)>,
        send_out: Arc<Postal>,
        for_node: CtxID,
    ) -> Self {
        Self {
            listen_to,
            send_on,
            send_out,
            for_node,
        }
    }

    pub async fn thread(mut self) {
        while let Some(v) = self.listen_to.recv().await {
            if v.to.node == self.for_node {
                // the message is for a local service
                match v.to.service {
                    Service::Broadcast() => {
                        for (serv, send) in self.send_on.iter_mut() {
                            if v.from
                                == (Destination {
                                    service: *serv,
                                    ..v.to
                                })
                            {
                                // don't send a broadcast right back to the thing that sent it
                                continue;
                            } else {
                                let _ = send.send(v.clone());
                            }
                        }
                    }
                    Service::Controller() => {
                        //
                    }
                    other => {
                        let (srv, snd) = self
                            .send_on
                            .iter_mut()
                            .find(|v| v.0 == other)
                            .expect("couldn't find a matching value for service");

                        let _ = snd.send(v);
                    }
                }
            } else {
                // message is going out to another node
                self.send_out.send(v);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Message {
    to: Destination,
    from: Destination,

    reply_to: Destination,

    content: Content,
}

#[derive(Debug, Clone)]
pub enum Content {
    Control(ControlMessage),

    Announce(AnnounceMessage),

    Quark(Photon),

    NameResolution(NameResolutionMessage),
}

#[derive(Debug, Clone)]
pub enum AnnounceMessage {
    PhaseComplete(Service, Phase),
}

#[derive(Debug, Clone)]
pub enum NameResolutionMessage {
    /// Ask the given node, within the context of
    /// its own location in the module tree, what
    /// node the given ScopedName refers to
    WhatIs(ScopedName),

    /// Asks if a node has a given symbol as a direct refer
    DoYouHave(IStr),

    /// A reply to a DoYouHave saying we don't
    IDontHave(IStr),

    /// A reply to a DoYouHave that says that symbol is at the given CtxID
    ItIsAt(IStr, CtxID),

    /// Says that the given ScopedName refers
    /// to the given CtxID
    RefersTo(ScopedName, CtxID),

    /// An announcement/reply from a node stating that
    /// the given ScopedName does not resolve given self
    /// as a root
    HasNoResolution(ScopedName),
}

/// How Quarks qtalk to each other :P
#[derive(Debug, Clone)]
pub enum Photon {}

#[derive(Debug, Clone)]
pub enum ControlMessage {
    /// Analogue for a heartbeat message
    CheckIn(),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Destination {
    pub node: CtxID,

    pub service: Service,
}

impl Destination {
    /// Yields a destination that seems to come from "nowhere",
    /// corresponds to injecting a message from outside the system
    pub fn narrator() -> Self {
        Self {
            node: CtxID(AtomicVecIndex::nil()),
            service: Service::Broadcast(),
        }
    }
}

pub struct Earpiece {
    listen: local_channel::mpsc::Receiver<Message>,

    //shout: Arc<Postal>,
    whisper: local_channel::mpsc::Sender<Message>,

    within: CtxID,
}

impl Earpiece {
    pub fn send(&mut self, message: Message) {
        /*if message.to.node == self.within {
            let _ = self.whisper.send(message);
        } else {
            self.shout.send(message);
        }*/
        let _ = self.whisper.send(message);
    }

    pub async fn wait(&mut self) -> Result<Message, ()> {
        self.listen.recv().await.ok_or(())
    }

    pub fn new(send: LocalSender<Message>, recv: LocalReceiver<Message>, within: CtxID) -> Self {
        Self {
            listen: recv,
            whisper: send,
            within,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Service {
    Resolver(),
    Quark(),
    Router(),

    /// A message specifically for the controller on a node,
    /// used for things like upward phase announcements
    Controller(),

    /// Should be copied and sent to every service on a node
    Broadcast(),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Phase {
    /// Says that initial bringup is complete,
    /// all data structures have been initialized
    /// and we are ready to begin actually dealing
    /// with new messages as they come in
    Init(),

    /// Means that we have finished all of our local resolution,
    /// and have entered a steady-state reply state
    LocallyComplete(),

    /// Means that we have finished shutting down our
    /// message handler, and have finished any work
    /// that we were expected to do for external messages
    GloballyComplete(),
}
