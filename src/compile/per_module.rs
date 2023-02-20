use tokio::task::LocalSet;
//use std::collec
use std::collections::HashMap;

use crate::{ast::{resolver::Resolver, tree::CtxID}, mir::quark::Quark, cst::ScopedName};

pub struct Group {
    resolver: Resolver,
    quark: Quark,
    handler: Handler,
}

impl Group {
    pub async fn thread(mut self) {
        let local = LocalSet::new();

        local.spawn_local(self.resolver.thread());
        local.spawn_local(self.quark.thread());
        local.spawn_local(self.handler.thread());

        local.await
    }

    pub fn send(&self, msg: &Message) {
    }

    pub fn recv(&self, me: &Destination) -> Result<Message, ()> {
        todo!()
    }
}

pub struct Handler {
}

impl Handler {
    pub async fn thread(self) {
    }
}

pub struct Message {
    to: Destination,
    from: Destination,

    reply_to: Destination,

    content: Content,
}

pub enum Content {
    Control(ControlMessage),

    Announce(AnnounceMessage),

    Quark(Photon),

    NameResolution(NameResolutionMessage),
}

pub enum AnnounceMessage {
    PhaseComplete(Service, Phase),
}

pub enum NameResolutionMessage {
    /// Ask the given node, within the context of
    /// its own location in the module tree, what
    /// node the given ScopedName refers to
    WhatIs(ScopedName),

    /// Says that the given ScopedName refers
    /// to the given CtxID
    RefersTo(ScopedName, CtxID),

    /// An announcement/reply from a node stating that
    /// the given ScopedName does not resolve given self
    /// as a root
    HasNoResolution(ScopedName),
}

/// How Quarks qtalk to each other :P
pub enum Photon {
}

pub enum ControlMessage {
}

pub struct Destination {
    node: CtxID,

    service: Service,
}

pub enum Service {
    Resolver(),
    Quark(),

    /// A message specifically for the controller on a node,
    /// used for things like upward phase announcements
    Controller(),

    /// Should be copied and sent to every service on a node
    Broadcast(),
}

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
