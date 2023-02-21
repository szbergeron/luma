use std::collections::{HashMap, HashSet};

use uuid::Uuid;

use crate::{cst::ScopedName, helper::interner::IStr, compile::per_module::Message};

use super::tree::CtxID;

pub struct Resolver {
    /// All of the ongoing conversations
    /// that this resolver is handling
    ///
    /// Conversations that have closed (we issue a term signal,
    /// or get one from the other end) can be removed from here
    conversations: HashMap<Uuid, Conversation>,

    /// The list of conversations that
    /// are currently waiting for a local symbol in order to resolve
    ///
    /// If we have resolved all of our imports and
    /// published all of our local symbols,
    /// then we should tell everyone in this list we
    /// couldn't find the given symbol
    was_asked_for_symbols: HashMap<IStr, Vec<Uuid>>,

    was_asked_for_composites: HashMap<ScopedName, Vec<Uuid>>,

    has_children: HashMap<IStr, Resolution>,

    /// The context that we are handling resolution in the context of
    for_ctx: CtxID,
}

impl Resolver {
    /// We have resolved a local symbol, now figure out
    /// any conversations that can be answered because of it
    ///
    /// If resolution is None, then the symbol was definitively not resolvable
    fn publish(symbol: IStr, resolution: Option<Resolution>) {
    }
}

pub struct Resolution {
    is_public: bool,
    is_at: CtxID,
}

pub struct Conversation {
    id: Uuid,
    messages: Vec<Message>,
}

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
    DoYouHave { symbol: IStr },

    /// A reply to a DoYouHave saying we don't
    IDontHave { symbol: IStr },

    /// A reply to a DoYouHave that says that symbol is at the given CtxID
    /// Also states whether it is public
    ItIsAt {
        symbol: IStr,
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
        given_root: CtxID,
    },
}
