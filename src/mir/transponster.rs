use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt::Debug,
    rc::Rc,
    sync::Arc,
};

use dashmap::DashMap;
use futures::future::join;
use itertools::Itertools;
use tracing::warn;
use uuid::Uuid;

use crate::{
    ast::{
        executor::{Executor, Thunk, UnsafeAsyncCompletable},
        tree::{CtxID, NodeUnion},
        types::StructuralDataDefinition,
    },
    compile::per_module::{Content, ConversationContext, Destination, Earpiece, Message, Postal},
    cst::{NodeInfo, SyntacticTypeReferenceRef},
    errors::TypeUnificationError,
    helper::{
        interner::{IStr, Internable},
        SwapWith,
    },
};

use super::{
    expressions::ExpressionID,
    quark::{Quark, ResolvedType, TypeError, TypeID},
};

/// Yes, it's a reference, and no, it's not a good one
///
/// This handles the work of doing inference on dynamic fields
/// given a type. It uses a stepped model, where all assignments
/// to a field are grouped based on staged visibility.
///
/// Quark on every node first runs a round, and tells
/// Transponster what assignments were made to any fields
/// on it within the initial round where the type of the
/// source expression could be fully inferred.
/// Quark also tells a transponster about every time
/// a field is assigned into it of unknown type,
/// and if a dynamic field is read it tells the
/// source transponster that the field was read
///
/// These pieces of information build a value flow
/// graph within the transponsters, and allow
/// for selecting root fields where the most
/// information is known about their type,
/// and where the fewest unknowns are assigned into them.
///
/// This is a potentially "unstable" (though deterministic
/// for any specific textual input) metric, so care must be
/// taken to craft not specifically the most accurate metric
/// (to provide as comprehensive inference as we could) but
/// rather to provide the most predictable and stable metric,
/// where we may group types and pull roots concurrently
/// even if there is a specific root that could provide
/// information for dependents
pub struct Transponster {
    for_ctx: CtxID,
    earpiece: Earpiece,

    generics: Vec<IStr>,

    //resolutions: HashMap<FieldID, >,

    //assignments: HashMap<FieldID, Vec<TypeVar>>,
    //dynamic_field_contexts: HashMap<FieldID, FieldContext>,
    /// If it's in here, then we've committed to a type for that
    /// variant/field and it can be used for inference
    dynamic_fields: RefCell<HashMap<IStr, DynFieldInfo>>,

    notify_when_resolved: HashMap<FieldID, Vec<(Destination, FieldID)>>,

    regular_fields: HashMap<IStr, SyntacticTypeReferenceRef>,

    conversations: ConversationContext,
    //// When we get a new instance of something in Quark, we put
    //// the instance in here. This allows us to nicely do unification for generics
    //// for any type, since the type itself handles doing that unification
    //// and Quark only has to handle detecting value flow and doing function
    //// type unification
    //instances: HashMap<InstanceID, Instance>,
}

pub type Score = fixed::types::I16F16;

pub struct DynFieldInfo {
    name: IStr,

    directs: RefCell<Vec<ResolvedType>>,

    /// Holds a list of conversation handles to reply to once we resolve the indirects
    indirects: RefCell<Vec<(Destination, Uuid)>>,

    committed_type: Rc<UnsafeAsyncCompletable<Option<ResolvedType>>>,

    executor: &'static Executor,
}

/*impl std::default::Default for DynFieldInfo {
    fn default() -> Self {
        Self {
            directs: Default::default(),
            indirects: RefCell::new(Vec::new()),
            committed_type: unsafe { UnsafeAsyncCompletable::new() },
        }
    }
}*/

#[derive(Debug)]
pub enum ScoreInfo {
    /// This should not be committed in its current state
    Never(),

    /// This should be immediately committed
    Now(),

    /// This is a bid, but is not eligible for immediate commit
    Maybe(Score),
}

impl DynFieldInfo {
    pub fn current_score(&self) -> ScoreInfo {
        if self.is_committed() {
            ScoreInfo::Never()
        } else if self.directs.borrow().is_empty() {
            ScoreInfo::Never()
        } else {
            let indirects = self.indirects.borrow();
            let directs_count = self.directs.borrow().len();

            if indirects.len() == directs_count {
                ScoreInfo::Now() // no indirects, some directs, this can be committed *now*
            } else {
                println!(
                    "have {directs_count} directs and {} indirects",
                    indirects.len()
                );

                let mut distinct_directs = HashMap::new();

                for direct in self.directs.borrow().iter().cloned() {
                    *distinct_directs.entry(direct).or_insert(0) += 1;
                }

                let mut flat = distinct_directs.into_iter().collect_vec();
                flat.sort_by_key(|(rty, count)| *count);

                let indirect_count = indirects.len() as f64;
                let total_directs = self.directs.borrow().len() as f64;

                let commit_to_direct_count = flat.last().as_ref().expect("no last?").1 as f64;

                let disagreeing_direct_count = (total_directs - commit_to_direct_count) as f64;

                let score = (commit_to_direct_count * 2.0)
                    - (indirect_count - disagreeing_direct_count * 2.0);

                let score = Score::from_num(score);

                ScoreInfo::Maybe(score)
            }
        }
    }

    pub fn new(name: IStr, within: &'static Executor) -> Self {
        Self {
            name,
            directs: RefCell::new(Vec::new()),
            indirects: RefCell::new(Vec::new()),
            committed_type: unsafe { UnsafeAsyncCompletable::new() },
            executor: within,
        }
    }

    pub fn is_committed(&self) -> bool {
        unsafe { self.committed_type.is_complete() }
    }

    pub fn commit_to(&self, ty: ResolvedType) {
        unsafe {
            self.committed_type
                .complete(Some(ty.clone()))
                .expect("already committed?")
        };

        for (dest, conversation) in (&mut *self.indirects.borrow_mut()).swap_with(Vec::new()) {
            // do nothing now, this is handled by completable
        }
    }

    pub fn commit_fail(&self) {
        unsafe { self.committed_type.complete(None).expect("tried to commit fail a completed field") };
    }

    pub fn commit(&self) -> ResolvedType {
        let mut counts = HashMap::new();

        for ty in self.directs.borrow().clone().into_iter() {
            let ent = counts.entry(ty).or_insert(Score::from_num(1.0));
            *ent = *ent + Score::from_num(1.0);
        }

        if counts.len() > 1 {
            tracing::error!("type error when conflicting field types");
        }

        let mut ordered = counts.into_iter().collect_vec();
        ordered.sort_by_key(|(ty, count)| *count);

        let (commits_to, _count) = ordered.last().cloned().expect("it was just here");

        self.commit_to(commits_to.clone());

        commits_to
    }

    pub fn add_indirect(&self, from: Destination, conversation: Uuid) {
        tracing::debug!("adds an indirect for field {}", self.name);
        self.indirects.borrow_mut().push((from, conversation));

        unsafe {
            let waits = self.committed_type.clone().wait();
            let name = self.name;
            self.executor.install(
                async move {
                    Postal::instance().send(Message {
                        to: from,
                        from: Destination::mediator(),
                        send_reply_to: Destination::nil(),
                        conversation,
                        content: Content::Transponster(Memo::ResolveIndirectUsage {
                            field: name,
                            commits_to: waits.await,
                        }),
                    })
                },
                "once a dynfield is resolved, notify the other end",
            )
        };

        /*let mut b = self.indirects.borrow_mut();
         *b = *b + 1;*/
    }

    pub fn add_direct(&self, ty: ResolvedType) {
        tracing::debug!("adds a direct for field {}", self.name);
        self.directs.borrow_mut().push(ty);
    }
}

#[derive(Debug, Clone)]
pub enum Memo {
    /// AnnounceCommit is used when a source says they have
    /// directs and have committed to a given type
    AnnounceCommit {
        original: Arc<AnnounceCommit>,
        for_field: FieldID,
    },

    NotifyIndirectUsage {
        of: IStr,
    },

    ResolveIndirectUsage {
        field: IStr,
        commits_to: Option<ResolvedType>,
    },

    NotifyDirectUsage {
        of: IStr,
        as_ty: ResolvedType,
    },

    /// Tells us to commit a field that we gave as a candidate
    InstructCommit {
        field: IStr,
    },

    /// When Quark stalls and the system settles,
    /// we need to collaborate and run an election on a field to commit
    BeginPhase(),

    /// If you are sent this, it means you asked a while ago
    /// about a field and we didn't know what type it was
    ///
    /// We now know the type, or know that it can not be resolved, so
    /// you can ask us again for more information
    CheckAgain {
        field: FieldID,
    },

    /// A quark can tell the transponster for its own node that
    /// it is a callable, so the transponster can respond to call
    /// format queries
    NotifySelfCallable {
        /// the context that any generics in the params/returns
        /// operate in if there are any generic args
        generics: HashMap<IStr, SyntacticTypeReferenceRef>,

        params: Vec<(IStr, SyntacticTypeReferenceRef)>,
        returns: SyntacticTypeReferenceRef,
    },

    CompilationStalled(),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AnnounceCommit {
    from_source: FieldID,
    commits_to: ResolvedType,
    weights: Vec<(fixed::types::I16F16, ResolvedType)>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct FieldID {
    name: IStr,
    on: CtxID,
    ///// This is a field on a struct parameterized by
    ///// the given typevar. If the struct this is on
    ///// is not generic, this is empty
    //parameterized_by: Vec<ResolvedType>,
}

lazy_static! {
    static ref MEDIATOR: Mediator = Mediator::new();
}

pub struct Mediator {
    pub candidates: DashMap<CtxID, Vec<(IStr, ScoreInfo)>>,
}

impl Mediator {
    pub fn instance() -> &'static Self {
        &MEDIATOR
    }

    fn new() -> Self {
        Self {
            candidates: DashMap::new(),
        }
    }

    pub fn add_candidate(&self, from: CtxID, field: IStr, score: ScoreInfo) {
        self.candidates
            .entry(from)
            .or_default()
            .push((field, score));
    }

    pub fn reset(&self) {
        self.candidates.clear();
    }

    /// If this commits anything, then true is returned
    pub fn run_election(&self) -> bool {
        let mut immediate = Vec::new();
        let mut otherwise = Vec::new();

        for pair in self.candidates.iter() {
            let (&ctx, candidates) = pair.pair();

            for (fname, score) in candidates.iter() {
                match score {
                    ScoreInfo::Never() => unreachable!("we got a candidate that is useless"),
                    ScoreInfo::Now() => immediate.push((ctx, *fname)),
                    ScoreInfo::Maybe(v) => {
                        otherwise.push((ctx, *fname, *v));
                    }
                }
            }
        }

        self.reset();

        if !immediate.is_empty() {
            // we have immediates we can apply
            for (cid, name) in immediate {
                println!("Committing immediate {name} in {cid:?}");
                Postal::instance().send(Message {
                    to: Destination::transponster(cid),
                    from: Destination::mediator(),
                    send_reply_to: Destination::nil(),
                    conversation: Uuid::new_v4(),
                    content: Content::Transponster(Memo::InstructCommit { field: name }),
                })
            }

            true
        } else if !otherwise.is_empty() {
            otherwise.sort_by_key(|(cid, name, score)| *score);

            let (cid, field, _) = otherwise
                .last()
                .expect("otherwise should be nonempty")
                .clone();

            Postal::instance().send(Message {
                to: Destination::transponster(cid),
                from: Destination::mediator(),
                send_reply_to: Destination::nil(),
                conversation: Uuid::new_v4(),
                content: Content::Transponster(Memo::InstructCommit { field }),
            });

            true
        } else {
            false
        }
    }

    /*pub fn as_dest() -> Destination {
        Destination {
            node: CtxID(AtomicVecIndex::nil()),
            service: Service::Mediator(),
        }
    }*/
}

impl Transponster {
    pub fn as_dest(&self) -> Destination {
        Destination::transponster(self.for_ctx)
    }

    pub fn get_candidates(&self) -> Vec<(IStr, ScoreInfo)> {
        let mut immediates = Vec::new();
        let mut suboptimals = Vec::new();

        for (&name, field) in self.dynamic_fields.borrow().iter() {
            let score = field.current_score();

            match score {
                ScoreInfo::Never() => {
                    // do nothing yet, these will eventually be a hard error on stall
                }
                ScoreInfo::Now() => {
                    immediates.push((name, score));
                }
                ScoreInfo::Maybe(_) => {
                    suboptimals.push((name, score));
                }
            }
        }

        if !immediates.is_empty() {
            immediates
        } else {
            suboptimals
        }
    }

    pub fn for_node(node_id: CtxID, earpiece: Earpiece) -> Self {
        let regular_fields = HashMap::new();

        let cc_send = earpiece.cloned_sender();

        let generics = node_id
            .resolve()
            .generics
            .clone()
            .into_iter()
            .map(|(a, b)| a)
            .collect_vec();

        Self {
            for_ctx: node_id,
            earpiece,

            //dynamic_field_contexts: HashMap::new(),
            dynamic_fields: RefCell::new(HashMap::new()),

            notify_when_resolved: HashMap::new(),

            regular_fields,
            generics,
            conversations: unsafe { ConversationContext::new(cc_send) },
            //instances: todo!(), // TODO: generics
        }
    }

    pub async fn entry(mut self, executor: &'static Executor, sd: &mut StructuralDataDefinition) {
        let parent_id = self.for_ctx.resolve().parent.unwrap();

        for field in sd.fields.iter_mut() {
            self.regular_fields.insert(
                field.name,
                field.has_type.expect("field didn't have a type?"),
            );

            warn!("transponster resolved a field type");
        }

        for mref in self.for_ctx.resolve().children.iter() {
            let (&name, child) = mref.pair();
            self.regular_fields
                .insert(name, child.resolve().canonical_typeref());
        }

        while let Ok(m) = self.earpiece.wait().await {
            if let Some(m) = self.conversations.dispatch(m) {
                match m.content {
                    Content::Transponster(memo) => match memo {
                        Memo::NotifyIndirectUsage { of } => {
                            let mut refm = self.dynamic_fields.borrow_mut();
                            let ent = refm.entry(of).or_insert_with(|| DynFieldInfo::new(of, executor));

                            ent.add_indirect(m.send_reply_to, m.conversation);
                        }
                        Memo::NotifyDirectUsage { of, as_ty } => {
                            let mut refm = self.dynamic_fields.borrow_mut();
                            let ent = refm.entry(of).or_insert_with(|| DynFieldInfo::new(of, executor));
                            ent.add_direct(as_ty);
                        }
                        Memo::ResolveIndirectUsage { field, commits_to } => {
                            unreachable!("shouldn't get this ourselves")
                        }
                        Memo::BeginPhase {} => {
                            self.run_phase();
                        }
                        Memo::InstructCommit { field } => {
                            //panic!("committing field {field}");
                            let rty = self
                                .dynamic_fields
                                .borrow()
                                .get(&field)
                                .expect("got a commit instruction for a field we don't know about")
                                .commit();
                        }
                        Memo::CompilationStalled() => {
                            for (name, info) in self.dynamic_fields.borrow().iter() {
                                if !info.is_committed() {
                                    info.commit_fail();
                                }
                            }
                        }
                        _ => todo!(),
                    },
                    Content::Quark(_) => todo!(),
                    _ => unreachable!("shouldn't receive anything else"),
                }
            }
        }

        tracing::error!("add iter of methods/children of the type");
    }

    pub fn run_phase(&mut self) {
        tracing::debug!("running a transponster phase");

        let candidates = self.get_candidates();

        for (field, score) in candidates {
            tracing::debug!("adding a candidate: {field}, {score:?}");
            Mediator::instance().add_candidate(self.for_ctx, field, score);
        }
    }

    /// If a certainty for the field *can* be computed,
    /// then a set of the typevars that it could be are returned, each
    /// paired with how likely that var is to be the case
    pub async fn compute_certainty(&mut self, field: FieldID) -> Option<Vec<(!, f64)>> {
        todo!("probably not using this approach in particular");
    }

    pub async fn thread(self, executor: &'static Executor) {
        warn!("starting transponster");

        let node = &self.for_ctx.resolve().inner;

        match node {
            crate::ast::tree::NodeUnion::Type(t) => {
                let mut sdd = t.lock().unwrap().clone();
                self.entry(executor, &mut sdd).await;
            }
            _other => {
                warn!("Transponster shuts down since this node type wasn't a Type")
                //
            }
        }

        tracing::error!("possible memory corruption risk, should not get here");
    }
}
