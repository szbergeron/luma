use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt::Debug,
    rc::Rc,
    sync::Arc,
};

use dashmap::{DashMap, DashSet};
use fixed::traits::ToFixed;
use futures::future::join;
use itertools::Itertools;
use tracing::warn;
use uuid::Uuid;

use crate::{
    ast::{
        executor::{Executor, Thunk, UnsafeAsyncCompletable},
        resolver2::NameResolver,
        tree::{CtxID, NodeUnion},
        types::StructuralDataDefinition,
    },
    avec::AtomicVecIndex,
    compile::per_module::{
        Content, ConversationContext, Destination, Earpiece, Message, Postal, Service,
    },
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

    committed_type: Rc<UnsafeAsyncCompletable<ResolvedType>>,
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

    pub fn new(name: IStr) -> Self {
        Self {
            name,
            directs: RefCell::new(Vec::new()),
            indirects: RefCell::new(Vec::new()),
            committed_type: unsafe { UnsafeAsyncCompletable::new() },
        }
    }

    pub fn is_committed(&self) -> bool {
        unsafe { self.committed_type.is_complete() }
    }

    pub fn commit_to(&self, ty: ResolvedType) {
        unsafe {
            self.committed_type
                .complete(ty.clone())
                .expect("already committed?")
        };

        for (dest, conversation) in (&mut *self.indirects.borrow_mut()).swap_with(Vec::new()) {
            Postal::instance().send(Message {
                to: dest,
                from: Destination::mediator(),
                send_reply_to: Destination::nil(),
                conversation,
                content: Content::Transponster(Memo::ResolveIndirectUsage {
                    field: self.name,
                    commits_to: ty.clone(),
                }),
            })
        }
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
        self.indirects.borrow_mut().push((from, conversation));
        /*let mut b = self.indirects.borrow_mut();
         *b = *b + 1;*/
    }

    pub fn add_direct(&self, ty: ResolvedType) {
        self.directs.borrow_mut().push(ty);
    }
}

#[derive(Clone, Debug)]
pub struct InstanceID(uuid::Uuid);

/// This is used later once we start doing things like typeclasses for dynamic fields,
/// so we can allow putting a subtype in with a write and reading a supertype
pub enum UsageDirection {
    Load(),
    Store(),
}

/// This is basically our new TypeVar
#[derive(Clone, Debug)]
pub struct Instance {
    pub id: InstanceID,

    /// The Ctx
    pub instantiated_in: CtxID,

    //pub instantiated_from: Option<CtxID>,
    /// If this was a field/method, this stores the
    /// expression that it was accessed on,
    /// for use later when determining how the function call works
    pub accessed_from: Option<ExpressionID>,

    pub of: InstanceOf,

    pub generics: Rc<HashMap<IStr, TypeID>>,

    pub once_base: Rc<UnsafeAsyncCompletable<CtxID>>, // when we resolve our base, notify this

    pub once_resolved: Rc<UnsafeAsyncCompletable<ResolvedType>>,
}

#[derive(Clone, Debug)]
pub enum InstanceOf {
    Type(InstanceOfType),

    Func(InstanceOfFn),

    /// This is a type that can be passed around but that we can do
    /// nothing but unify with itself
    ///
    /// This allows opaque values to be passed around
    Generic(IStr),

    /// Means this is only named, but never called nor constructed yet
    /// so what it "is" will need to be figured out
    Unknown(),
}

#[derive(Clone)]
pub struct InstanceOfType {
    //regular_fields: Rc<HashMap<IStr, SyntacticTypeReferenceRef>>,

    // since fields type can't be used to j
    pub from: CtxID,
    pub regular_fields: HashMap<IStr, Thunk<TypeID>>,
    pub methods: HashMap<IStr, Thunk<TypeID>>,
    //methods: HashMap<>
}

impl std::fmt::Debug for InstanceOfType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InstanceOfType")
            .field(
                "from",
                &self.from.resolve().canonical_typeref().resolve().unwrap(),
            )
            .finish()
    }
}

#[derive(Clone, Debug)]
pub struct InstanceOfFn {
    parameters: Vec<TypeID>,
    returns: TypeID,

    from: Option<CtxID>,
}

#[derive(Clone, Debug)]
pub struct Unify {
    pub from: TypeID,
    pub into: TypeID,
}

pub enum ValueOrThunk<T: Debug + Clone + 'static> {
    Value(T),
    Thunk(Thunk<T>),
}

impl<T: Debug + Clone + 'static> ValueOrThunk<T> {
    pub async fn to_value(self) -> T {
        match self {
            Self::Value(v) => v,
            Self::Thunk(t) => t.extract().await,
        }
    }
}

pub struct UnifyThunk {
    pub from: ValueOrThunk<TypeID>,
    pub into: ValueOrThunk<TypeID>,
}

impl UnifyThunk {
    pub async fn to_unify(self) -> Unify {
        let (into, from) = join(self.into.to_value(), self.from.to_value()).await;
        Unify { into, from }
    }
}

pub struct Constrain {
    pub ty: TypeID,
    pub is_a: SyntacticTypeReferenceRef,
}

#[derive(Clone, Debug)]
pub struct ArgConversionError {
    pub argument: Option<TypeID>,
    pub parameter: Option<(TypeID, IStr)>,

    pub for_function: CtxID,

    pub comment: IStr,
}

impl Instance {
    #[async_recursion::async_recursion(?Send)]
    pub async fn from_resolved(rty: ResolvedType, within: &'static Quark) -> (Self, Vec<Unify>) {
        let mut generics = Vec::new();

        let mut unify = Vec::new();

        for rt in rty.generics {
            let (inst, mut additional_unify) = Instance::from_resolved(rt, within).await;
            let tid = within.introduce_instance(inst, NodeInfo::Builtin);
            generics.push(tid);

            unify.append(&mut additional_unify);
        }

        let (inst, mut additional_unify) = Self::with_generics(rty.node, within, generics).await;

        unify.append(&mut additional_unify);

        (inst, unify)
    }

    pub fn as_call(
        &self,
        args: Vec<TypeID>,
        expected_return: TypeID,
        generics: Vec<TypeID>,
        within: &Quark,
    ) -> (
        Vec<UnifyThunk>,
        Option<TypeUnificationError>,
        Vec<ArgConversionError>,
    ) {
        // if we are generic, then we need to check whether those generics propagate
        // through to our result, or if our result can be instantiated
        //
        // if we aren't callable, then we should return None

        match &self.of {
            InstanceOf::Type(t) => (
                Vec::new(),
                Some(TypeUnificationError {
                    from: todo!(),
                    from_peers: todo!(),
                    into: todo!(),
                    into_peers: todo!(),
                    for_expression: todo!(),
                    context: todo!(),
                    reason_for_unification: todo!(),
                    reason_for_failure: todo!(),
                }),
                Vec::new(),
            ),
            InstanceOf::Func(f) => {
                let mut unify = Vec::new();
                let mut errors = Vec::new();

                for eb in f.parameters.iter().zip_longest(args.iter()) {
                    match eb {
                        itertools::EitherOrBoth::Both(param, arg) => {
                            //panic!("making a unify");
                            unify.push(UnifyThunk {
                                from: ValueOrThunk::Value(*arg),
                                into: ValueOrThunk::Value(*param),
                            })
                        }
                        itertools::EitherOrBoth::Left(param) => {
                            errors.push(ArgConversionError {
                                argument: None,
                                parameter: Some((*param, "unknown".intern())),
                                for_function: f.from.unwrap(),
                                comment: "the function had a parameter for which an argument was not supplied".intern()
                            })
                        },
                        itertools::EitherOrBoth::Right(arg) => errors.push(ArgConversionError {
                            for_function: f.from.unwrap(),
                            argument: Some(*arg),
                            parameter: None,
                            comment: "the function did not have any additional parameters, this argument is extraneous".intern(),
                        }),
                    }
                }

                unify.push(UnifyThunk {
                    into: ValueOrThunk::Value(f.returns),
                    from: ValueOrThunk::Value(expected_return),
                });

                (unify, None, errors)
            }
            InstanceOf::Generic(_) => todo!("can't call generic"),
            InstanceOf::Unknown() => todo!("tried to interpret an unsolved with an of"),
        }

        /*Err(TypeError {
            components: todo!(),
            complaint: format!("tried to call a type/value that was not callable"),
        })*/
    }
    pub async fn construct_instance(
        base: CtxID,
        field_values: HashMap<IStr, TypeID>,
        provided_generics: Vec<TypeID>,
        within: &'static Quark,
    ) -> (Self, Vec<Unify>) {
        let mut unifies = Vec::new();

        let n = base.resolve();
        //let mut generic_map = HashMap::new();

        let origin = Self::infer_instance(Some(base), within).await;

        if !provided_generics.is_empty() {
            for gen in origin
                .generics
                .clone()
                .iter()
                .zip_longest(provided_generics)
            {
                let ((origin_gen_name, origin_tid), given_tid) = gen
                    .both()
                    .expect("more/fewer generics provided for instance than target type contains");

                unifies.push(Unify {
                    from: given_tid,
                    into: *origin_tid,
                });

                //let base = within.resolve_typeref(syntr, , from_base)

                //generic_map.insert(their_name, our_type);
            }
        }

        if let InstanceOf::Type(t) = &origin.of {
            let all_keys: HashSet<IStr> = t
                .regular_fields
                .keys()
                .copied()
                .chain(field_values.keys().copied())
                .collect();

            for key in all_keys {
                let inst_field_tid = t
                    .regular_fields
                    .get(&key)
                    .cloned()
                    .expect("user provided an extra key");
                let provided_field_tid =
                    field_values.get(&key).copied().expect("user omitted a key");

                unifies.push(Unify {
                    from: provided_field_tid,
                    into: inst_field_tid.extract().await,
                });
            }

            (origin, unifies)
        } else {
            todo!("uhhh")
        }
    }

    pub async fn with_generics(
        base: CtxID,
        within: &'static Quark,
        generics: Vec<TypeID>,
    ) -> (Self, Vec<Unify>) {
        let v = Self::infer_instance(Some(base), within).await;

        let mut unifies = Vec::new();

        for (tid, (name, syntr)) in generics.into_iter().zip(base.resolve().generics.iter()) {
            let tid_of_node = v
                .generics
                .get(name)
                .expect("should be unreachable, these are made here after all");

            unifies.push(Unify {
                from: tid,
                into: *tid_of_node,
            });
        }

        (v, unifies)
    }

    pub async fn infer_instance(base: Option<CtxID>, within: &'static Quark) -> Self {
        let mut inst = Self::plain_instance(within);

        if let Some(v) = base {
            inst.resolve_base(v, within).await;
        }

        inst
    }

    pub fn plain_instance(within: &Quark) -> Self {
        let inst = Instance {
            id: InstanceID(uuid::Uuid::new_v4()),
            //instantiated_from: base,
            instantiated_in: within.node_id,
            of: InstanceOf::Unknown(), // need to figure this out from what we're based on
            generics: Rc::new(HashMap::new()),
            accessed_from: None,
            once_base: unsafe { UnsafeAsyncCompletable::new() },
            once_resolved: unsafe { UnsafeAsyncCompletable::new() },
        };

        unsafe {
            let canary = inst.once_base.clone().wait();
            within.executor.install(
                async move {
                    let v = canary.await;

                    tracing::info!("got a base?? it is: {v:?}");
                },
                "waiting for canary to fire",
            )
        }

        inst
    }

    #[async_recursion::async_recursion(?Send)]
    pub async fn resolve_base(&mut self, base: CtxID, within: &'static Quark) {
        let inst = base.resolve();

        // use inst to get regular_fields from the transponster or something
        let gens_for_type = Rc::new(
            inst.generics
                .iter()
                .map(|(g, r)| (*g, within.new_tid(NodeInfo::Builtin)))
                .collect(),
        );

        unsafe {
            let resolved = self.once_resolved.clone();

            let once_base = self.once_base.clone();

            let generics_unresolved = self.generics.clone();

            within.executor.install(
                async move {
                    let mut generics = Vec::new();
                    for (generic_name, generic_tid) in generics_unresolved.iter() {
                        let gen_ty = within.with_instance(*generic_tid, |instance| {
                            instance.once_resolved.clone().wait()
                        });
                        generics.push(gen_ty.await);
                    }

                    let base = once_base.wait().await;

                    let resolved_ty = ResolvedType {
                        node: base,
                        generics,
                    };

                    resolved.complete(resolved_ty);
                },
                "once we know the full type of a var, finish its once_resolved",
            );
        };

        self.generics = gens_for_type;

        //panic!();

        let of = match &inst.inner {
            NodeUnion::Type(t) => {
                let mut inst_fields = HashMap::new();
                let mut inst_methods = HashMap::new();

                // add generics since we didn't already

                let fields = t.lock().unwrap().fields.clone();

                for field in fields {
                    let fname = field.name;
                    let ty = field.has_type.unwrap();

                    let generics = self.generics.clone(); // rc clone

                    let thunk = unsafe {
                        Thunk::new(within.executor, async move {
                            let tid = within
                                .resolve_typeref(ty, &generics, inst.parent.unwrap())
                                .await;

                            tid
                        })
                    };

                    inst_fields.insert(fname, thunk);
                }

                let methods = t.lock().unwrap().methods.clone();

                for (mname, mref) in methods {
                    let minst_thunk = unsafe {
                        Thunk::new(within.executor, async move {
                            tracing::warn!("we're inferring the instance for field {mname}");
                            let minst = Self::infer_instance(Some(mref), within).await;

                            let minst_tid = within.introduce_instance(minst, NodeInfo::Builtin);

                            tracing::warn!("this instantiation of {mname} on {mref:?} was given tid {minst_tid:?}");

                            minst_tid
                        })
                    };

                    inst_methods.insert(mname, minst_thunk);
                }

                let _ = unsafe { self.once_base.complete(base) }; // we don't yield yet, so this is
                                                                  // fine until ret

                InstanceOf::Type(InstanceOfType {
                    regular_fields: inst_fields,
                    methods: inst_methods,
                    from: base,
                })
            }
            NodeUnion::Function(f, i) => {
                // the typeref there should be resolved, depending on if its a method or not,
                // either the super scope or the super-super scope

                let resolve_within = if f.is_method {
                    inst.parent.unwrap().resolve().parent.unwrap()
                } else {
                    inst.parent.unwrap()
                };

                tracing::warn!("make function res better");
                let _ = unsafe { self.once_base.complete(base) };

                /*let parameters = f.parameters.clone().into_iter().map(|(pname, pty)| unsafe {
                    let generics = self.generics.clone();
                    Thunk::new(within.executor, async move {
                        let tid = within.resolve_typeref(pty, &generics, resolve_within).await;

                        tid
                    })
                });

                let generics = self.generics.clone();
                let returns = unsafe {
                    Thunk::new(within.executor, async move {
                        within
                            .resolve_typeref(f.return_type, &generics, resolve_within)
                            .await
                    })
                };*/

                let mut parameters = Vec::new();

                for (pname, pty) in f.parameters.clone() {
                    let tid = within
                        .resolve_typeref(pty, &self.generics, resolve_within)
                        .await;

                    parameters.push(tid);
                }

                let returns = within
                    .resolve_typeref(f.return_type, &self.generics, resolve_within)
                    .await;

                InstanceOf::Func(InstanceOfFn {
                    parameters,
                    returns,
                    from: Some(base),
                })
            }
            other => {
                todo!("no")
            }
        };

        self.of = of;

    }

    /// allows us to unify two things of known base and say they are the "same type"
    ///
    /// if this returns Ok(), then the vec of TypeIDs is unifications that this says may happen
    pub fn unify_with(
        self,
        stores_into: Instance,
        because: Unify,
        within: &Quark,
    ) -> Result<(Instance, Vec<Unify>), Vec<TypeError>> {
        //panic!("wooo");

        tracing::info!(
            "unifying two instances! the resulting iid will be {:?}",
            self.id
        );

        assert!(self.instantiated_in == stores_into.instantiated_in);

        /*if let (Some(a), Some(b)) = (self.instantiated_from, stores_into.instantiated_from) {
            if self.instantiated_from != stores_into.instantiated_from {
                tracing::error!(
                    "we don't allow typeclasses yet, so treat unequal ctx for assignment as type error, if this changes make sure to update below so we compare all fields"
                );
                return Err(TypeError {
                    components: todo!(),
                    complaint: todo!(),
                });
            }
        }*/

        tracing::error!("typeclass doohickey");

        let all_generic_keys: HashSet<IStr> = self
            .generics
            .keys()
            .into_iter()
            .copied()
            .chain(stores_into.generics.keys().into_iter().copied())
            .collect();

        let mut all_generic_unifies = Vec::new();

        let mut unified_generics = HashMap::new();

        for key in all_generic_keys {
            let a = self.generics.get(&key);
            let b = stores_into.generics.get(&key);

            let v = match (a, b) {
                (None, None) => None,
                (Some(&t), None) | (None, Some(&t)) => Some(t),
                (Some(&a), Some(&b)) => {
                    if a != b {
                        let unify = Unify { from: a, into: b };
                        all_generic_unifies.push(unify);
                    }

                    Some(a)
                }
            };

            if let Some(v) = v {
                unified_generics.insert(key, v);
            }
        }

        let new_of = match (self.of, stores_into.of) {
            (InstanceOf::Type(ta), InstanceOf::Type(tb)) => {
                // unify two type instances
                //let field_keys = ta.regular_fields.keys().copied().chain(tb.regular_fields.keys().copied()).collect();
                for k in self.generics.keys() {
                    tracing::info!("looking at generic {k}");
                    let from = self.generics.get(k).copied().unwrap();
                    let into = stores_into.generics.get(k).copied().unwrap();

                    all_generic_unifies.push(Unify { from, into });
                }

                InstanceOf::Type(ta)
            }

            (InstanceOf::Func(fa), InstanceOf::Func(fb)) => {
                // unify two func calls
                todo!("unify two function calls")
            }

            (InstanceOf::Unknown(), other) | (other, InstanceOf::Unknown()) => other,

            (other_a, other_b) => {
                panic!("we tried to unify a function call with a structural type? our type system doesn't allow this");
            }
        };

        let accessed_from = match (self.accessed_from, stores_into.accessed_from) {
            (None, None) => {
                // the most obvious case,
                None
            }
            (Some(v), None) => {
                // this is fine, we're assigning into a field (probably)
                tracing::info!("what");
                None
            }
            (None, Some(v)) => {
                tracing::info!("no");
                None
            }
            (Some(a), Some(b)) => {
                // this means it's an instance of `a.b = c.d`,
                // which is fine, it just means we don't have a sensical
                // "accessed from" anymore unless we formally
                // define the semantics of what comes out of
                // an assignment, value-wise (it's an rval, right?
                // it can't be a method with a sensical callee, right?)
                None
            }
        };

        let (conflicted_a, once_base) = unsafe {
            UnsafeAsyncCompletable::combine(
                within.executor,
                self.once_base,
                stores_into.once_base,
                |va, vb| {
                    tracing::warn!("we're unifying id {va:?} with id {vb:?} in ctx");
                    if va != vb {
                        //panic!("user tried to unify two different types")
                        let first = va.resolve().canonical_typeref().resolve().unwrap();
                        let second = vb.resolve().canonical_typeref().resolve().unwrap();

                        let msg = format!("can not assign a value of type {first:?} into a value of type {second:?}");

                        Err(TypeError {
                            components: vec![because.from, because.into],
                            complaint: msg,
                        })
                    } else {
                        Ok(())
                    }
                },
            )
        };

        let (conflicted_b, once_resolved) = unsafe {
            UnsafeAsyncCompletable::combine(
                within.executor,
                self.once_resolved,
                stores_into.once_resolved,
                |va, vb| {
                    tracing::warn!("we're unifying id {va:?} with id {vb:?} in ctx");
                    if va != vb {
                        let msg = format!(
                            "can not assign a value of type {va:?} into a value of type {vb:?}"
                        );

                        Err(TypeError {
                            components: vec![because.from, because.into],
                            complaint: msg,
                        })
                    } else {
                        Ok(())
                    }
                },
            )
        };

        let i = Instance {
            id: InstanceID(uuid::Uuid::new_v4()),
            instantiated_in: self.instantiated_in,
            generics: Rc::new(unified_generics),
            of: new_of,
            accessed_from,
            once_base,
            once_resolved, // since this is all dependent on the sets, we don't need to
                           // do any fancy unify and can just pick one
        };

        match (conflicted_a, conflicted_b) {
            (Some(Err(a)), Some(Err(b))) => Err(vec![b]),
            (Some(Err(e)), _) | (_, Some(Err(e))) => Err(vec![e]),
            _ => Ok((i, all_generic_unifies)),
        }
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
        commits_to: ResolvedType,
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
                            self.dynamic_fields
                                .borrow_mut()
                                .entry(of)
                                .or_insert_with(|| DynFieldInfo::new(of))
                                .add_indirect(m.send_reply_to, m.conversation);
                        }
                        Memo::NotifyDirectUsage { of, as_ty } => {
                            self.dynamic_fields
                                .borrow_mut()
                                .entry(of)
                                .or_insert_with(|| DynFieldInfo::new(of))
                                .add_direct(as_ty);
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
        let candidates = self.get_candidates();

        for (field, score) in candidates {
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
