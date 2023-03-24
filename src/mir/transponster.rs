use std::{
    collections::{HashMap, HashSet},
    fmt::{format, Debug},
    rc::Rc,
    sync::Arc,
};

use fixed::traits::ToFixed;
use futures::future::{join, join_all};
use itertools::Itertools;
use tracing::{instrument::WithSubscriber, warn};

use crate::{
    ast::{
        executor::{Executor, Thunk, UnsafeAsyncCompletable},
        resolver2::NameResolver,
        tree::{CtxID, NodeUnion},
        types::StructuralDataDefinition,
    },
    compile::per_module::{Content, ConversationContext, Destination, Earpiece, Message, Service},
    cst::{NodeInfo, SyntacticTypeReferenceRef},
    errors::{self, TypeUnificationError},
    helper::interner::{IStr, Internable},
};

use super::{
    expressions::{AssignmentDirection, ExpressionID},
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
    dynamic_field_contexts: HashMap<FieldID, FieldContext>,

    /// If it's in here, then we've committed to a type for that
    /// variant/field and it can be used for inference
    dynamic_fields: HashMap<FieldID, Rc<UnsafeAsyncCompletable<PortableTypeVar>>>,

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

    pub notify: Rc<UnsafeAsyncCompletable<CtxID>>, // when we resolve our base, notify this
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

    /*
    pub async fn typeof_regular_field(&self, field: IStr, within: &mut Quark) -> Option<TypeID> {
        if let InstanceOf::Type(InstanceOfType {
            regular_fields,
            methods: regular_methods,
            from,
        }) = &self.of
        {
            match &regular_fields.get(&field) {
                None => None,
                Some(tid) => Some(tid.clone().extract().await),
            }
        } else {
            todo!("user tried to get a field on a function type")
        }
    }

    /// If we already have a type for the DynField, then return it here
    pub fn typeof_dynamic_field(&self, field: IStr) -> Option<ResolvedType> {
        todo!()
    }*/

    /*

    /// the direction here says, if Load then "the field is loaded from into something of TID
    /// <with_tid>, and the inverse if direction is Store
    pub fn use_field(
        &self,
        field: IStr,
        with_tid: TypeID,
        direction: UsageDirection,
    ) -> (UsageHandle, Vec<Unify>) {
        let mut unify = Vec::new();

        match &self.of {
            InstanceOf::Type(t) => {
                if let Some(&field_type) = t.regular_fields.get(&field) {
                    todo!("neat");
                    let u = match direction {
                        UsageDirection::Load() => Unify {
                            from: field_type,
                            into: with_tid,
                        },
                        UsageDirection::Store() => Unify {
                            from: with_tid,
                            into: field_type,
                        },
                    };

                    unify.push(u);

                    (UsageHandle(uuid::Uuid::new_v4()), unify)
                //} else if let Some(pair) = t.m
                } else {
                    // this is a dynamic field
                    todo!("dynamic fields")
                }
            }
            InstanceOf::Func(f) => todo!("unify function calls"),
            InstanceOf::Generic(g) => {
                todo!("what now?")
            }
            InstanceOf::Unknown() => todo!("rip"),
        }
    }

    /// allows us to add a direct
    /// if the field was already resolved, this returns a TypeError describing the
    /// mismatch
    pub fn resolve_usage(
        &self,
        usage: UsageHandle,
        direction: UsageDirection,
        to_type: ResolvedType,
    ) -> Result<(), TypeError> {
        todo!()
    }

    /*pub fn use_field(
        &self,
        name: IStr,
    ) -> UnsafeAsyncCompletableFuture<T> {
    }*/

*/

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
            notify: unsafe { UnsafeAsyncCompletable::new() },
        };

        unsafe {
            let canary = inst.notify.clone().wait();
            within.executor.install(
                async move {
                    let v = canary.await;

                    tracing::error!("got a base?? it is: {v:?}");
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

        let of = match &inst.inner {
            NodeUnion::Type(t) => {
                let mut inst_fields = HashMap::new();
                let mut inst_methods = HashMap::new();

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

                let _ = unsafe { self.notify.complete(base) }; // we don't yield yet, so this is
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
                let _ = unsafe { self.notify.complete(base) };

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

        self.generics = gens_for_type;
    }

    /// allows us to unify two things of known base and say they are the "same type"
    ///
    /// if this returns Ok(), then the vec of TypeIDs is unifications that this says may happen
    pub fn unify_with(
        self,
        stores_into: Instance,
        because: Unify,
        within: &Quark,
    ) -> Result<(Instance, Vec<Unify>), TypeError> {
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

        let (conflicted, notify) = unsafe {
            UnsafeAsyncCompletable::combine(
                within.executor,
                self.notify,
                stores_into.notify,
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

        let i = Instance {
            id: InstanceID(uuid::Uuid::new_v4()),
            instantiated_in: self.instantiated_in,
            generics: Rc::new(unified_generics),
            of: new_of,
            accessed_from,
            notify,
        };

        if let Some(Err(e)) = conflicted {
            Err(e)
        } else {
            Ok((i, all_generic_unifies))
        }
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
pub struct UsageHandle(uuid::Uuid);

/// We can potentially extend this into supporting HKTs
/// by allowing generics to be provided on an opaque TypeID
///
/// though, that will make actually doing the stepped
/// dyn field solve a lottttt harder
#[derive(Debug, Clone)]
pub enum PortableTypeVar {
    UnPortable(TypeID),
    /// a free type that should be matched to a new TypeID within the solver system
    Free(),
    Instantiation(Arc<Instantiation>),
}

#[derive(Debug, Clone)]
pub struct Instantiation {
    /// eventually will make this support constraints as alternatives on it
    /// instead of just either a TypeID that was passed through or fully
    /// unconstrained
    generics: HashMap<IStr, PortableTypeVar>,

    of_base: CtxID,
}

struct FieldContext {
    for_field: FieldID,

    //// map of <Origin, Recipient>
    //forwarded_broadcasts: HashMap<FieldID, HashSet<FieldID>>,
    //// Map<Origin Field, Map<Local Field, (Received Count, Decided Type, Decision Weights)>>
    //received_broadcasts: HashMap<FieldID, HashMap<FieldID, (usize, ResolvedType, Vec<(f32, ResolvedType)>)>>,
    received_broadcasts: HashSet<Arc<AnnounceCommit>>,

    usages: HashSet<UsageHandle>,

    resolutions: HashMap<UsageHandle, ResolvedType>,
}

impl FieldContext {
    pub fn new_for_field(for_field: FieldID) -> Self {
        Self {
            for_field,
            received_broadcasts: HashSet::new(),
            usages: HashSet::new(),
            resolutions: HashMap::new(),
        }
    }

    /// After any given pass, this takes what we know
    /// about the usage of this field and tries to
    /// put a number on how sure we are that
    /// we are the type we think, and thus how good a
    /// candidate we are for committing
    ///
    /// Any number over 0.95 should imply we are a candidate
    /// for immediate commit, without trying to do an election
    pub fn calculate_certainty(&self) -> fixed::types::I16F16 {
        // simply take how many direct usages we have and compare that
        // to how many unknowns we have, for now
        //
        // we may want to include an additional biasing operation later
        // where we get more confident with more directs even
        // with a lower ratio
        let usage_count = self.usages.len() as f64;
        let resolution_count = self.resolutions.len() as f64;

        let ratio = if usage_count > 0.0 {
            resolution_count / usage_count
        } else {
            // if we have no usages, we have no certainty and no type we could possibly be
            0.0
        };

        ratio.to_fixed()
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

    AskFieldType {
        instantiation: Arc<Instantiation>,
        field: FieldID,
    },

    ReplyFieldType {
        new_instantiation: Arc<Instantiation>,
    },

    /// If you are sent this, it means you asked a while ago
    /// about a field and we didn't know what type it was
    ///
    /// We now know the type, or know that it can not be resolved, so
    /// you can ask us again for more information
    CheckAgain { field: FieldID },

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

impl Transponster {
    pub fn as_dest(&self) -> Destination {
        Destination::transponster(self.for_ctx)
    }

    pub fn for_node(node_id: CtxID, earpiece: Earpiece) -> Self {
        let regular_fields = HashMap::new();

        let cc_send = earpiece.cloned_sender();

        Self {
            for_ctx: node_id,
            earpiece,

            dynamic_field_contexts: HashMap::new(),
            dynamic_fields: HashMap::new(),

            notify_when_resolved: HashMap::new(),

            regular_fields,
            generics: Vec::new(),
            conversations: unsafe { ConversationContext::new(cc_send) },
            //instances: todo!(), // TODO: generics
        }
    }

    pub fn is_regular_field(&self, field: IStr) -> bool {
        self.regular_fields.contains_key(&field)
    }

    /// If the instantiation didn't
    /// allow us to resolve the type of the field directly,
    /// then we return the determining TypeID
    ///
    /// We may wait to resolve until we have committed to a type
    /// in the case that we will *eventually* have enough information
    /// to make that determination
    pub async fn wait_solve_field(
        &mut self,
        field: IStr,
        on: Arc<Instantiation>,
    ) -> PortableTypeVar {
        if let Some(v) = self.regular_fields.get(&field) {
            // turn the STR into a lowered form according to the types in the Instantiation
            match v.resolve().unwrap().inner.clone() {
                crate::cst::SyntacticTypeReferenceInner::Single { name } => {
                    if let [one] = name.scope.as_slice() && self.generics.contains(one) {
                        //PortableTypeVar::UnPortable(on.generics[one].clone())
                        on.generics[one].clone()
                    } else {
                        let nr = NameResolver {
                            name,
                            based_in: self.for_ctx.resolve().parent.unwrap(),
                            reply_to: self.for_ctx,
                            service: Service::Oracle()
                        };

                        let resolved = nr
                            .using_context(&self.conversations)
                            .await;
                        let resolved = resolved.expect("need to handle unresolved names");

                        // no generics since this is just a single (the other ones get more
                        // complicated)
                        PortableTypeVar::Instantiation(Arc::new(Instantiation {
                            generics: HashMap::new(),
                            of_base: resolved,
                        }))
                    }
                }
                _ => todo!("other kinds of STR"),
            }
        } else {
            // could be a dynamic field, so this is a new usage
            unsafe {
                let f = self
                    .dynamic_fields
                    .entry(FieldID {
                        name: field,
                        on: self.for_ctx,
                    })
                    .or_insert(UnsafeAsyncCompletable::new());

                f.clone().wait().await
            }
        }
    }

    pub async fn entry(mut self, executor: &'static Executor, sd: &mut StructuralDataDefinition) {
        let parent_id = self.for_ctx.resolve().parent.unwrap();

        for field in sd.fields.iter_mut() {
            /*let tres = SymbolResolver {
                node_id: self.for_ctx,
                earpiece: &mut self.earpiece,
                for_service: crate::compile::per_module::Service::Oracle(),
            }
            .resolve(field.has_type.as_mut().unwrap())
            .await;*/
            //let ft = field.has_type.as_ref().expect("no type?");
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

        tracing::error!("add iter of methods/children of the type");
    }

    fn field_handle_mut(
        fields: &mut HashMap<FieldID, FieldContext>,
        field: FieldID,
    ) -> &mut FieldContext {
        fields.entry(field.clone()).or_insert(FieldContext {
            for_field: field,
            received_broadcasts: HashSet::new(),
            usages: HashSet::new(),
            resolutions: HashMap::new(),
        })
    }

    fn field_handle(&mut self, field: FieldID) -> &FieldContext {
        self.dynamic_field_contexts
            .entry(field.clone())
            .or_insert(FieldContext {
                for_field: field,
                received_broadcasts: HashSet::new(),
                usages: HashSet::new(),
                resolutions: HashMap::new(),
            })
    }

    /*pub async fn wait_for(&mut self, field: FieldID) -> Result<TypeVar, !> {
        todo!()
    }*/

    pub async fn use_field(&mut self, field: FieldID) -> UsageHandle {
        assert!(field.on == self.for_ctx, "not our field");

        if let Some(v) = self.regular_fields.get(&field.name) {
            // we had a regular field for it, maybe tie in generics here?
            todo!()
        } else {
            // it must be a generic field then
            let usage_id = UsageHandle(uuid::Uuid::new_v4());

            usage_id
        }
    }

    pub async fn resolve_usage(
        &mut self,
        field: FieldID,
        usage: UsageHandle,
        value_type: ResolvedType,
    ) {
        assert!(
            field.on == self.for_ctx,
            "if it isn't on self, then it isn't a direct"
        );

        let field_context = Self::field_handle_mut(&mut self.dynamic_field_contexts, field);

        assert!(
            field_context.usages.contains(&usage),
            "we don't have a matching usage for this"
        );

        let prior = field_context.resolutions.insert(usage, value_type);

        assert!(
            prior.is_none(),
            "they already resolved this usage, why are they doing it again"
        );
    }

    /*pub fn receive_broadcast(&mut self, ac: Arc<AnnounceCommit>, for_field: FieldID) {
        let sd = self.as_dest();
        let fc = Self::field_handle_mut(&mut self.dynamic_field_contexts, for_field);

        let already_received = fc.received_broadcasts.insert(ac.clone());

        if !already_received {
            // we then forward it along
            for f in fc.assigns_into.iter() {
                Self::send_announce_to(sd, &mut self.earpiece, ac.clone(), f.clone());
            }
        }
    }*/

    pub fn send_announce_to(
        from: Destination,
        ep: &mut Earpiece,
        ac: Arc<AnnounceCommit>,
        to: FieldID,
    ) {
        ep.send(Message {
            to: Destination::transponster(to.on),
            from,
            send_reply_to: from,
            conversation: uuid::Uuid::new_v4(),
            content: Content::Transponster(Memo::AnnounceCommit {
                original: ac,
                for_field: to,
            }),
        })
    }

    pub async fn emit_broadcasts(&mut self) {
        let self_as_dest = self.as_dest();

        for (fid, fc) in self.dynamic_field_contexts.iter_mut() {
            let directs = fc.resolutions.iter();

            let mut counts: HashMap<ResolvedType, fixed::types::I16F16> = HashMap::new();

            for (usage, resolution) in directs {
                *counts.entry(resolution.clone()).or_insert(0.0.to_fixed()) +=
                    (1.0).to_fixed::<fixed::types::I16F16>();
            }

            let mut weights = Vec::new();

            let len = counts.len() as f32;

            for (ty, count) in counts.into_iter() {
                // this can't be a div by zero since len must have been > 0 to get into this loop
                weights.push((count / len.to_fixed::<fixed::types::I16F16>(), ty));
            }

            if weights.len() > 1 {
                todo!("emit a type error, we have conflicting assignments");
            }

            if !weights.is_empty() {
                // we get to choose one and do a broadcast
                //weights.sort_unstable_by_key(|(a, b)| *a);
                let (weight, ty) = weights
                    .iter()
                    .max_by_key(|(a, b)| *a)
                    .expect("wasn't empty, but no max?")
                    .clone();

                tracing::info!("node {:?} commits to type {ty:?}", self.for_ctx);

                let announce = Arc::new(AnnounceCommit {
                    from_source: *fid,
                    commits_to: ty,
                    weights,
                });

                for (dest, target_field) in
                    self.notify_when_resolved.remove(fid).unwrap_or(Vec::new())
                {
                    let m = Memo::AnnounceCommit {
                        original: announce.clone(),
                        for_field: target_field,
                    };

                    let m = Message {
                        to: dest,
                        from: self_as_dest,
                        send_reply_to: Destination::nil(),
                        conversation: uuid::Uuid::new_v4(),
                        content: Content::Transponster(m),
                    };

                    self.earpiece.send(m);
                }

                // now the last one
            }
        }
    }

    /*pub async fn add_usage(&mut self, field: FieldID, value_type: TypeVar) {
        assert!(
            field.on == self.for_ctx,
            "can only add a usage where the field is on self, otherwise it's nonsense"
        );

        match value_type.current {
            TypeType::Resolved(box rt) => {
                //self.add_direct(field, rt).await;
                let fc = self.dynamic_field_contexts.entry(field).or_insert(FieldContext::new_for_field(field));

                fc.resolutions
            }
            TypeType::Symbolic(box sy) => {
                // we need to add an indirect here and set a waiter on it
            }
            TypeType::Refer(_) => {
                panic!("transponster was given a refer but should have been given a root")
            }
            TypeType::Unknown() => {
                panic!("what")
            }
        }
    }*/

    /// Takes a field on self and says
    pub async fn add_indirect(
        &mut self,
        on_self: FieldID,
        on_other: FieldID,
        _direction: AssignmentDirection,
    ) {
    }

    /// Called every time the quark phase finishes, so for each field we have
    /// that isn't yet sealed that we have new directs on, we can seal them
    /// and commit to a type
    pub async fn end_quark_phase(&mut self) {}

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
                self.entry(executor, &mut t.lock().unwrap()).await;
            }
            _other => {
                warn!("Transponster shuts down since this node type wasn't a Type")
            }
        }

        tracing::error!("possible memory corruption risk, should not get here");
    }

    pub async fn ask_type_of(
        &mut self,
        field: FieldID,
        notify_later: Destination,
    ) -> Result<!, ()> {
        todo!()
    }
}
