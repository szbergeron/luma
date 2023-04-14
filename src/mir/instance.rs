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
    errors::{CompilationError, TypeUnificationError},
    helper::{
        interner::{IStr, Internable},
        SwapWith,
    },
};

use super::{
    expressions::ExpressionID,
    quark::{Quark, ResolvedType, TypeError, TypeID},
    scribe::Monomorphization,
};

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

#[derive(Clone, Debug, Copy)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
            let tid = within.introduce_instance(inst, NodeInfo::Builtin, false);
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
                .map(|(g, r)| (*g, within.new_tid(NodeInfo::Builtin, false)))
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
                                .resolve_typeref(ty, &generics, inst.parent.unwrap(), false)
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

                            let minst_tid =
                                within.introduce_instance(minst, NodeInfo::Builtin, false);

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
            NodeUnion::Function(f, _) => {
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
                        .resolve_typeref(pty, &self.generics, resolve_within, false)
                        .await;

                    parameters.push(tid);
                }

                let returns = within
                    .resolve_typeref(f.return_type, &self.generics, resolve_within, false)
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
        reason: IStr,
        within: &'static Quark,
    ) -> (Instance, Vec<Unify>) {
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

        let once_base = unsafe {
            UnsafeAsyncCompletable::combine(
                within.executor,
                self.once_base,
                stores_into.once_base,
                move |va, vb| {
                    tracing::warn!("we're unifying id {va:?} with id {vb:?} in ctx");
                    if va != vb {
                        //panic!("user tried to unify two different types")
                        let first = va.resolve().canonical_typeref().resolve().unwrap();
                        let second = vb.resolve().canonical_typeref().resolve().unwrap();

                        let msg = format!("can not assign a value of type {first:?} into a value of type {second:?}");

                        within.add_type_error(TypeError {
                            components: vec![because.from, because.into],
                            complaint: msg,
                            because_unify: because,
                        });
                    } else {
                    }
                },
            )
        };

        let once_resolved = unsafe {
            UnsafeAsyncCompletable::combine(
                within.executor,
                self.once_resolved,
                stores_into.once_resolved,
                move |va, vb| {
                    tracing::warn!("we're unifying id {va:?} with id {vb:?} in ctx");
                    if va != vb {
                        let msg = format!(
                            "can not assign a value of type {va:?} into a value of type {vb:?}"
                        );

                        within.add_type_error(TypeError {
                            components: vec![because.from, because.into],
                            complaint: msg,
                            because_unify: because,
                        });
                    } else {
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

        (i, all_generic_unifies)
    }

    pub fn notify_monomorphizations(&self) {
        let resolved = unsafe {
            self.once_resolved
                .try_get()
                .expect("tried to check monomorphization on an incomplete thing")
        };

        match &self.of {
            InstanceOf::Type(t) => {
                Postal::instance().send_and_forget(
                    Destination::transponster(resolved.node),
                    Content::Monomorphization(Monomorphization::from_resolved(resolved)),
                );
            }
            InstanceOf::Func(f) => {
                Postal::instance().send_and_forget(
                    Destination::quark(resolved.node),
                    Content::Monomorphization(Monomorphization::from_resolved(resolved)),
                );
            }
            _ => panic!("shouldn't call monomorphization on anything other than type or func"),
        }
    }
}
