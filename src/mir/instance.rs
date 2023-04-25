use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt::Debug,
    rc::Rc,
    sync::Arc,
};

use dashmap::DashMap;
use either::Either;
use futures::future::join;
use itertools::{EitherOrBoth, Itertools};
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
        interner::{IStr, Internable, SpurHelper},
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

    pub generics: Rc<Vec<(IStr, TypeID)>>,

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

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Generic {
    pub ties_to: IStr,
    pub is_a: TypeID,
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

        for resolved_generic in rty.generics.clone() {
            let (resolved_generic_inst, mut additional_unify) =
                Instance::from_resolved(resolved_generic.clone(), within).await;

            let tid = within.introduce_instance(resolved_generic_inst, NodeInfo::Builtin, false);

            generics.push(tid);

            tracing::warn!(
                "marks {tid:?} (gen param) as equivalent to tr {:?}",
                resolved_generic
                    .node
                    .resolve()
                    .canonical_typeref()
                    .resolve()
                    .unwrap()
            );

            unify.append(&mut additional_unify);
        }

        let (inst, mut additional_unify) = Self::with_generics(rty.node, within, generics).await;

        unify.append(&mut additional_unify);

        unsafe {
            tracing::warn!(
                "From_resolved resolved fut is {}, {}",
                inst.once_base.is_complete(),
                inst.once_resolved.is_complete()
            );
        }

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
                    from: expected_return,
                    from_peers: vec![],
                    into: expected_return,
                    into_peers: vec![],
                    for_expression: NodeInfo::Builtin,
                    context: vec![],
                    reason_for_unification: "".intern(),
                    reason_for_failure: "tried to interpret a regular type as a callable".intern(),
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

                tracing::warn!("adding unify based on key {key}");

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
        let mut basic = Self::plain_instance(within);

        let mut unifies = Vec::new();

        basic.resolve_base(base, within).await;

        if basic.generics.len() != generics.len() {
            // need to build out actual generics
            println!("User mismatched generics for type");

            println!(
                "Base is {base:?}, gens are {:?}, but the unresolved ones are
                         {:?}, the base is {:?}",
                generics,
                basic.generics,
                base.resolve().canonical_typeref().resolve().unwrap()
            );
            println!(
                "issue: no generics provided for something that needs them,
                         so we can't complete once_resolved through this"
            );
        } else {
            // unify each generic tid with the corresponding one from the other side

            basic
                .generics
                .iter()
                .copied()
                .zip(generics.into_iter())
                .for_each(|((name, a), b)| {
                    tracing::warn!("Unifying {a:?} and {b:?} since they share a generic {name}");

                    unifies.push(Unify { from: a, into: b })
                });
        }

        (basic, unifies)
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
            generics: Rc::new(Vec::new()),
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
        let base_node = base.resolve();

        tracing::warn!(
            "resolve_base resolving to exactly {:?}",
            base.resolve().canonical_typeref().resolve().unwrap()
        );

        assert!(
            self.generics.is_empty(),
            "should only be resolving base on a clean inst"
        );

        let tidd_generics = base_node.generics.iter().copied().map(|(name, stref)| {
            (
                name,
                within.new_tid(
                    NodeInfo::Builtin,
                    format!("a plain generic tid for gen {name} and base {base:?}"),
                    false,
                ),
            )
        });

        let tidd_generics = Rc::new(tidd_generics.collect_vec());

        self.generics = tidd_generics;

        unsafe {
            self.once_base
                .complete(base)
                .expect("someone else got here first?");

            // do the resolved completion once generics are completed
            let gens_inner = self.generics.clone();

            let once_resolved_inner = self.once_resolved.clone();

            within.executor.install(
                async move {
                    let mut resolved = Vec::new();
                    for (name, tid) in gens_inner.iter().copied() {
                        let r = within.resolved_type_of(tid).await;
                        resolved.push(r);
                    }

                    let resolved = ResolvedType {
                        node: base,
                        generics: resolved,
                    };

                    once_resolved_inner
                        .complete(resolved)
                        .expect("should be the only one completing this");
                },
                "complete self once all generics are completed",
            )
        }

        let of = match &base_node.inner {
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
                            tracing::warn!("looking for type of method now");
                            let tid = within
                                .resolve_typeref(
                                    ty,
                                    &generics.iter().copied().collect(),
                                    base_node.parent.unwrap(),
                                    false,
                                )
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

                InstanceOf::Type(InstanceOfType {
                    regular_fields: inst_fields,
                    methods: inst_methods,
                    from: base,
                })
            }
            NodeUnion::Generic(gn) => {
                tracing::warn!("Got a generic by {gn}");
                InstanceOf::Generic(*gn)
            }
            NodeUnion::Function(f, _) => {
                // the typeref there should be resolved, depending on if its a method or not,
                // either the super scope or the super-super scope

                let resolve_within = if f.is_method {
                    base_node.parent.unwrap().resolve().parent.unwrap()
                } else {
                    base_node.parent.unwrap()
                };

                let mut parameters = Vec::new();

                for pi in f.parameters.clone() {
                    let pname = pi.name;
                    let pty = pi.typ;
                    let tid = within
                        .resolve_typeref(
                            pty,
                            &self.generics.iter().copied().collect(),
                            resolve_within,
                            false,
                        )
                        .await;

                    parameters.push(tid);
                }

                let returns = within
                    .resolve_typeref(
                        f.return_type,
                        &self.generics.iter().copied().collect(),
                        resolve_within,
                        false,
                    )
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
    ) -> Result<(Instance, Vec<Unify>), TypeError> {
        let mut unifies = Vec::new();

        tracing::info!(
            "unifying two instances! the resulting iid will be {:?}",
            self.id
        );

        assert!(self.instantiated_in == stores_into.instantiated_in);

        tracing::error!("typeclass doohickey");

        let mut new_generics = Vec::new();

        for ent in self
            .generics
            .iter()
            .copied()
            .zip_longest(stores_into.generics.iter().copied())
        {
            match ent {
                EitherOrBoth::Both((name_a, tid_a), (name_b, tid_b)) => {
                    assert_eq!(name_a, name_b);
                    unifies.push(Unify {
                        from: tid_a,
                        into: tid_b,
                    });

                    new_generics.push((name_a, tid_a));
                }
                EitherOrBoth::Left((name_a, tid_a)) => {
                    tracing::warn!("// should this happen?");
                    new_generics.push((name_a, tid_a));
                }
                EitherOrBoth::Right((name_a, tid_a)) => {
                    tracing::warn!("// should this happen?");
                    new_generics.push((name_a, tid_a));
                }
            }
        }

        let new_of = match (self.of, stores_into.of) {
            (InstanceOf::Type(ta), InstanceOf::Type(tb)) => {
                tracing::warn!("do this proper later");
                if ta.from != tb.from {
                    let te = TypeError {
                        components: vec![],
                        complaint: format!("original reason for unification was {reason}, tried to unify a {:?} with a {:?}", ta.from.resolve().canonical_typeref().resolve().unwrap(), tb.from.resolve().canonical_typeref().resolve().unwrap()),
                        because_unify: because,
                    };
                    return Err(te);
                } else {
                    InstanceOf::Type(ta)
                }

                //within.add_type_error(TypeError { components: , complaint: (), because_unify: () })
                //assert_eq!(ta.from, tb.from);
            }

            (InstanceOf::Func(fa), InstanceOf::Func(fb)) => {
                // unify two func calls
                todo!("unify two function calls")
            }

            (InstanceOf::Unknown(), other) | (other, InstanceOf::Unknown()) => other,

            (InstanceOf::Func(f), InstanceOf::Type(t))
            | (InstanceOf::Type(t), InstanceOf::Func(f)) => {
                println!("we tried to unify a function call with a structural type? our type system doesn't allow this");

                let te = TypeError {
                    because_unify: because,
                    complaint: format!("Original reason for unification was {reason} tried to unify a {:?} with {:?}",
                                       t.from.resolve().canonical_typeref().resolve().unwrap(), f.from.map(|f| f.resolve().canonical_typeref().resolve().unwrap())),
                    components: vec![],
                };

                return Err(te);
            }
            _ => todo!(),
        };

        let once_base = unsafe {
            UnsafeAsyncCompletable::combine(
                within.executor,
                self.once_base,
                stores_into.once_base,
                move |va, vb| {
                    tracing::warn!("we're unifying id {va:?} with id {vb:?} in ctx");
                    //panic!("user tried to unify two different types")
                    if va != vb {
                        let first = va.resolve().canonical_typeref().resolve().unwrap();
                        let second = vb.resolve().canonical_typeref().resolve().unwrap();

                        let msg = format!("can not assign a value of type {first:?} into a value of type {second:?}");

                        within.add_type_error(TypeError {
                            components: vec![because.from, because.into],
                            complaint: msg,
                            because_unify: because,
                        });
                    }
                },
            )
        };

        let once_resolved = unsafe {
            let orig = UnsafeAsyncCompletable::new();

            let complete_once_resolved = orig.clone();
            let once_base_inner = once_base.clone();
            let new_generics_inner = new_generics.clone();

            within.executor.install(
                async move {
                    let base = once_base_inner.wait().await; // make sure we have the base

                    let mut resolved_ones = Vec::new();

                    for (name, tid) in new_generics_inner {
                        let rt = within.resolved_type_of(tid).await;

                        resolved_ones.push(rt);
                    }

                    let rt = ResolvedType {
                        node: base,
                        generics: resolved_ones,
                    };

                    complete_once_resolved
                        .complete(rt.clone())
                        .expect("this is top down");
                },
                "unified completion future",
            );

            orig
        };

        let i = Instance {
            id: InstanceID(uuid::Uuid::new_v4()),
            instantiated_in: self.instantiated_in,
            generics: Rc::new(new_generics),
            of: new_of,
            accessed_from: None,
            once_base,
            once_resolved, // since this is all dependent on the sets, we don't need to
                           // do any fancy unify and can just pick one
        };

        Ok((i, unifies))
    }

    pub fn notify_monomorphizations(&self) {
        let resolved = unsafe {
            self.once_resolved
                .try_get()
                .expect("tried to check monomorphization on an incomplete thing")
        };

        let ctx = resolved.node;

        match &self.of {
            InstanceOf::Type(t) => {
                debug_assert_eq!(t.from.resolve().generics.len(), resolved.generics.len());
                debug_assert_eq!(resolved.generics.len(), t.from.resolve().generics.len());
                Postal::instance().send_and_forget(
                    Destination::transponster(ctx),
                    Content::Monomorphization(Monomorphization::from_resolved(resolved)),
                );
            }
            InstanceOf::Func(f) => {
                Postal::instance().send_and_forget(
                    Destination::quark(ctx),
                    Content::Monomorphization(Monomorphization::from_resolved(resolved)),
                );
            }
            InstanceOf::Generic(_) => {
                // noop, generics don't monomorphize
            }
            InstanceOf::Unknown() => {
                println!("Just how *did* we get here???");
            }
        }
    }
}
