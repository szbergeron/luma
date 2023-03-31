use crate::{
    compile::per_module::StalledDog,
    errors::{CompilationError, TypeUnificationError},
};
use std::{cell::RefCell, collections::HashMap, pin::Pin, rc::Rc};

use tracing::{info, warn};
use uuid::Uuid;

use crate::{
    ast::{
        self, executor::Executor, resolver2::NameResolver, tree::CtxID, types::FunctionDefinition,
    },
    compile::per_module::{
        Content, ControlMessage, ConversationContext, Destination, Earpiece, Message, Service,
    },
    cst::{
        self, ExpressionWrapper, NodeInfo, ScopedName, SyntacticTypeReferenceInner,
        SyntacticTypeReferenceRef,
    },
    helper::interner::{IStr, Internable},
    mir::{
        expressions::{AnyExpression, Bindings, Composite, ExpressionContext, Literal, VarID},
        transponster::InstanceOf,
    },
};

use super::transponster::InstanceID;
use super::{
    expressions::ExpressionID,
    sets::Unifier,
    transponster::{Instance, Unify, UsageHandle},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TypeID(uuid::Uuid, cst::NodeInfo);

impl TypeID {
    pub fn span(&self) -> NodeInfo {
        self.1
    }
}

#[derive(Clone, Debug)]
pub struct TypeError {
    pub components: Vec<TypeID>,
    pub complaint: String,
}

/// Quark instances are provided a single
/// function context to try to resolve within,
/// and convert that function into a fully
/// linearized, SSA form callable with types
/// fully resolved from a static-compilation
/// perspective
///
/// Inputs for types are TypeConstraints,
/// while outputs are references to actual Nodes
/// with IDs and filled in generics
pub struct Quark {
    acting_on: RefCell<ExpressionContext>,

    type_of_var: RefCell<HashMap<VarID, TypeID>>,

    /// If we resolve a TypeID from here, we should tell any usages/instances
    /// that there is a new direct
    dynfield_notifies: RefCell<HashMap<TypeID, Vec<(InstanceID, UsageHandle)>>>,

    /// If we've resolved a type far enough to know that it is an Instance,
    /// it is recorded here so we can do field analysis on it
    pub instances: RefCell<Unifier<TypeID, Instance>>,

    once_know: RefCell<HashMap<TypeID, Vec<Action>>>,

    //once_know_ctx: RefCell<HashMap<TypeID, Rc<UnsafeAsyncCompletable<CtxID>>>>,
    sender: local_channel::mpsc::Sender<Message>,

    pub executor: &'static Executor,

    generics: Rc<HashMap<IStr, TypeID>>,

    pub node_id: CtxID,

    pub type_errors: RefCell<ErrorState>,

    conversations: ConversationContext,
}

pub struct ErrorState {
    pub is_tainted: bool,

    pub errors_to_be_flushed: Vec<CompilationError>,
}

impl ErrorState {
    pub fn new() -> Self {
        Self {
            is_tainted: false,
            errors_to_be_flushed: vec![],
        }
    }

    pub fn push(&mut self, e: CompilationError) {
        self.is_tainted = true;
        self.errors_to_be_flushed.push(e);
    }
}

pub enum Action {
    LoadFieldInto(IStr, TypeID),
    StoreFieldFrom(IStr, TypeID),

    /// Provide Vec<Parameters> as .0, the return value is stored into .1
    CallWith(Vec<TypeID>, TypeID),

    /// Says the type being operated on is being casted to <TypeID>
    ///
    /// This doesn't really give us a *lot* of back-info,
    /// but does allow us to verify the cast at this time
    CastTo(TypeID),

    /// Says that we should take the resolution
    /// we got for the given TypeID and give it to
    /// this Instance which may open up additional opportunities
    /// to resolve things
    ResolveUsage(InstanceID, UsageHandle),
}

impl Quark {
    pub fn add_unify<R: Into<IStr>>(&self, from: TypeID, into: TypeID, reason: R) {
        // this could be batched later for perf improvement, but for now just
        // do an increment every time
        StalledDog::nudge_quark();

        let reason: IStr = reason.into();
        tracing::debug!("unifies {from:?} into {into:?} because {reason}");
        tracing::warn!("borrows instances for add_unify");
        let mut refm = self.instances.borrow_mut();
        let mut resulting_unifies = Vec::new();

        let res = refm.unify(from, into, |a, b| {
            tracing::info!("having to merge two instances");
            let original_a = a.clone();
            match a.unify_with(b, Unify { from, into }, self) {
                Ok((v, u)) => {
                    resulting_unifies = u;
                    Ok(v)
                }
                Err(e) => Err(e),
            }
        });
        //.expect("user did a type error");
        match res {
            Err(e) => {
                let TypeError {
                    components,
                    complaint,
                } = e;
                // collect the peers of each component
                let e = TypeUnificationError {
                    from,
                    from_peers: refm.peers_of(from),
                    into,
                    into_peers: refm.peers_of(into),
                    for_expression: NodeInfo::Builtin,
                    reason_for_unification: reason,
                    reason_for_failure: complaint.intern(),
                    context: Vec::new(),
                };

                let e = CompilationError::TypeError(e);

                self.type_errors.borrow_mut().push(e);

                return; // don't attempt any malformed descendent unifies
            }
            Ok(_) => {
                // do nothing, add the resulting unifies
            }
        }

        // this add_unify could also call it
        std::mem::drop(refm);

        for Unify { from, into } in resulting_unifies {
            self.add_unify(from, into, "resulting unify from an instance add".intern());
        }

        tracing::debug!("New status for sets:");
    }

    /// When each quark phase completes we get one message here to flush errors
    /// before switching over to Transponster
    fn end_one_phase(&self) {
        let errors = self.type_errors.borrow_mut();

        if errors.is_tainted {}
    }

    /// If compilation fully stalls then this is called,
    /// to let us emit final errors or prepare Scribe to output our
    /// monomorphizations
    fn end_last_phase(&self) {
        let errors = self.type_errors.borrow_mut();

        if errors.is_tainted {
            // we got a type error, so we shouldn't
            // emit any complaints about un-restrained variables
        } else {
            let free_vars = self.instances.borrow().gather_free();

            if free_vars.len() > 0 {
                let b = self.instances.borrow();

                //println!("Instances: {b:#?}");
                println!("Unrestricted vars: {}", free_vars.len());
            }
        }
    }

    fn as_dest(&self) -> Destination {
        Destination {
            node: self.node_id,
            service: Service::Quark(),
        }
    }

    /// turn a typeref into a bunch of `stuff`
    ///
    /// this is how we take typerefs within fields on instances
    /// and try to make them symbolic within some Quark context
    ///
    /// this turns it into either an instance or a TypeID, depending on how well things go
    #[async_recursion::async_recursion(?Send)]
    pub async fn resolve_typeref(
        &'static self,
        tr: SyntacticTypeReferenceRef,
        with_generics: &HashMap<IStr, TypeID>,
        from_base: CtxID,
    ) -> TypeID {
        let trv = tr.resolve().unwrap();
        match trv.inner {
            SyntacticTypeReferenceInner::Single { name } => {
                // we are maybe an instance of a type? or some node? so figure out which one it
                // is
                tracing::info!("resolving nr {name:?}");
                let nr = NameResolver {
                    name: name.clone(),
                    based_in: from_base,
                    reply_to: self.node_id,
                    service: Service::Quark(),
                };
                let r = nr.using_context(&self.conversations).await;

                //panic!("resolved using nr");

                match r {
                    Ok(cid) => {
                        tracing::info!("constructs an instance for single {name:?}, and it pointed to {cid:?} for a simple typeref");
                        let instance = Instance::infer_instance(Some(cid), self).await;

                        tracing::debug!("tr base is: {:?}", instance.of);

                        // don't set an accessed_from for this, since it's a plain function
                        // (shouldn't have a propagated `self`)

                        //let tid = self.new_tid();

                        let tid = self.introduce_instance(instance, trv.info);

                        tracing::debug!("introduced {tid:?}");

                        tid
                    }
                    Err(_) => {
                        todo!("need to handle import errors with Instances");

                        // once we've propagated the error, we just treat this as unconstrained
                        // to let us find as many errors as possible
                        let tid = self.new_tid(trv.info);

                        tid
                    }
                }
            }
            SyntacticTypeReferenceInner::Unconstrained() => {
                tracing::warn!("constructing completely unconstrained typeref, need to validate that or something");
                let tid = self.new_tid(trv.info);

                tid
            }
            SyntacticTypeReferenceInner::Generic { label } => {
                //async_backtrace::
                println!("Generics: {:?}", with_generics);
                let existing_tid = with_generics
                    .get(&label)
                    .expect("got a generic that didn't mean anything in the context");
                *existing_tid
            }
            SyntacticTypeReferenceInner::Tuple(t) => {
                todo!("need to actually make tuple types")
            }
            SyntacticTypeReferenceInner::Parameterized { name, generics } => {
                let base = NameResolver {
                    name: name.clone(),
                    based_in: from_base,
                    reply_to: self.node_id,
                    service: Service::Quark(),
                };
                let base = base.using_context(&self.conversations).await;

                let base = match base {
                    Err(e) => {
                        tracing::error!("report import errors");
                        return self.new_tid(NodeInfo::Builtin);
                    }
                    Ok(v) => v,
                };

                let mut resolved_generics = Vec::new();

                for generic in generics {
                    let resolved = self
                        .resolve_typeref(generic.intern(), with_generics, from_base)
                        .await;

                    resolved_generics.push(resolved);
                }

                let (inst, unify) = Instance::with_generics(base, self, resolved_generics).await;

                for Unify { from, into } in unify {
                    self.add_unify(
                        from,
                        into,
                        "constructing instance with generics made these ties",
                    );
                }

                self.introduce_instance(inst, trv.info)
            }
            SyntacticTypeReferenceInner::Reference { to, mutable } => {
                let nr = NameResolver {
                    name: ScopedName::from_many("std::pointers::reference"),
                    based_in: from_base,
                    reply_to: self.node_id,
                    service: Service::Quark(),
                };

                todo!()

                //let ty = SyntacticTypeReferenceInner::
            }
            SyntacticTypeReferenceInner::Pointer { to, mutable } => {
                todo!("handle pointers in quark")
            }
        }
    }

    pub fn with_instance<F, R>(&self, tid: TypeID, f: F) -> R
    where
        F: FnOnce(&Instance) -> R,
    {
        tracing::warn!("borrows instances for with_instance");
        let mut refm = self.instances.borrow_mut();
        if let Some(v) = refm.v_for(tid) {
            f(v)
        } else {
            let new_tid = TypeID(Uuid::new_v4(), tid.span());
            let instance = Instance::plain_instance(self);

            refm.add_kv(new_tid, instance);

            // unify needs to borrow instances
            std::mem::drop(refm);

            self.add_unify(
                new_tid,
                tid,
                "identity unify since no instance existed for that tid",
            );

            tracing::warn!("borrows instances for with_instance after unify");
            let refm = self.instances.borrow_mut();
            let v = f(refm.v_for(new_tid).unwrap());

            v
        }

        // refm drops
    }

    /// Returns the direct TID for the instance once unified with the base tid
    pub fn assign_instance(&self, tid: TypeID, instance: Instance) -> TypeID {
        // make a new tid inst here since the regular one automatically adds it to unifier
        let instance_tid = TypeID(uuid::Uuid::new_v4(), tid.span());

        tracing::warn!("borrows instances for assign_instance");
        let mut refm = self.instances.borrow_mut();

        refm.add_kv(instance_tid, instance);

        std::mem::drop(refm);

        self.add_unify(
            tid,
            instance_tid,
            format!("we added an instance for {tid:?}"),
        );

        instance_tid
    }

    /// Use this to introduce an instance with a completely untethered TID
    pub fn introduce_instance(&self, instance: Instance, span: NodeInfo) -> TypeID {
        let instance_tid = TypeID(Uuid::new_v4(), span);

        tracing::warn!("borrows instances for introduce_instance");
        let mut refm = self.instances.borrow_mut();

        refm.add_kv(instance_tid, instance);

        instance_tid
    }

    pub fn new_tid(&self, for_span: NodeInfo) -> TypeID {
        let tid = TypeID(uuid::Uuid::new_v4(), for_span);

        tracing::warn!("borrows instances for new_tid");
        self.instances.borrow_mut().add_k(tid);

        tracing::debug!("creates tid {tid:?}");

        tid
    }

    pub fn is_method(&self) -> bool {
        match &self.node_id.resolve().inner {
            ast::tree::NodeUnion::Function(fd, _) => fd.is_method,
            _ => unreachable!(),
        }
    }

    pub fn search_within(&self) -> CtxID {
        if self.is_method() {
            self.node_id
                .resolve()
                .parent
                .unwrap()
                .resolve()
                .parent
                .unwrap()
        } else {
            self.node_id.resolve().parent.unwrap()
        }
    }

    pub fn for_node(
        node_id: CtxID,
        sender: local_channel::mpsc::Sender<Message>,
        executor: &'static Executor,
    ) -> Self {
        warn!("quark is being improperly initialized to make things happy");
        let cs = sender.clone();

        let mut generics = HashMap::new();
        for (gname, gtype) in node_id.resolve().generics.iter() {
            tracing::error!("ignoring constraints on generics for now");
            let g_tid = TypeID(uuid::Uuid::new_v4(), NodeInfo::Builtin);

            generics.insert(*gname, g_tid);
        }

        Self {
            type_errors: RefCell::new(ErrorState::new()),
            sender,
            node_id,
            acting_on: RefCell::new(ExpressionContext::new_empty()),
            type_of_var: RefCell::new(HashMap::new()),
            executor,
            dynfield_notifies: RefCell::new(HashMap::new()),
            instances: RefCell::new(Unifier::new()),
            once_know: RefCell::new(HashMap::new()),
            conversations: unsafe { ConversationContext::new(cs) },
            generics: Rc::new(generics),
        }
    }

    pub fn do_the_thing(&'static self, on_id: ExpressionID) -> TypeID {
        //let root_type_id = self.new_tid();
        let root_type_id = self.do_the_thing_rec(on_id, false);

        root_type_id
    }

    /// lval is true if we are being assigned into, otherwise we can be treated as an rval
    pub fn do_the_thing_rec(&'static self, on_id: ExpressionID, is_lval: bool) -> TypeID {
        tracing::info!("do_the_thing_rec on id: {on_id}");

        match self.acting_on.borrow().get(on_id).clone() {
            AnyExpression::Block(b) => {
                let block_ty = self.new_tid(b.info);

                let mut last_expr_tid = None;

                for &eid in b.expressions.iter() {
                    //let c_ty = self.new_tid();
                    tracing::info!("handling an expr in a block, has eid {eid:?}");
                    let e_ty = self.do_the_thing_rec(eid, false);

                    last_expr_tid = Some(e_ty);

                    //self.type_of[&eid] = c_ty;
                    //self.type_of.borrow_mut().insert(eid, e_ty);
                }

                match last_expr_tid {
                    None => {
                        todo!("block has type unit")
                    }
                    Some(tid) => {
                        self.add_unify(
                            //self.type_of.borrow().get(eid).copied().unwrap(),
                            tid,
                            block_ty,
                            "a block returns the same type as the last expression in it".intern(),
                        );
                    }
                }

                block_ty
            }
            AnyExpression::Assign(a) => {
                let lhs_tid = self.do_the_thing_rec(a.lhs, true); // assignment happening, so we
                                                                  // must be an lval
                let rhs_tid = self.do_the_thing_rec(a.rhs, false);

                self.add_unify(
                    rhs_tid,
                    lhs_tid,
                    "left and ride hand side of an assignment should be the same type".intern(),
                );

                rhs_tid // for now, we assign straight through, so don't re-take the value
            }
            AnyExpression::Convert(c) => todo!(),
            AnyExpression::While(_) => todo!(),
            AnyExpression::Branch(_) => todo!(),
            AnyExpression::Binding(_) => todo!(),
            AnyExpression::Invoke(i) => {
                //todo!("for an invocation, we need to figure out specifically what it's on");
                if is_lval {
                    tracing::error!("uhhhh, a func was an lval? need references?");
                }

                tracing::info!("it's an invocation: {i:#?}");

                let resulting_type_id = self.new_tid(i.info);
                tracing::info!(
                    "creates result type id for the invocation of {resulting_type_id:?}"
                );

                unsafe {
                    // the type of the thing we're calling (could be field
                    // or function ref, could even be a function/closure literal)

                    let fn_tid = self.do_the_thing_rec(i.target_fn, is_lval);

                    //let mut ptypes = vec![fn_tid]; // we're assuming things are methods for now
                    let mut ptypes = Vec::new();

                    for arg in i.args {
                        let atid = self.do_the_thing_rec(arg, false);
                        tracing::info!("looked at an arg {arg:?}, got tid: {atid:?}");
                        ptypes.push(atid);
                    }

                    tracing::info!("Got ptypes, they are: {ptypes:?}");

                    self.executor.install(
                        async move {
                            let fctx = self
                                .with_instance(fn_tid, |instance| instance.notify.clone().wait())
                                .await;

                            let (unifies, unify_error, arg_errors) =
                                self.with_instance(fn_tid, |instance| {
                                    let as_call_res =
                                        instance.as_call(ptypes, resulting_type_id, vec![], self);

                                    as_call_res
                                });

                            tracing::info!("starting thunks");
                            for thunk in unifies {
                                //panic!("thunking");
                                let Unify { from, into } = thunk.to_unify().await;
                                //panic!("thunkd");
                                self.add_unify(from, into, "instance of a call told us to");
                            }

                            for error in arg_errors {
                                self.type_errors
                                    .borrow_mut()
                                    .push(CompilationError::ArgConversionError(error))
                            }

                            tracing::debug!("function got unified? maybe?");
                        },
                        "verify function call is with the right params",
                    );
                }

                resulting_type_id
            }
            AnyExpression::StaticAccess(sa) => {
                let result_ty = self.new_tid(sa.info);
                tracing::info!("static accessing: {sa:?}");
                // the type of the base that we're accessing the field on
                let src_ty = self.do_the_thing_rec(sa.on, false);

                unsafe {
                    self.executor.install(async move {
                        let fut = self.with_instance(src_ty, |instance| { instance.notify.clone().wait() });

                        // wait for that instance to notify, since that means we've resolved the
                        // base

                        let base_ctx = fut.await;

                        //panic!("got base ctx for sa");

                        // the base *must* have resolved at this point, so we can just use it and
                        // ask for the field!

                        let (as_field, as_method) = self.with_instance(src_ty, |instance| {
                            tracing::info!("the iid of the one we got back is {:?}", instance.id);
                            match &instance.of {
                                InstanceOf::Type(t) => {
                                    let f = t.regular_fields.get(&sa.field).cloned();
                                    let m = t.methods.get(&sa.field).cloned();

                                    (f, m)
                                }
                                InstanceOf::Func(f) => {
                                    panic!("user tried to access a field on a function");
                                }
                                InstanceOf::Generic(g) => {
                                    panic!("generic? {g}");
                                },
                                InstanceOf::Unknown() => {
                                    panic!("stop");
                                }
                            }
                        });

                        let rtid = match (as_field, as_method) {
                            (None, None) => {
                                            tracing::error!("Field is: {}", sa.field);
                                            panic!("user tried to access a field that didn't exist on the type")
                            }
                            (None, Some(m)) => {
                                tracing::info!("it was a method");

                                let mtid = m.extract().await;

                                Some(mtid)
                            },
                            (Some(f), None) => {
                                let ftid = f.extract().await;

                                Some(ftid)
                            },
                            (Some(_), Some(_)) => todo!(),
                        };

                        match rtid {
                            Some(tid) => {
                                self.add_unify(tid, result_ty, "the field's type unifies with the result of its usage");
                            },
                            None => {
                                tracing::error!("no tid?");
                            }
                        }
                        /*{
                            Some(tid) => {
                                self.add_unify(tid, result_ty, "the field's type unifies with the result of its usage");
                            },
                            None => {
                            }
                        }*/

                        /*

                                    match f {
                                        None => {
                                            tracing::error!("Field is: {}", sa.field);
                                            tracing::error!("All fields are: {:?}", t.regular_fields);
                                            panic!("user tried to access a field that didn't exist on the type")
                                        },
                                        Some(tid) => {
                                            tracing::info!("user got a valid field!");
                                            Some(*tid)
                                        }
                                    }
                                    */

                        tracing::info!("we unified a field type!");

                        //todo!("now figure out field type since we have the base");
                    }, "wait for a resolution on the base of an access")
                }

                result_ty
            }
            AnyExpression::DynamicAccess(_) => {
                todo!("no syntactic meaning to dynamic access anymore")
            }
            AnyExpression::Variable(v, n) => {
                //self.type_of.borrow_mut().insert(k, v)
                // here we also care if we're an lval or an rval
                let ovt = self.type_of_var.borrow().get(&v).copied().unwrap();

                let vt = self.new_tid(n);

                self.add_unify(ovt, vt, "a variable has the type of its binding");

                tracing::info!("it's on a variable: {v:?}");

                vt
            }
            AnyExpression::Literal(l) => {
                // YOOOOOOOOOOOOOOOOOOOOOOo we can now add a direct,
                // woooooooooooooooooooooooooooooooo
                if is_lval {
                    panic!("user tried to assign into a literal");
                }
                tracing::info!("got a literal: {l:?}");

                let typeof_literal = self.new_tid(l.info);

                unsafe {
                    self.executor.install(
                        async move {
                            tracing::info!("resolving the type of a literal");
                            let Literal {
                                has_type,
                                value,
                                info,
                            } = l;
                            let hm = HashMap::new();

                            //panic!("going to resolve: {:?}", has_type.resolve().unwrap().value());

                            let ltid = self
                                .resolve_typeref(has_type, &hm, self.search_within())
                                .await;

                            //panic!("got literal type");

                            self.add_unify(
                                ltid,
                                typeof_literal,
                                "a literal should unify with the type it acts as".intern(),
                            );

                            tracing::error!("we now have the type of a literal, should resolve it");
                        },
                        "resolve the type of a literal once we have it",
                    );
                }

                tracing::info!("leaving behind a literal to go do other things");

                typeof_literal
            }
            AnyExpression::Composite(c) => {
                let Composite {
                    base_type,
                    generics,
                    fields,
                    info,
                } = c;

                let typeof_composite = self.new_tid(info);

                unsafe {
                    self.executor.install(
                        async move {
                            let nr = NameResolver {
                                name: base_type.clone(),
                                based_in: self.search_within(),
                                reply_to: self.node_id,
                                service: Service::Quark()
                            };

                            let ctx_for_base = nr
                                .using_context(&self.conversations)
                                .await;

                            tracing::info!("resolved a ctx for name {base_type:?} for a composite construction, it ended up being {ctx_for_base:?}");

                            let ctx_for_base = match ctx_for_base {
                                Ok(v) => v,
                                Err(ie) => todo!("handle import error"),
                            };

                            //.expect("couldn't resolve base tyep");

                            let mut inst_fields = HashMap::new();

                            for (fname, fexp) in fields {
                                //let fty = self.new_tid();
                                let fty = self.do_the_thing_rec(fexp, false); // not lval as we load from it

                                inst_fields.insert(fname, fty);
                            }

                            let mut inst_generics = Vec::new();

                            for gen in generics {
                                let ty = self
                                    .resolve_typeref(gen, self.generics.as_ref(), self.search_within())
                                    .await;

                                inst_generics.push(ty);
                            }

                            //let base_id = self.resolve_typeref(c.base_type, self.type_args, self.node_id).await;
                            let (inst, unifies) = Instance::construct_instance(
                                ctx_for_base,
                                inst_fields,
                                inst_generics,
                                &self,
                            ).await;

                            self.assign_instance(typeof_composite, inst);

                            for Unify { from, into } in unifies {
                                self.add_unify(from, into, "the instance said to!".intern());
                            }

                            //self.instances.borrow_mut().insert(e_ty_id, inst);
                        },
                        "composite resolution block",
                    );
                }

                typeof_composite
            }
        }
    }

    #[allow(unused_mut)]
    pub async fn lower_to_mir<'func>(
        &'static self,
        f: &'func ast::types::FunctionDefinition,
        imp: &'func mut ExpressionWrapper,
    ) -> ExpressionID {
        //let mut ec = ExpressionContext::new_empty();
        let mut binding_scope = Bindings::fresh();

        for (param_name, param_type) in f.parameters.clone() {
            let ptype = self
                .resolve_typeref(param_type, self.generics.as_ref(), self.search_within())
                .await;

            let vid = self.acting_on.borrow_mut().next_var();

            self.type_of_var.borrow_mut().insert(vid, ptype);

            binding_scope.add_binding(param_name, vid);
        }

        let ae = AnyExpression::from_ast(&mut self.acting_on.borrow_mut(), imp, &mut binding_scope);

        tracing::info!("expressions in quark:");

        //for e in self.acting_on.borrow().expressions.

        tracing::info!("done with from_ast for node {:?}", self.node_id);

        //todo!("descend completed?");
        ae

        //rec(&mut f.implementation).await;
    }

    pub async fn thread(self, mut ep: Earpiece) {
        let boxed = Box::pin(self); // put ourselves into the heap in a definitely known location
                                    // to avoid dumb shenaniganery
        let sptr: *const Quark = Pin::into_inner(boxed.as_ref()) as *const _;
        let sref: &'static Quark = unsafe { sptr.as_ref().unwrap() };

        info!("starts quark thread");

        match &sref.node_id.resolve().inner {
            ast::tree::NodeUnion::Function(f, imp) => {
                let f = f.clone();
                warn!(
                    "quark for a function starts up using ctx id {:?}",
                    sref.node_id
                );
                let mut imp = imp
                    .lock()
                    .unwrap()
                    .take()
                    .expect("someone else got to this impl first?");

                unsafe {
                    let cloned: FunctionDefinition = f.clone();
                    let eps = ep.cloned_sender();

                    sref.executor.install(
                        async move { sref.entry(cloned, &mut imp).await },
                        "quark worker thread".to_owned(),
                    )

                    //while let Some(v) =
                }

                while let Ok(v) = ep.wait().await {
                    tracing::info!("got a message for quark, forwarding to conversations...");
                    let remainder = sref.conversations.dispatch(v);
                    if let Some(v) = remainder {
                        //tracing::error!("unhandled message? it is: {:#?}", v);
                        match v.content {
                            Content::Quark(photon) => match photon {
                                crate::compile::per_module::Photon::CompilationStalled() => {
                                    sref.end_last_phase()
                                }
                            },
                            Content::Control(_) => todo!(),
                            Content::Announce(_) => todo!(),
                            Content::Transponster(_) => todo!(),
                            Content::NameResolution(_) => todo!(),
                            Content::Error(_) => todo!(),
                        }
                    }
                }

                unreachable!()

                //self.thread_stage_2(f).await;
            }
            _ => {
                // we don't do anything in the other cases, we only make sense in the case of being
                // a function
                warn!(
                    "quark for node {:?} is shutting down, as it is not a function",
                    sref.node_id
                );

                while let Ok(v) = ep.wait().await {
                    info!("Quark got a message");

                    ep.send(Message {
                        to: v.send_reply_to,
                        send_reply_to: Destination::nil(),
                        from: Destination {
                            node: sref.node_id,
                            service: Service::Quark(),
                        },
                        conversation: v.conversation,
                        content: Content::Control(ControlMessage::CanNotReply()),
                    });
                }
            }
        }
    }

    #[allow(unused_mut)]
    pub async fn entry(
        &'static self,
        f: crate::ast::types::FunctionDefinition,
        imp: &mut ExpressionWrapper, //executor: &'static Executor,
    ) {
        tracing::info!("getting parent for node: {:?}", self.node_id);
        let parent_id = self.node_id.resolve().parent.unwrap();

        tracing::error!("need to properly uh...handle generics for stuff");
        /*for (name, tr) in f.parameters.iter_mut() {
            SymbolResolver {
                node_id: self.node_id,
                earpiece: &mut self.earpiece,
                for_service: Service::Quark(),
            }
            .resolve(tr)
            .await;

            //println!("resolved a param type");
        }

        let tres = SymbolResolver {
            node_id: self.node_id,
            earpiece: &mut self.earpiece,
            for_service: Service::Quark(),
        }
        .resolve(&mut f.return_type)
        .await;*/
        //tres.resolve_typeref(&mut f.return_type).await;

        // this is all we need to do for now
        // to let the local transponster know how to answer call queries
        // and instantiations
        /*let _ = self.earpiece.cloned_sender().send(Message {
            to: Destination::transponster(self.node_id),
            from: self.as_dest(),
            send_reply_to: Destination::nil(),
            conversation: Uuid::new_v4(),
            content: Content::Transponster(Memo::NotifySelfCallable {
                generics: self
                    .node_id
                    .resolve()
                    .generics
                    .clone()
                    .into_iter()
                    .collect(),
                params: f.parameters.clone(),
                returns: f.return_type.clone(),
            }),
        });*/

        let aeid = self.lower_to_mir(&f, imp).await;

        let root_tid = self.do_the_thing(aeid);

        //let return_ty = f.return_type.resolve().unwrap().clone();

        let return_ty = self
            .resolve_typeref(f.return_type, &self.generics.as_ref(), self.search_within())
            .await;

        self.add_unify(
            root_tid,
            return_ty,
            "the function block expr must return the type requested".intern(),
        );

        //yiel
        // start walking the function tree now? or do later?

        //self.thread_stage_2(f).await;
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ResolvedType {
    node: CtxID,
    generics: Vec<ResolvedType>,
}
