use crate::avec::AtomicVecIndex;
use crate::compile::per_module::Photon;
use crate::cst::{FunctionBuiltin, SyntacticTypeReference};
use crate::errors::{FieldAccessError, UnrestrictedTypeError};
use crate::helper::CopyMethod;
use crate::mir::expressions::{Binding, For, If};
use crate::mir::scribe::ScribeOne;
use crate::mir::transponster::Memo;
use crate::{
    compile::per_module::StalledDog,
    errors::{CompilationError, TypeUnificationError},
    helper::SwapWith,
};
use std::collections::HashSet;
use std::fmt::Debug;
use std::{cell::RefCell, collections::HashMap, pin::Pin, rc::Rc};

use either::Either;
use once_cell::unsync::OnceCell;
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
        instance::InstanceOf,
    },
};

use super::instance::Generic;
use super::scribe::Monomorphization;
use super::{
    expressions::ExpressionID,
    instance::{Instance, Unify},
    sets::Unifier,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
/// (ID of the typevar, info for where it was parsed, whether it's a "root")
pub struct TypeID(uuid::Uuid, cst::NodeInfo, bool);

impl TypeID {
    pub fn span(&self) -> NodeInfo {
        self.1
    }

    pub fn is_root(&self) -> bool {
        self.2
    }
}

#[derive(Clone, Debug)]
pub struct TypeError {
    pub components: Vec<TypeID>,
    pub complaint: String,
    pub because_unify: Unify,
}

pub struct TypeMap {
    primary: RefCell<HashMap<ExpressionID, TypeID>>,
}

impl TypeMap {
    pub fn set(&self, eid: ExpressionID, to: TypeID) {
        self.primary.borrow_mut().insert(eid, to);
    }

    pub fn get(&self, eid: ExpressionID) -> TypeID {
        self.primary
            .borrow()
            .get(&eid)
            .copied()
            .expect("eid tid wasn't set")
    }

    pub fn new() -> Self {
        Self {
            primary: RefCell::new(HashMap::new()),
        }
    }
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
    pub acting_on: RefCell<ExpressionContext>,

    pub type_of_var: RefCell<HashMap<VarID, TypeID>>,

    /// If we resolve a TypeID from here, we should tell any usages/instances
    /// that there is a new direct
    //dynfield_notifies: RefCell<HashMap<TypeID, Vec<(InstanceID, UsageHandle)>>>,

    /// If we've resolved a type far enough to know that it is an Instance,
    /// it is recorded here so we can do field analysis on it
    pub instances: RefCell<Unifier<TypeID, Instance>>,

    pub typeofs: TypeMap,

    //once_know: RefCell<HashMap<TypeID, Vec<Action>>>,

    //once_know_ctx: RefCell<HashMap<TypeID, Rc<UnsafeAsyncCompletable<CtxID>>>>,
    sender: local_channel::mpsc::Sender<Message>,

    pub executor: &'static Executor,

    pub generics: Rc<HashMap<IStr, TypeID>>,

    pub node_id: CtxID,

    pub type_errors: RefCell<ErrorState>,

    conversations: ConversationContext,

    monomorphizations: RefCell<HashSet<Monomorphization>>,

    pub meta: Meta,
}

#[derive(Default)]
pub struct Meta {
    pub returns: OnceCell<TypeID>,
    pub params: OnceCell<Vec<(IStr, TypeID, VarID)>>,

    pub entry_id: OnceCell<ExpressionID>,

    pub is_builtin: OnceCell<Option<FunctionBuiltin>>,

    pub are_methods: RefCell<HashMap<ExpressionID, TypeID>>,

    pub generics_from_name: RefCell<HashMap<IStr, CtxID>>,
    pub generics_to_name: RefCell<HashMap<CtxID, IStr>>,
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
    /*
    /// Says that we should take the resolution
    /// we got for the given TypeID and give it to
    /// this Instance which may open up additional opportunities
    /// to resolve things
    ResolveUsage(InstanceID, UsageHandle),
    */
}

impl Quark {
    pub fn add_unify<R: Into<IStr>>(&'static self, from: TypeID, into: TypeID, reason: R) {
        // this could be batched later for perf improvement, but for now just
        // do an increment every time
        StalledDog::nudge_quark();

        let reason: IStr = reason.into();
        println!("unifies {from:?} into {into:?} because {reason}");
        tracing::warn!("borrows instances for add_unify");
        let mut refm = self.instances.borrow_mut();
        let mut resulting_unifies = Vec::new();

        let _useme = refm.unify(from, into, |from_inst, into_inst| -> Result<Instance, !> {
            tracing::info!("having to merge two instances");
            let original_a = from_inst.clone();
            let (v, u) = from_inst.unify_with(into_inst, Unify { from, into }, reason, self);
            resulting_unifies = u;

            Ok(v)
        });

        //.expect("user did a type error");

        // this add_unify could also call it
        std::mem::drop(refm);

        for Unify { from, into } in resulting_unifies {
            self.add_unify(from, into, "resulting unify from an instance add".intern());
        }

        tracing::debug!("New status for sets:");

        let b = self.instances.borrow();
        //tracing::debug!("{:#?}", b);
    }

    #[track_caller]
    pub async fn resolved_type_of(&'static self, t: TypeID) -> ResolvedType {
        self.with_instance(t, |inst| unsafe { inst.once_resolved.clone().wait() })
            .await
    }

    /// When each quark phase completes we get one message here to flush errors
    /// before switching over to Transponster
    fn end_one_phase(&self) {
        let mut errors = self.type_errors.borrow_mut();

        if errors.is_tainted {
            for error in errors.errors_to_be_flushed.swap_with(vec![]).into_iter() {
                let _ = self.sender.send(Message {
                    to: Destination::nil(),
                    from: Destination::nil(),
                    send_reply_to: Destination::nil(),
                    conversation: Uuid::new_v4(),
                    content: Content::Error(error),
                });
            }
        }
    }

    pub fn add_error(&self, e: CompilationError) {
        self.type_errors.borrow_mut().push(e);
    }

    pub fn add_type_error(&self, e: TypeError) {
        let refm = self.instances.borrow();

        let TypeError {
            because_unify,
            components,
            complaint,
        } = e;

        let Unify { from, into } = because_unify;

        let e = TypeUnificationError {
            from,
            from_peers: refm.peers_of(from),
            into,
            into_peers: refm.peers_of(into),
            for_expression: NodeInfo::Builtin,
            reason_for_unification: "todo".intern(),
            reason_for_failure: complaint.intern(),
            context: Vec::new(),
        };

        self.add_error(CompilationError::TypeError(e));
    }

    /// If compilation fully stalls then this is called,
    /// to let us emit final errors or prepare Scribe to output our
    /// monomorphizations
    fn end_last_phase(&'static self) {
        let mut errors = self.type_errors.borrow_mut();

        tracing::debug!("Ending last phase, current unification state:");
        //tracing::debug!("{:#?}", self.instances.borrow());

        if errors.is_tainted {
            tracing::warn!("not emitting errors since already tainted");
            // we got a type error, so we shouldn't
            // emit any complaints about un-restrained variables
        } else {
            tracing::warn!("going to emit any errors...");
            let free_vars = self.instances.borrow().gather_free();

            self.instances.borrow().map_roots(|tid, inst| {
                if unsafe { inst.once_resolved.is_complete() } {
                    inst.notify_monomorphizations();
                }
            });

            let unresolved_insts = self.instances.borrow().reduce_roots(|tid, inst| {
                if unsafe { !inst.once_resolved.is_complete() } {
                    Some(*tid)
                } else {
                    None
                }
            });

            if !unresolved_insts.is_empty() {}

            tracing::error!("Unresolved insts: {}", unresolved_insts.len());

            for tid in unresolved_insts {
                let note = if let Some(v) = self.with_instance(tid, |inst| unsafe { inst.once_base.try_get().copied() }) {
                    let cr = v.resolve().canonical_typeref().resolve().unwrap();

                    format!("the type base was found to be a {cr:?}").intern()
                } else {
                    "nothing about the type base could be determined".intern()
                };

                let b = self.instances.borrow();
                println!("Instances: {b:#?}");

                errors.push(CompilationError::UnrestrictedTypeError(
                    UnrestrictedTypeError {
                        note,
                        tid,
                        peers: self.instances.borrow().peers_of(tid),
                    },
                ))
            }

            if free_vars.len() > 0 {

            }


            tracing::error!("Unrestricted vars: {}", free_vars.len());
        }

        std::mem::drop(errors);

        self.end_one_phase();
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
    #[track_caller]
    pub async fn resolve_typeref(
        &'static self,
        tr: SyntacticTypeReferenceRef,
        with_generics: &HashMap<IStr, TypeID>,
        from_base: CtxID,
        is_root: bool,
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

                        //panic!("got an instance");

                        tracing::debug!("tr base is: {:?}", instance.of);

                        // don't set an accessed_from for this, since it's a plain function
                        // (shouldn't have a propagated `self`)

                        //let tid = self.new_tid();

                        let tid = self.introduce_instance(instance, trv.info, true);

                        tracing::debug!("introduced {tid:?}");

                        tid
                    }
                    Err(_) => {
                        todo!("need to handle import errors with Instances");

                        // once we've propagated the error, we just treat this as unconstrained
                        // to let us find as many errors as possible
                        let tid = self.new_tid(trv.info, true);

                        tid
                    }
                }
            }
            SyntacticTypeReferenceInner::Unconstrained() => {
                tracing::warn!("constructing completely unconstrained typeref, need to validate that or something");
                let tid = self.new_tid(trv.info, is_root);

                tid
            }
            SyntacticTypeReferenceInner::Generic { label } => {
                //async_backtrace::
                println!("Generics: {:?}", with_generics);
                let existing_tid = with_generics.get(&label);

                let existing_tid = match existing_tid {
                    Some(s) => {
                        println!("Gives the generic the known tid {s:?}");
                        *s
                    }
                    None => {
                        println!("The generic was {label}");
                        panic!("got a generic that didn't mean anything in the context, we are {:?} and within {:?}", self.node_id, from_base);
                    }
                };
                existing_tid
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
                        return self.new_tid(NodeInfo::Builtin, is_root);
                    }
                    Ok(v) => v,
                };

                let mut resolved_generics = Vec::new();

                for generic in generics {
                    let resolved = self
                        .resolve_typeref(generic.intern(), with_generics, from_base, is_root)
                        .await;

                    resolved_generics.push(resolved);
                }

                println!(
                    "Makes an instance of {:?} with generics {resolved_generics:?}",
                    base.resolve().canonical_typeref().resolve().unwrap()
                );
                let (inst, unify) = Instance::with_generics(base, self, resolved_generics).await;

                for Unify { from, into } in unify {
                    self.add_unify(
                        from,
                        into,
                        "constructing instance with generics made these ties",
                    );
                }

                self.introduce_instance(inst, trv.info, is_root)
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

    #[track_caller]
    pub fn with_instance<F, R>(&'static self, tid: TypeID, f: F) -> R
    where
        F: FnOnce(&Instance) -> R,
    {
        tracing::warn!("borrows instances for with_instance");
        let mut refm = self.instances.borrow_mut();
        if let Some(v) = refm.v_for(tid) {
            f(v)
        } else {
            let new_tid = TypeID(Uuid::new_v4(), tid.span(), false);
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
    pub fn assign_instance(
        &'static self,
        tid: TypeID,
        instance: Instance,
        is_root: bool,
    ) -> TypeID {
        // make a new tid inst here since the regular one automatically adds it to unifier
        let instance_tid = TypeID(uuid::Uuid::new_v4(), tid.span(), is_root);

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
    pub fn introduce_instance(
        &'static self,
        instance: Instance,
        span: NodeInfo,
        is_root: bool,
    ) -> TypeID {
        let instance_tid = TypeID(Uuid::new_v4(), span, is_root);

        tracing::warn!("borrows instances for introduce_instance");
        let mut refm = self.instances.borrow_mut();

        refm.add_kv(instance_tid, instance);

        instance_tid
    }

    pub fn new_tid(&self, for_span: NodeInfo, is_root: bool) -> TypeID {
        let tid = TypeID(uuid::Uuid::new_v4(), for_span, is_root);

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

        println!("Generics are {:?}", node_id.resolve().generics);

        for (gname, gtype) in node_id.resolve().generics.iter() {
            println!(
                "a - added a generic {gname} within {node_id:?} who is {:?}",
                node_id.resolve().canonical_typeref().resolve().unwrap()
            );
            let g_tid = TypeID(uuid::Uuid::new_v4(), NodeInfo::Builtin, true);

            generics.insert(*gname, g_tid);
        }

        Self {
            type_errors: RefCell::new(ErrorState::new()),
            sender,
            node_id,
            acting_on: RefCell::new(ExpressionContext::new_empty()),
            type_of_var: Default::default(),
            executor,
            //dynfield_notifies: RefCell::new(HashMap::new()),
            instances: RefCell::new(Unifier::new()),
            //once_know: RefCell::new(HashMap::new()),
            conversations: unsafe { ConversationContext::new(cs) },
            generics: Rc::new(generics),
            typeofs: TypeMap::new(),
            monomorphizations: Default::default(),
            meta: Default::default(),
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

        let res_tid = match self.acting_on.borrow().get(on_id).clone() {
            AnyExpression::Block(b) => {
                let block_ty = self.new_tid(b.info, false);

                for &eid in b.statements.iter() {
                    //let c_ty = self.new_tid();
                    tracing::info!("handling an expr in a block, has eid {eid:?}");
                    let e_ty = self.do_the_thing_rec(eid, false);

                    //self.type_of[&eid] = c_ty;
                    //self.type_of.borrow_mut().insert(eid, e_ty);
                }

                let final_expr = match b.final_expr {
                    Some(e) => Some(self.do_the_thing_rec(e, false)),
                    None => None,
                };

                match final_expr {
                    None => {
                        unsafe {
                            self.executor.install(
                                async move {
                                    let unit_ty = self
                                        .resolve_typeref(
                                            SyntacticTypeReferenceRef::from_std("std::Unit"),
                                            &self.generics,
                                            self.node_id,
                                            false,
                                        )
                                        .await;

                                    self.add_unify(
                                        unit_ty,
                                        block_ty,
                                        "an empty block returns unit",
                                    );
                                },
                                "resolve unit ref for empty block",
                            )
                        };
                    }
                    Some(tid) => {
                        self.add_unify(
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
            AnyExpression::If(i) => {
                let If {
                    condition,
                    then_do,
                    else_do,
                } = i;

                let cond_ty = self.do_the_thing_rec(condition, false);
                let then_ty = self.do_the_thing_rec(then_do, false);
                let else_ty = else_do.map(|ed| self.do_the_thing_rec(ed, false));

                if let Some(else_ty) = else_ty {
                    self.add_unify(then_ty, else_ty, "then and else must have same type");
                }

                unsafe {
                    self.executor.install(
                        async move {
                            let btid = self
                                .resolve_typeref(
                                    SyntacticTypeReferenceRef::from_std("std::bool"),
                                    &HashMap::new(),
                                    self.node_id,
                                    false,
                                )
                                .await;

                            self.add_unify(btid, cond_ty, "the condition of an if must be a bool");
                        },
                        "force condition to be a bool",
                    )
                }

                then_ty
            }
            AnyExpression::For(f) => {
                let For {
                    body,
                    pre,
                    post,
                    condition,
                } = f;

                self.do_the_thing_rec(pre, false);

                let cond_ty = self.do_the_thing_rec(condition, false);

                self.do_the_thing_rec(post, false);

                let body_ty = self.do_the_thing_rec(body, false);

                unsafe {
                    self.executor.install(
                        async move {
                            let btid = self
                                .resolve_typeref(
                                    SyntacticTypeReferenceRef::from_std("std::bool"),
                                    &HashMap::new(),
                                    self.node_id,
                                    false,
                                )
                                .await;

                            self.add_unify(btid, cond_ty, "the condition of a for must be a bool");
                        },
                        "force condition to be a bool",
                    )
                }

                /*unsafe {
                    self.executor.install(
                        async move {
                            let utid = self
                                .resolve_typeref(
                                    SyntacticTypeReferenceRef::from_std("std::Unit"),
                                    &HashMap::new(),
                                    self.node_id,
                                    false,
                                )
                                .await;

                            self.add_unify(utid, cond_ty, "the body of a for loop should be of type Unit");
                        },
                        "force body to be a Unit",
                    )
                }*/

                body_ty
            }
            AnyExpression::Convert(c) => todo!(),
            AnyExpression::While(_) => todo!(),
            AnyExpression::Branch(_) => todo!(),
            AnyExpression::Binding(b) => {
                let Binding {
                    info,
                    name,
                    introduced_as,
                    has_type,
                    from_source,
                } = b;

                let src_tid = self.do_the_thing_rec(from_source, false);

                let var_tid = *self
                    .type_of_var
                    .borrow_mut()
                    .entry(introduced_as)
                    .or_insert(self.new_tid(info, true));

                self.add_unify(
                    src_tid,
                    var_tid,
                    "assigning into a variable means the variable has the type of the source"
                        .intern(),
                );

                src_tid
            }
            AnyExpression::Invoke(i) => {
                //todo!("for an invocation, we need to figure out specifically what it's on");
                if is_lval {
                    tracing::error!("uhhhh, a func was an lval? need references?");
                }

                tracing::info!("it's an invocation: {i:#?}");

                let resulting_type_id = self.new_tid(i.info, false);
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
                                .with_instance(fn_tid, |instance| instance.once_base.clone().wait())
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
                let result_ty = self.new_tid(sa.info, false);
                tracing::info!("static accessing: {sa:?}");
                // the type of the base that we're accessing the field on
                let src_ty = self.do_the_thing_rec(sa.on, false);
                //let src_span = self.acting_on.borrow().get(sa.on).

                unsafe {
                    self.executor.install(async move {
                        let fut = self.with_instance(src_ty, |instance| { instance.once_base.clone().wait() });

                        // wait for that instance to notify, since that means we've resolved the
                        // base

                        let base_ctx = fut.await;

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
                                // it's a dynamic field

                                let fname = sa.field;

                                self.executor.install(async move {
                                    let direct = self.with_instance(result_ty, |instance| { instance.once_resolved.clone().wait() }).await;

                                    //panic!("we got the type of the ret!");

                                    //let inst: Instance = Instance::from_resolved(direct_resolved);

                                    tracing::warn!("sends a wait for on a NotifyDirectUsage");

                                    // tell the related type about the direct
                                    let resp = self.conversations.wait_for(
                                        Message {
                                            to: Destination { node: base_ctx, service: Service::Transponster() },
                                            from: self.as_dest(),
                                            send_reply_to: self.as_dest(),
                                            conversation: Uuid::new_v4(),
                                            content: Content::Transponster( crate::mir::transponster::Memo::NotifyDirectUsage { of: fname, as_ty: direct } )
                                        }
                                    ).await;

                                }, "once we know the type of a dynfield from usages, we should add a direct");

                                self.executor.install(async move {
                                    tracing::warn!("sends a wait for on a NotifyIndirectUsage");
                                    // tell the dest type that we have an indirect usage
                                    let resp = self.conversations.wait_for(
                                        Message {
                                            to: Destination { node: base_ctx, service: Service::Transponster() },
                                            from: self.as_dest(),
                                            send_reply_to: self.as_dest(),
                                            conversation: Uuid::new_v4(),
                                            content: Content::Transponster( crate::mir::transponster::Memo::NotifyIndirectUsage { of: fname } )
                                        }
                                    ).await;

                                    tracing::error!("we heard back about an indirect?");

                                    let resolve_ty = match resp.content {
                                        Content::Transponster(Memo::ResolveIndirectUsage { field, commits_to }) => {
                                            commits_to
                                        },
                                        _ => unreachable!(),
                                    };

                                    tracing::error!("got ty: {resolve_ty:?}");

                                    if let Some(ty)= resolve_ty {

                                        let (inst, unify) = Instance::from_resolved(ty, self).await;

                                        for Unify { from, into } in unify {
                                            self.add_unify(from, into, "creating an instance from a resolved type caused this");
                                        }

                                        let tid = self.introduce_instance(inst, sa.info, true);

                                        self.add_unify(result_ty, tid,
                                                       "a dynamic field should be compatible with its usages after resolution");
                                    } else {
                                        self.add_error(CompilationError::FieldAccessError(
                                                FieldAccessError {
                                                    base_expr_span: sa.info,
                                                    field_span: sa.info,
                                                    error_info: format!("this field did not exist, and its dynamic property type could not be successfully inferred"),
                                                }))
                                    }
                                }, "once we get a resolved notification back from the dynfield, we should unify that value");

                                None
                            }
                            (None, Some(m)) => {
                                tracing::info!("it was a method");

                                let mtid = m.extract().await;

                                self.meta.are_methods.borrow_mut().insert(on_id, mtid);

                                Some(mtid)
                            },
                            (Some(f), None) => {
                                let ftid = f.extract().await;

                                Some(ftid)
                            },
                            (Some(_), Some(_)) => todo!("what? they were both a field and a method?"),
                        };

                        match rtid {
                            Some(tid) => {
                                self.add_unify(tid, result_ty, "the field's type unifies with the result of its usage");
                            },
                            None => {
                                tracing::error!("no tid?");
                            }
                        }

                        tracing::info!("we unified a field type!");
                    }, "wait for a resolution on the base of an access")
                }

                result_ty
            }
            AnyExpression::Variable(v, n) => {
                //self.type_of.borrow_mut().insert(k, v)
                // here we also care if we're an lval or an rval
                let ovt = self.type_of_var.borrow().get(&v).copied().unwrap();

                let vt = self.new_tid(n, false);

                self.add_unify(ovt, vt, "a variable has the type of its binding");

                tracing::info!("it's on a variable: {v:?}");

                vt
            }
            AnyExpression::OuterReference(s, ni) => {
                let et = self.new_tid(ni, true);

                unsafe {
                    self.executor.install(
                        async move {
                            let nr = NameResolver {
                                name: s.clone(),
                                based_in: self.search_within(),
                                reply_to: self.node_id,
                                service: Service::Quark(),
                            };

                            let ctx_for_base = nr.using_context(&self.conversations).await;

                            let inst = Instance::infer_instance(ctx_for_base.ok(), self).await;

                            self.assign_instance(et, inst, false);
                        },
                        "resolve the type of a static outer reference (like a function call)",
                    );
                }

                //self.instances.borrow_mut().add_k(et);

                et
            }
            AnyExpression::Literal(l) => {
                // YOOOOOOOOOOOOOOOOOOOOOOo we can now add a direct,
                // woooooooooooooooooooooooooooooooo
                if is_lval {
                    panic!("user tried to assign into a literal");
                }
                tracing::info!("got a literal: {l:?}");

                let typeof_literal = self.new_tid(l.info, true);

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
                                .resolve_typeref(has_type, &hm, self.search_within(), false)
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

                let typeof_composite = self.new_tid(info, true);

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

                            let mut inst_fields = HashMap::new();

                            for (fname, fexp) in fields {
                                let fty = self.do_the_thing_rec(fexp, false); // not lval as we load from it

                                inst_fields.insert(fname, fty);
                            }

                            let mut inst_generics = Vec::new();

                            for gen in generics {
                                let ty = self
                                    .resolve_typeref(gen, self.generics.as_ref(), self.search_within(), false)
                                    .await;

                                inst_generics.push(ty);
                            }

                            assert!(inst_generics.is_empty(), "just for now, since don't have code to use it");

                            let (inst, unifies) = Instance::construct_instance(
                                ctx_for_base,
                                inst_fields,
                                inst_generics,
                                &self,
                            ).await;

                            self.assign_instance(typeof_composite, inst, true);

                            for Unify { from, into } in unifies {
                                self.add_unify(from, into, "the instance said to!".intern());
                            }
                        },
                        "composite resolution block",
                    );
                }

                typeof_composite
            }
            _ => {
                panic!("any other lowered form here should should not exist yet, and it is a compiler bug if it does")
            }
        };

        self.typeofs.set(on_id, res_tid);

        res_tid
    }

    #[allow(unused_mut)]
    pub async fn lower_to_mir<'func>(
        &'static self,
        f: &'func ast::types::FunctionDefinition,
        imp: &'func mut ExpressionWrapper,
        bindings: &'func mut Bindings<'func>,
    ) -> ExpressionID {
        let ae = AnyExpression::from_ast(&mut self.acting_on.borrow_mut(), imp, bindings);

        tracing::info!("expressions in quark:");

        tracing::info!("done with from_ast for node {:?}", self.node_id);

        ae
    }

    pub async fn thread(&'static self, mut ep: Earpiece) {
        /*let boxed = Box::pin(self); // put ourselves into the heap in a definitely known location
                                    // to avoid dumb shenaniganery
        let sptr: *const Quark = Pin::into_inner(boxed.as_ref()) as *const _;
        let sref: &'static Quark = unsafe { sptr.as_ref().unwrap() };*/

        info!("starts quark thread");

        match &self.node_id.resolve().inner {
            ast::tree::NodeUnion::Function(f, imp) => {
                let f = f.clone();
                warn!(
                    "quark for a function starts up using ctx id {:?}, which is {:?}",
                    self.node_id, self.node_id.resolve().canonical_typeref().resolve().unwrap(),
                );
                let imp = imp
                    .lock()
                    .unwrap()
                    .take()
                    .expect("someone else got to this impl first?");

                unsafe {
                    let cloned: FunctionDefinition = f.clone();
                    let eps = ep.cloned_sender();

                    self.executor.install(
                        async move { self.entry(cloned, imp).await },
                        "quark worker thread".to_owned(),
                    )
                }

                while let Ok(v) = ep.wait().await {
                    tracing::info!("got a message for quark, forwarding to conversations...");
                    let remainder = self.conversations.dispatch(v);
                    if let Some(v) = remainder {
                        //tracing::error!("unhandled message? it is: {:#?}", v);
                        match v.content {
                            Content::Monomorphization(m) => {
                                self.monomorphizations.borrow_mut().insert(m);
                            }
                            Content::Quark(photon) => match photon {
                                Photon::CompilationStalled() => self.end_last_phase(),
                                Photon::EndPhase() => self.end_one_phase(),
                                Photon::BeginPhase() => {
                                    tracing::warn!("beginphase not handled yet");
                                }
                                Photon::StartCodeGen() => {}
                            },
                            Content::StartCodeGen() => {
                                tracing::error!("Codegen not implemented yet");

                                if self.node_id.resolve().generics.is_empty() {
                                    self.monomorphizations
                                        .borrow_mut()
                                        .insert(Monomorphization {
                                            of: self.node_id,
                                            with: vec![],
                                        });
                                }

                                for mono in self.monomorphizations.borrow().iter() {
                                    let m = Message {
                                        to: Destination::scribe(self.node_id),
                                        from: self.as_dest(),
                                        send_reply_to: Destination::nil(),
                                        conversation: Uuid::new_v4(),
                                        content: Content::Scribe(
                                            crate::mir::scribe::Note::MonoFunc {
                                                func: mono.clone(),
                                            },
                                        ),
                                    };
                                    self.sender.send(m).unwrap();
                                    /*let mut s =
                                        ScribeOne::new(either::Either::Left(self), mono.clone());
                                    unsafe {
                                        self.executor.install(
                                            async move {
                                                s.codegen().await;
                                            },
                                            "do codegen for function",
                                        )
                                    }*/
                                }
                            }
                            Content::Control(_) => todo!(),
                            Content::Announce(_) => todo!(),
                            Content::Transponster(_) => todo!(),
                            Content::NameResolution(_) => todo!(),
                            Content::Error(_) => todo!(),
                            Content::Scribe(_) => unreachable!("bad message type"),
                        }
                    }
                }

                unreachable!()
            }
            _ => {
                // we don't do anything in the other cases, we only make sense in the case of being
                // a function
                warn!(
                    "quark for node {:?} is shutting down, as it is not a function",
                    self.node_id
                );

                while let Ok(v) = ep.wait().await {
                    //info!("Quark got a message");

                    ep.send(Message {
                        to: v.send_reply_to,
                        send_reply_to: Destination::nil(),
                        from: Destination {
                            node: self.node_id,
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
        imp: Either<ExpressionWrapper, FunctionBuiltin>, //executor: &'static Executor,
    ) {
        tracing::info!("getting parent for node: {:?}", self.node_id);
        let parent_id = self.node_id.resolve().parent.unwrap();

        tracing::error!("need to properly uh...handle generics for stuff");

        let mut binding_scope = Bindings::fresh();

        let mut params = Vec::new();

        for (&gst, &gty) in self.generics.iter() {
            let gid = CtxID::new_generic(gst);
            println!("Made a new generic node");

            self.meta.generics_from_name.borrow_mut().insert(gst, gid);
            self.meta.generics_to_name.borrow_mut().insert(gid, gst);

            println!("Makes one from generic resolved");
            let (inst, unify) = Instance::from_resolved(
                ResolvedType {
                    node: gid,
                    generics: vec![],
                },
                self,
            )
            .await;

            for Unify { from, into } in unify {
                unreachable!("maybe?");
                self.add_unify(from, into, "idk, why did we get this?");
            }

            println!(
                "takes generic by name {gst} and gives it a specific cid: {gid:?} for gty {gty:?}"
            );

            let new_tid = self.introduce_instance(inst, NodeInfo::Builtin, false);

            self.add_unify(new_tid, gty, "generics serve as roots for other variables");

            //self.introduce_instance(gty, inst, true);
        }

        for (param_name, param_type) in f.parameters.clone() {
            let ptype = self
                .resolve_typeref(
                    param_type,
                    self.generics.as_ref(),
                    self.search_within(),
                    true,
                )
                .await;

            let pty_s = param_type.resolve().unwrap();

            let vid = self.acting_on.borrow_mut().next_var();

            println!("resolved the type of {param_name} with given type {pty_s:?} to {ptype:?}");

            self.type_of_var.borrow_mut().insert(vid, ptype);

            binding_scope.add_binding(param_name, vid);

            params.push((param_name, ptype, vid));
        }

        self.meta.params.set(params).unwrap();

        let return_ty = self
            .resolve_typeref(
                f.return_type,
                &self.generics.as_ref(),
                self.search_within(),
                true,
            )
            .await;
        self.meta.returns.set(return_ty).unwrap();

        match imp {
            Either::Left(mut imp) => {
                self.meta.is_builtin.set(None).expect("set prior?");

                let aeid = self.lower_to_mir(&f, &mut imp, &mut binding_scope).await;

                self.meta.entry_id.set(aeid).unwrap();

                let root_tid = self.do_the_thing(aeid);

                //let return_ty = f.return_type.resolve().unwrap().clone();

                self.add_unify(
                    root_tid,
                    return_ty,
                    "the function block expr must return the type requested".intern(),
                );
            }
            Either::Right(fr) => {
                self.meta
                    .is_builtin
                    .set(Some(fr.clone()))
                    .expect("set prior?");
                // we don't typecheck this, it also can't be a generic so
                // it only requires one monomorphization
                println!("Got a dec that we need a builtin, builtin is {:?}", fr);
                //panic!("got something neat")
            }
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct ResolvedType {
    pub node: CtxID,

    // either it's a proper given type or, potentially,
    // it's a generic passing through
    pub generics: Vec<ResolvedType>,
    // TODO: finish this and do proper monomorphization once I have time (not right now :) )
    //pub generics: Vec<ResolvedType>,
}

impl Debug for ResolvedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stref = self.node.resolve().canonical_typeref().resolve().unwrap();
        match self.generics.as_slice() {
            [] => write!(f, "{:?}", stref),
            other => write!(f, "{:?}<{:?}>", stref, other),
        }
    }
}
