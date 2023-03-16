use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
    sync::Arc, cell::RefCell,
};

//use itertools::Itertools;

use tracing::{info, warn};
use uuid::Uuid;

use crate::{
    ast::{
        self,
        executor::{Executor, UnsafeAsyncCompletable},
        resolver2::NameResolver,
        tree::CtxID,
        types::AbstractTypeReference,
    },
    avec::{AtomicVec, AtomicVecIndex},
    compile::per_module::{
        Content, ControlMessage, ConversationContext, Destination, Earpiece, Message, Service,
    },
    cst::{
        ExpressionWrapper, GenericHandle, SyntacticTypeReferenceInner, SyntacticTypeReferenceRef,
    },
    helper::{
        interner::{IStr, Internable},
        CompilationError,
    },
    mir::{
        expressions::{AnyExpression, Bindings, Composite, ExpressionContext},
        transponster::Memo,
    },
};

use super::{
    expressions::ExpressionID,
    transponster::{FieldID, Instance, InstanceID, UsageHandle},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TypeID(uuid::Uuid);

#[derive(Clone)]
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
    //type_args: Vec<SymbolicType>,
    //typer: TypeContext,

    //value: Linear,

    //allocations: HashMap<usize, Allocation>,
    //allocations: Vec<Allocation>,

    //allocation_references: HashMap<usize, OwnedAllocationReference>,

    //variables: Vec<(IStr, AllocationReference)>,
    //frames: Vec<usize>,
    acting_on: ExpressionContext,

    type_of: RefCell<HashMap<ExpressionID, TypeID>>,

    //specializations: Vec<HashMap<IStr, ResolvedType>>,

    //wait_resolve: HashMap<TypeID, Vec<executor::UnsafeAsyncCompletable<Result<(), ()>>>>,
    /// If we resolve a TypeID from here, we should tell any usages/instances
    /// that there is a new direct
    dynfield_notifies: RefCell<HashMap<TypeID, Vec<(InstanceID, UsageHandle)>>>,

    /// If we've resolved a type far enough to know that it is an Instance,
    /// it is recorded here so we can do field analysis on it
    instances: RefCell<HashMap<TypeID, Instance>>,

    once_know: RefCell<HashMap<TypeID, Vec<Action>>>,

    earpiece: Earpiece,

    executor: &'static Executor,

    generics: Rc<HashMap<IStr, TypeID>>,

    pub node_id: CtxID,

    conversations: ConversationContext,
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
    pub fn add_unify(&self, a: TypeID, b: TypeID, reason: IStr) {
        todo!()
    }

    pub fn action(&self, once: ExpressionID, apply: Action) {
        todo!("apply actions")
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
    pub async fn resolve_typeref(
        &self,
        tr: SyntacticTypeReferenceRef,
        with_generics: Rc<HashMap<IStr, TypeID>>,
        from_base: CtxID,
    ) -> TypeID {
        match &tr.resolve().unwrap().inner {
            SyntacticTypeReferenceInner::Single { name } => {
                if let [one] = name.scope.as_slice() && let Some(v) = with_generics.get(one) {
                    // we are ourselves just a generic, so pass through the type ID ffrom the
                    // generic
                    *v
                } else {
                    // we are maybe an instance of a type? or some node? so figure out which one it
                    // is
                    let r = NameResolver::new(name.clone(), from_base).using_context(&self.conversations).await;
                    match r {
                        Ok(cid) => {
                            let instance = Instance::infer_instance(Some(cid), self);

                            // don't set an accessed_from for this, since it's a plain function
                            // (shouldn't have a propagated `self`)

                            let tid = self.new_tid();

                            self.instance_for(tid, instance);

                            tid
                        },
                        Err(_) => {
                            todo!("need to handle import errors with Instances");

                            // once we've propagated the error, we just treat this as unconstrained
                            // to let us find as many errors as possible
                            let tid = self.new_tid();

                            tid
                        },
                    }
                }
            }
            SyntacticTypeReferenceInner::Unconstrained() => {
                tracing::warn!("constructing completely unconstrained typeref, need to validate that or something");
                let tid = self.new_tid();

                tid
            }
            SyntacticTypeReferenceInner::Tuple(t) => {
                todo!("need to actually make tuple types")
            }
            SyntacticTypeReferenceInner::Parameterized { name, generics } => {
                todo!("handle generics for tr")
            }
            SyntacticTypeReferenceInner::Reference { to, mutable } => {
                todo!("handle references in quark")
            }
            SyntacticTypeReferenceInner::Pointer { to, mutable } => {
                todo!("handle pointers in quark")
            }
        }
    }

    pub fn instance_for(&self, tid: TypeID, instance: Instance) {
        let was_there = self.instances.borrow_mut().insert(tid, instance);

        assert!(was_there.is_none(), "instance already assigned for tid");
    }

    pub fn new_tid(&self) -> TypeID {
        TypeID(uuid::Uuid::new_v4())
    }

    pub fn for_node(node_id: CtxID, earpiece: Earpiece, executor: &'static Executor) -> Self {
        warn!("quark is being improperly initialized to make things happy");
        let cs = earpiece.cloned_sender();

        let mut generics = HashMap::new();
        for (gname, gtype) in node_id.resolve().generics.iter() {
            tracing::error!("ignoring constraints on generics for now");
            let g_tid = TypeID(uuid::Uuid::new_v4());

            generics.insert(*gname, g_tid);
        }

        Self {
            //allocations: Vec::new(),
            //allocation_references: HashMap::new(),
            //variables: Vec::new(),
            //frames: Vec::new(),
            earpiece,
            node_id,
            acting_on: ExpressionContext::new_empty(),
            type_of: RefCell::new(HashMap::new()),
            executor,
            //typer: TypeContext::fresh(),
            dynfield_notifies: RefCell::new(HashMap::new()),
            instances: RefCell::new(HashMap::new()),
            once_know: RefCell::new(HashMap::new()),
            conversations: unsafe { ConversationContext::new(cs) },
            generics: Rc::new(generics),
            //specializations: todo!(),
        }
    }

    pub async unsafe fn do_the_thing(&mut self, on_id: ExpressionID) {
        let root_type_id = self.new_tid();
        let sptr = self as *mut Self;
        let sref = sptr.as_ref().unwrap();

        sref.do_the_thing_rec(on_id, root_type_id, false);
    }

    /// lval is true if we are being assigned into, otherwise we can be treated as an rval
    pub fn do_the_thing_rec(&'static self, on_id: ExpressionID, use_tid: TypeID, is_lval: bool) {
        tracing::info!("do_the_thing_rec on id: {on_id}");

        /*let e_ty_id: TypeID = self.typer.register_type(TypeVar {
            within: self.node_id,
            referees: Vec::new(),
            current: TypeType::Unknown(),
        });*/

        let e_ty_id = use_tid;

        match self.acting_on.get(on_id).clone() {
            AnyExpression::Block(b) => {
                for &eid in b.expressions.iter() {
                    let c_ty = self.new_tid();
                    let fut = self.do_the_thing_rec(eid, c_ty, false);

                    //self.type_of[&eid] = c_ty;
                    self.type_of.borrow_mut().insert(eid, c_ty);
                }

                match b.expressions.last() {
                    None => {
                        todo!("block has type unit")
                    }
                    Some(eid) => {
                        self.add_unify(
                            e_ty_id,
                            self.type_of.borrow().get(eid).copied().unwrap(),
                            "a block returns the same type as the last expression in it".intern(),
                        );
                    }
                }
            }
            AnyExpression::Assign(a) => {
                let lhs_tid = self.new_tid();
                let rhs_tid = self.new_tid();

                self.do_the_thing_rec(a.lhs, lhs_tid, true); // assignment happening, so we
                                                             // must be an lval
                self.do_the_thing_rec(a.rhs, rhs_tid, false);

                self.add_unify(
                    rhs_tid,
                    lhs_tid,
                    "left and ride hand side of an assignment should be the same type".intern(),
                );

                self.add_unify(
                    e_ty_id,
                    lhs_tid,
                    "an assignment returns the type of the LHS".intern(),
                );
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

                unsafe {
                    self.executor.install(
                        async move {
                            let resulting_type_id = e_ty_id;

                            let func_ret_tid = self.new_tid();

                            self.do_the_thing_rec(i.target_fn, func_ret_tid, is_lval);

                            let fid: FieldID = todo!();
                        },
                        "find type for an invocation, waiting on resolving the type of the base",
                    )
                }
            }
            AnyExpression::StaticAccess(sa) => {
                let field_src_tid = e_ty_id;
                match is_lval {
                    true => {
                        self.action(sa.on, Action::StoreFieldFrom(sa.field, field_src_tid));
                    }
                    false => {
                        self.action(sa.on, Action::LoadFieldInto(sa.field, field_src_tid));
                    }
                }

                //self.action(sa.on, Action::
            }
            AnyExpression::DynamicAccess(_) => {
                todo!("no syntactic meaning to dynamic access anymore")
            }
            AnyExpression::Variable(v) => {
                // here we also care if we're an lval or an rval
            }
            AnyExpression::Literal(l) => {
                // YOOOOOOOOOOOOOOOOOOOOOOo we can now add a direct,
                // woooooooooooooooooooooooooooooooo
                if is_lval {
                    panic!("user tried to assign into a literal");
                }
                //
            }
            AnyExpression::Composite(c) => {
                let Composite {
                    base_type,
                    generics,
                    fields,
                } = c;

                unsafe {
                    self.executor.install(async move {

                    let ctx_for_base =
                        NameResolver::new(base_type, self.node_id).using_context(&self.conversations).await;

                    let ctx_for_base = match ctx_for_base {
                        Ok(v) => v,
                        Err(ie) => todo!("handle import error")
                    };

                    //.expect("couldn't resolve base tyep");

                    let mut inst_fields = HashMap::new();

                    for (fname, fexp) in fields {
                        let fty = self.new_tid();
                        self.do_the_thing_rec(fexp, use_tid, false); // not lval as we load from it

                        inst_fields.insert(fname, fty);
                    }

                    let mut inst_generics = Vec::new();

                    for gen in generics {
                        let ty = self.resolve_typeref(gen, self.generics.clone(), self.node_id).await;

                        inst_generics.push(ty);
                    }

                    //let base_id = self.resolve_typeref(c.base_type, self.type_args, self.node_id).await;
                    let inst = Instance::construct_instance(ctx_for_base, inst_fields, inst_generics);

                    self.instances.borrow_mut().insert(e_ty_id, inst);
                }, "composite resolution block");
                }
            }
        }
    }

    #[allow(unused_mut)]
    pub async fn descend<'func>(
        &mut self,
        f: &'func ast::types::FunctionDefinition,
        imp: &'func mut ExpressionWrapper,
    ) -> ExpressionID {
        //let mut ec = ExpressionContext::new_empty();

        let mut binding_scope = Bindings::fresh();

        let ae = AnyExpression::from_ast(&mut self.acting_on, imp, &mut binding_scope);

        //todo!("descend completed?");
        ae

        //rec(&mut f.implementation).await;
    }

    pub async fn thread(mut self) {
        info!("starts quark thread");

        match &self.node_id.resolve().inner {
            ast::tree::NodeUnion::Function(f, imp) => {
                warn!("quark for a function starts up");
                let mut imp = imp
                    .lock()
                    .unwrap()
                    .take()
                    .expect("someone else got to this impl first?");
                self.entry(&f, &mut imp).await;

                //self.thread_stage_2(f).await;
            }
            _ => {
                // we don't do anything in the other cases, we only make sense in the case of being
                // a function
                warn!(
                    "quark for node {:?} is shutting down, as it is not a function",
                    self.node_id
                );

                while let Ok(v) = self.earpiece.wait().await {
                    info!("Quark got a message");

                    self.earpiece.send(Message {
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
        mut self,
        f: &crate::ast::types::FunctionDefinition,
        imp: &mut ExpressionWrapper, //executor: &'static Executor,
    ) {
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
        self.earpiece.send(Message {
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
        });

        let aeid = self.descend(f, imp).await;

        unsafe {
            self.do_the_thing(aeid).await;
        }

        // start walking the function tree now? or do later?

        //self.thread_stage_2(f).await;
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ResolvedType {
    node: CtxID,
    generics: Vec<ResolvedType>,
}
