use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
    sync::Arc,
};

//use itertools::Itertools;

use tracing::{info, warn};
use uuid::Uuid;

use crate::{
    ast::{
        self,
        executor::{self, Executor, UnsafeAsyncCompletable},
        tree::CtxID,
        types::AbstractTypeReference,
    },
    avec::{AtomicVec, AtomicVecIndex},
    compile::per_module::{Content, ControlMessage, Destination, Earpiece, Message, Service},
    cst::{GenericHandle, SyntacticTypeReferenceInner, SyntacticTypeReferenceRef},
    helper::{
        interner::{IStr, Internable},
        CompilationError,
    },
    mir::{
        expressions::{AnyExpression, Bindings, ExpressionContext},
        transponster::Memo,
    },
};

use super::{
    expressions::ExpressionID,
    transponster::{FieldID, Instance, InstanceID, UsageHandle},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TypeID(AtomicVecIndex);

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
    type_args: Vec<SymbolicType>,

    typer: TypeContext,

    //value: Linear,

    //allocations: HashMap<usize, Allocation>,
    //allocations: Vec<Allocation>,

    //allocation_references: HashMap<usize, OwnedAllocationReference>,

    //variables: Vec<(IStr, AllocationReference)>,
    //frames: Vec<usize>,
    acting_on: ExpressionContext,

    type_of: HashMap<ExpressionID, TypeID>,

    //wait_resolve: HashMap<TypeID, Vec<executor::UnsafeAsyncCompletable<Result<(), ()>>>>,

    /// If we resolve a TypeID from here, we should tell any usages/instances
    /// that there is a new direct
    dynfield_notifies: HashMap<TypeID, Vec<(InstanceID, UsageHandle)>>,

    /// If we've resolved a type far enough to know that it is an Instance,
    /// it is recorded here so we can do field analysis on it
    instances: HashMap<TypeID, Instance>,

    once_know: HashMap<TypeID, Vec<Action>>,

    earpiece: Earpiece,

    executor: &'static Executor,

    node_id: CtxID,
}

pub enum Action {
    LoadFieldInto(IStr, TypeID),
    StoreFieldFrom(IStr, TypeID),

    /// Provide Vec<Parameters> as .0, the return value is stored into .1
    CallWith(Vec<TypeID>, TypeID),
}

impl Quark {
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
    fn resolve_typeref(
        &mut self,
        tr: SyntacticTypeReferenceRef,
        with_generics: HashMap<IStr, TypeID>,
        from_base: CtxID,
    ) -> TypeID {
        match tr.resolve().unwrap().inner {
            SyntacticTypeReferenceInner::Single { name } => {
                if let [one] = name.scope.as_slice() && let Some(v) = with_generics.get(one) {
                    // we are ourselves just a generic, so pass through the type ID ffrom the
                    // generic
                    *v
                } else {
                    // we are maybe an instance of a type? or some node? so figure out which one it
                    // is
                    todo!("new instance")
                }
            }
            SyntacticTypeReferenceInner::Unconstrained() => todo!(),
            SyntacticTypeReferenceInner::Tuple(t) => {
                todo!("need to actually make tuple types")
            }
            SyntacticTypeReferenceInner::Parameterized { name, generics } => todo!(),
            SyntacticTypeReferenceInner::Reference { to, mutable } => todo!(),
            SyntacticTypeReferenceInner::Pointer { to, mutable } => todo!(),
        }
    }

    pub fn instance_for(&mut self, tid: TypeID, instance: Instance) {
        todo!()
    }

    pub fn new_tid(&mut self) -> TypeID {
        todo!()
    }

    pub fn for_node(node_id: CtxID, earpiece: Earpiece, executor: &'static Executor) -> Self {
        warn!("quark is being improperly initialized to make things happy");

        Self {
            type_args: Vec::new(),
            //allocations: Vec::new(),
            //allocation_references: HashMap::new(),
            //variables: Vec::new(),
            //frames: Vec::new(),
            earpiece,
            node_id,
            acting_on: ExpressionContext::new_empty(),
            type_of: HashMap::new(),
            wait_resolve: HashMap::new(),
            executor,
            typer: TypeContext::fresh(),
            dynfield_notifies: todo!(),
            instances: todo!(),
        }
    }

    pub fn add_unify(&mut self, a: TypeID, b: TypeID, reason: IStr) {
        todo!()
    }

    pub async fn do_the_thing(&mut self, on_id: ExpressionID) {
        let root_type_id = self.do_the_thing_rec(on_id);
    }

    pub fn do_the_thing_rec(&mut self, on_id: ExpressionID) -> TypeID {
        tracing::info!("do_the_thing_rec on id: {on_id}");

        let e_ty_id: TypeID = self.typer.register_type(TypeVar {
            within: self.node_id,
            referees: Vec::new(),
            current: TypeType::Unknown(),
        });

        match self.acting_on.get(on_id).clone() {
            AnyExpression::Block(b) => {
                for &eid in b.expressions.iter() {
                    let c_ty = self.do_the_thing_rec(eid);

                    //self.type_of[&eid] = c_ty;
                    self.type_of.insert(eid, c_ty);
                }

                match b.expressions.last() {
                    None => {
                        todo!("block has type unit")
                    }
                    Some(eid) => {
                        self.add_unify(
                            e_ty_id,
                            self.type_of[eid],
                            "a block returns the same type as the last expression in it".intern(),
                        );
                    }
                }
            }
            AnyExpression::Assign(a) => {
                let rhs_tid = self.do_the_thing_rec(a.rhs);
                let lhs_tid = self.do_the_thing_rec(a.lhs);

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
                todo!("for an invocation, we need to figure out specifically what it's on");

                unsafe {
                    self.executor.install(
                        async move {
                            let resulting_type_id = e_ty_id;

                            let fid: FieldID = todo!();
                        },
                        "find type for an invocation, waiting on resolving the type of the base",
                    )
                }
            }
            AnyExpression::StaticAccess(_) => todo!(),
            AnyExpression::DynamicAccess(_) => todo!(),
            AnyExpression::Variable(_) => todo!(),
            AnyExpression::Literal(l) => {}
            AnyExpression::Composite(c) => {
                let ty = SymbolicType {
                    generics: todo!(),
                    typeclass: todo!(),
                };

                todo!()
            }
        }

        e_ty_id
    }

    #[allow(unused_mut)]
    pub async fn descend<'func>(
        &mut self,
        f: &'func mut ast::types::FunctionDefinition,
    ) -> ExpressionID {
        //let mut ec = ExpressionContext::new_empty();

        let mut binding_scope = Bindings::fresh();

        let ae =
            AnyExpression::from_ast(&mut self.acting_on, &f.implementation, &mut binding_scope);

        //todo!("descend completed?");
        ae

        //rec(&mut f.implementation).await;
    }

    pub async fn thread(mut self) {
        info!("starts quark thread");

        match &self.node_id.resolve().inner {
            ast::tree::NodeUnion::Function(f) => {
                warn!("quark for a function starts up");
                self.entry(&mut f.lock().unwrap()).await;

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
        f: &mut crate::ast::types::FunctionDefinition,
        //executor: &'static Executor,
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

        let aeid = self.descend(f).await;

        self.do_the_thing(aeid).await;

        // start walking the function tree now? or do later?

        //self.thread_stage_2(f).await;
    }
}

/// The known type of a variable at a point in the code,
/// but symbolically accounting for generics
#[derive(Clone, Debug)]
pub struct SymbolicType {
    /// A vec of symbolic types within the context that are being resolved
    generics: Vec<TypeID>,

    /// The type that this allocation must provide at this usage site
    typeclass: Option<AbstractTypeReference>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ResolvedType {
    node: CtxID,
    generics: Vec<ResolvedType>,
}

/// No, this has nothing *directly* to do with HKTs,
/// it's just rust doesn't allow anonymous unions
#[derive(Clone, Debug)]
pub enum TypeType {
    /// This type has been unioned with another to
    /// create a new type, that this and the other ID both are the same as
    Refer(TypeID),

    /// A type that refers to a node and has all
    /// of its generics populated with also resolved types
    Resolved(Box<ResolvedType>),

    /// A type with *some* information known,
    /// either the
    Symbolic(Box<SymbolicType>),

    /// If you want to be all academic about it, this
    /// is a "T"
    Unknown(),
}

impl TypeType {}

#[derive(Debug, Clone)]
pub struct TypeVar {
    pub within: CtxID,
    pub referees: Vec<TypeID>,
    pub current: TypeType,
}

#[derive(Debug, Clone)]
pub struct TypeVarLocalID(usize);

#[derive(Debug, Clone)]
pub struct TypeVarID {
    pub for_quark: CtxID,
    pub for_var: TypeVarLocalID,
}

pub struct TypeContext {
    types: AtomicVec<TypeVar>,

    when_resolve: HashMap<TypeID, Rc<UnsafeAsyncCompletable<()>>>,

    dynamic_fields: HashSet<TypeID>,
}

impl TypeContext {
    pub async fn step_until_stable(&mut self) {
        let waiting_for: HashMap<FieldID, TypeVar> = HashMap::new();

        //
    }

    pub fn fresh() -> Self {
        Self {
            types: AtomicVec::new(),
            when_resolve: HashMap::new(),
            dynamic_fields: HashSet::new(),
        }
    }

    //pub async fn hm_app(

    pub fn unify(&mut self, a: TypeID, b: TypeID) -> Result<TypeID, TypeError> {
        let TypeID(root_1) = self.root(a);
        let TypeID(root_2) = self.root(b);

        if root_1 == root_2 {
            return Ok(TypeID(root_1)); // these types are already unioned, they have the same root
        }

        // we can safely do a get_two, since the types are not
        // the same

        //let merged = ar.current.union(&br.current);
        let merged = self.unify_simple(TypeID(root_1), TypeID(root_2))?;

        let nt = TypeVar {
            referees: vec![TypeID(root_1), TypeID(root_2)],
            current: merged,
            within: todo!(),
        };

        let (nti, _) = self.types.push(nt);

        //let (ar, br) = self.get_two(root_1, root_2);
        let ar = self.types.get_mut(root_1);
        ar.current = TypeType::Refer(TypeID(nti));

        let br = self.types.get_mut(root_2);
        br.current = TypeType::Refer(TypeID(nti));

        todo!()
    }

    /*
    /// returns, *unordered*, mut refs to two Type vals by ID
    pub fn get_two(&mut self, a: usize, b: usize) -> (&mut Type, &mut Type) {
        let first = a.min(b);
        let second = a.max(b);

        let (vf, vs) = self.types.split_at_mut(second);
        return (vf[first], vs[0]);
    }
    */

    /*
    /// Takes two roots (non Refer types) and creates a new type with
    /// the combination of their information
    pub fn unify_roots(&mut self, a: usize, b: usize) -> Result<TypeType, TypeError> {
        if a == b {
            // already unified, same type
        } else {
            let unified = TypeType::Symbolic(());
            let ty =
            self.types.indexed_insert(unified);
        }
    }*/

    /// In most cases, simple direct substitution is directly possible
    ///
    /// So, we optimize this case, and try to build the narrowing method as
    /// a more expensive, less intuitive, fallback
    ///
    /// This is called with roots, so a and b must be distinct
    pub fn unify_simple(
        &mut self,
        TypeID(ia): TypeID,
        TypeID(ib): TypeID,
    ) -> Result<TypeType, TypeError> {
        let ta = self.types.get(ia);
        let tb = self.types.get(ib);

        match (&ta.current, &tb.current) {
            (TypeType::Unknown(), TypeType::Unknown()) => Ok(TypeType::Unknown()),
            (v, TypeType::Unknown()) | (TypeType::Unknown(), v) => Ok(v.clone()),
            (TypeType::Resolved(a), TypeType::Resolved(b)) => {
                // TODO: check that the two resolved types are the same type, if they've been
                // resolved but are different then the programmer made a typing error
                todo!()
            }
            (TypeType::Resolved(r), TypeType::Symbolic(s))
            | (TypeType::Symbolic(s), TypeType::Resolved(r)) => {
                // TODO: check that the symbolic constraints are compatible with the resolved
                // constraints
                todo!()
            }
            (TypeType::Symbolic(a), TypeType::Symbolic(b)) => {
                // TODO: merge the types, this is the substitution case
                if a.generics.len() != b.generics.len() {
                    Err(TypeError { components: vec![TypeID(ia), TypeID(ib)], complaint: String::from("types could not be unified because the cardinality of their generics was not compatible") })
                } else {
                    let unified_generics: Vec<(TypeID, TypeID)> = a
                        .generics
                        .iter()
                        .zip(b.generics.iter())
                        .map(|(a, b)| (a.clone(), b.clone()))
                        .collect();

                    for (ga, gb) in unified_generics {
                        let unioned = self.unify(ga.clone(), gb.clone());
                    }

                    todo!()
                }
            }
            (_, TypeType::Refer(_)) | (TypeType::Refer(_), _) => {
                unreachable!("values are roots, so can not be refer")
            }
        }
    }

    /// descends this type ID until the type in question
    /// no longer is a Refer(), so a "root" type has been found
    pub fn root(&self, TypeID(mut id): TypeID) -> TypeID {
        while let TypeType::Refer(TypeID(iid)) = self.types.get(id).current {
            id = iid;
        }

        TypeID(id)
    }

    pub fn register_type(&mut self, t: TypeVar) -> TypeID {
        TypeID(self.types.push(t).0)
    }
}

impl SymbolicType {
    pub fn unknown() -> Self {
        Self {
            generics: todo!(),
            typeclass: todo!(),
        }
    }

    pub fn union(&self, other: SymbolicType) -> Self {
        todo!()
    }

    /// At the moment, we don't support inheritance
    /// But we do support deref
    ///
    /// So this is actually somewhat complicated, as
    /// a variable of type T can come from a source of type *T or &T,
    /// and that coercion needs to be backpropagated (sort of) here
    ///
    /// What we do is we try to find a candidate base that could satisfy the
    /// other bases after some operation(s), and then try to resolve generics on that
    pub fn try_merge(&self, other: SymbolicType) -> Result<SymbolicType, CompilationError> {
        /*let mut bases: Vec<_> = other
            .facts
            .iter()
            .chain(self.facts.iter())
            .filter_map(|e| {
                if let Fact::Base(b) = e {
                    Some(b.clone())
                } else {
                    None
                }
            })
            .collect();

        for b in bases {}*/

        todo!()
    }
}

/*#[derive(PartialEq, Eq, Hash, Clone, Copy)]
enum TypeReferenceBacking {
    Real(AbsoluteTypeReference),
    Indirect(IndirectTypeReference),
}*/

struct NameResolverContext {}

/// An initialization is a branch point for type inference
///
/// When we construct, it takes a set of literals or other expressions
/// and ties them all into the type of a new object
pub struct Initialization {
    is_type: SymbolicType,

    values: Vec<(IStr, Value)>,
}

pub enum Value {
    /// Used when we don't have a literal,
    /// and we aren't composing a nested initialization
    Expression(AllocationReference),

    Initialization(Initialization),

    /// A list of bytes
    Literal(Vec<u8>),

    /// If a value is not provided we can simply zero the field
    /// Shouldn't actually expose syntactically, but used under the hood
    Zeroed(),

    /// Really shouldn't be used, and should almost never be exposed
    /// to the programmer except with intrinsics with a lot of warnings
    Uninit(),

    /// later use, can use for partial initialization similar to rust { ..., ..val } syntax
    Expanded(),
}

/// An Allocation is a typed memory region
/// that can be referenced, assigned into,
/// modified, and read
/// They should not be semantically reused for different values,
/// and should be treated as pseudo SSA
///
/// They are basically temporary registers
pub struct Allocation {
    id: usize,

    name: Option<IStr>,
    note: Option<IStr>,

    allocation_type: TypeID,

    size: Option<usize>,

    source: Source,
}

pub struct FunctionCall {
    //to: CallableReference, TODO
    args: Vec<AllocationReference>,

    returns: AllocationReference,

    generics: Vec<GenericHandle>,
}

pub struct Construction {
    of: AbstractTypeReference,

    /// Each `field:value` pair provided
    inputs: HashMap<IStr, AllocationReference>,

    returns: AllocationReference,
}

pub struct UnresolvedMethodCall {
    named: IStr,

    args: Vec<AllocationReference>,

    returns: AllocationReference,
}

pub enum Source {
    /// If `!` or an analogue is provided,
    /// this does not have a source since
    /// it is unreachable. The type
    /// of this allocation must be inferred
    /// from downstream rather than top-to-bottom
    Never(),

    /// A Literal can be treated as an F(<builtin>) -> T,
    /// the output is inferred but with (some) constraints
    /// placed on it, such as "if the input parameter is an integer,
    /// the inferred type must be integral in nature".
    /// The input itself does have a type, but is casted
    /// into the output type and the cast is optimized away.
    Literal(),

    /// A Function has a set of inputs and a set of outputs,
    /// with generics linking them together. Through
    /// those parameters, and their constraints,
    /// Functions can help with inference both forward and backward,
    /// and are very helpful specifically *because* they
    /// are never overloaded, and thus provide
    /// information that is as helpful as construction
    /// and literals with single output types.
    Function(Arc<FunctionCall>),

    /// When we do Foo { a: b, c: d },
    /// We perform a Construction
    ///
    /// A Construction is *very* similar to a normal
    /// function, and the types are resolved in much the same way.
    ///
    /// A Constructor does not currently allow dynamic members
    /// to be directly populated, instead those are desugared
    /// into dot assignments after construction
    Construction(Arc<Construction>),

    /// Methods are separate from functions because
    /// the target method is, if looking at all base T,
    /// likely overloaded. Thus, until the
    /// base type can be sufficiently inferred to determine
    /// which method(s) are candidates for the call,
    /// this lookup is blocked.
    ///
    /// Once the method is resolved, it is turned into a Function(FunctionCall),
    /// with the target method as the callable
    MethodUnresolved(Arc<UnresolvedMethodCall>),
    //MethodResolved(Arc<ResolvedMethodCall>),
}

impl Allocation {
    /// Returns an allocation with an uninitialized ID
    /// and no constraints
    pub fn todo() {}
    /*pub fn any(ctx: &TypeContext) -> Self {
        Self {
            id: 0,
            name: None,
            note: None,

            allocation_type: ctx.,
            size: None,
            source: todo!(),
        }
    }*/
}

#[derive(Copy, Clone)]
pub struct AllocationReference(usize);

#[derive(Clone, Copy)]
pub struct OwnedAllocationReference {
    id: usize,

    inner: AllocationReferenceInner,
}

#[derive(Clone)]
pub struct TypeError {
    components: Vec<TypeID>,
    complaint: String,
}

#[derive(Clone, Copy)]
pub enum AllocationReferenceInner {
    /// A direct reference "owns" an
    /// allocation, and is used for binds
    ///
    /// The content of Direct is the id of an Allocation
    Direct(usize),

    /// An indirect reference holds the id
    /// of an AllocationReference, saying
    /// it talks about the same reference
    Indirect(usize),
}

impl Quark {
    /*pub async fn thread_stage_2(mut self, f: &mut FunctionDefinition) {
        while let Ok(v) = self.earpiece.wait().await {
            info!("quark got a message");
        }
    }*/

    /*
    /// Convert tree form into serial-evaluated sea of nodes (to be translated down to LLVM later)
    fn build_from(root: ast::types::FunctionDefinition) -> Quark {
        todo!()
    }

    /// Entry point to start the augmented Hindley Milner algorithm from a built quark
    fn do_hm(&mut self) {}

    fn push_frame(&mut self) {
        self.frames.push(self.variables.len())
    }

    fn pop_frame(&mut self) {
        self.variables.truncate(self.frames.pop().unwrap_or(0))
    }

    fn declare_variable(&mut self, named: IStr) -> AllocationReference {
        if let Some((name, alloc)) = self.variables.iter().find(|e| e.0 == named) {
            println!("Shadows a variable named {name}");
        }

        let aref = self.alloc_any();

        self.variables.push((named, aref));

        aref
    }

    fn get_variable(&mut self, named: IStr) -> Option<AllocationReference> {
        self.variables
            .iter()
            .rev()
            .find(|(name, aref)| *name == named)
            .map(|(_, aref)| *aref)
    }

    /// Returns a reference to a blank, new, allocation
    fn alloc_any(&mut self) -> AllocationReference {
        let id = self.allocations.indexed_insert(todo!());
        self.allocations[id].id = id;

        AllocationReference(id)
    }

    /// returns the ID of the root allocation that is referenced
    pub fn allocation_for(&mut self, ar: AllocationReference) -> usize {
        let owned = self.allocation_references.get(&ar.0).unwrap();
        let mut seen = HashSet::new();
        let mut cur = owned;

        loop {
            //let cycle = seen.insert(cur.id);
            match cur.inner {
                AllocationReferenceInner::Direct(d) => {
                    //break self.allocations.get_mut(&d).unwrap()
                    break d;
                }
                AllocationReferenceInner::Indirect(i) => {
                    if seen.contains(&i) {
                        panic!("Detected a reference cycle in allocation set!");
                    } else {
                        seen.insert(i);
                        cur = self.allocation_references.get(&i).unwrap();
                    }
                }
            }
        }
    }

    fn solve_move(&mut self, from: AllocationReference, into: AllocationReference) {}
    */
}
