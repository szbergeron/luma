use std::{
    borrow::Borrow,
    cell::RefCell,
    collections::{hash_map::DefaultHasher, HashMap},
    fmt::format,
    hash::{Hash, Hasher},
    sync::Mutex,
    time::Duration,
};

use async_recursion::async_recursion;
use either::Either;
use futures::future::join_all;
use itertools::Itertools;
use local_channel::mpsc::Sender;
use rand::random;
use smallstr::SmallString;

use crate::{
    ast::{
        executor::{Executor, Thunk},
        resolver2::{ImportError, NameResolver},
        tree::{CtxID, NodeUnion},
    },
    avec::AtomicVec,
    compile::per_module::{
        static_pinned_leaked, Content, ConversationContext, Earpiece, Message, Service,
    },
    cst::{self, ScopedName, SyntacticTypeReference, StructuralTyAttrs},
    helper::{
        interner::{IStr, Internable, SpurHelper},
        SwapWith,
    },
    lir::expressions::{Lower, UntypedVar, VarType, Variable},
};

use super::{
    expressions::{
        AnyExpression, Assign, Binding, Composite, ExpressionID, If, Invoke, Literal, VarID,
    },
    quark::{Quark, ResolvedType},
    transponster::Transponster,
};

#[derive(Debug, Clone)]
pub enum Note {
    MonoType { ty: Monomorphization },

    MonoFunc { func: Monomorphization },
}

pub struct Scribe {
    local_transponster: &'static Transponster,
    local_quark: &'static Quark,

    conversations: ConversationContext,

    sender: Sender<Message>,

    monomorphizations: AtomicVec<Monomorphization>,

    monomorphization_workers: RefCell<HashMap<ResolvedType, Thunk<()>>>,

    executor: &'static Executor,

    for_node: CtxID,
}

impl Scribe {
    pub fn for_node(
        for_node: CtxID,
        sender: Sender<Message>,
        executor: &'static Executor,
        quark: &'static Quark,
        transponster: &'static Transponster,
    ) -> Self {
        let cs = sender.clone();

        Self {
            for_node,
            monomorphization_workers: Default::default(),
            local_transponster: transponster,
            local_quark: quark,
            conversations: unsafe { ConversationContext::new(cs) },
            monomorphizations: AtomicVec::new(),
            executor,
            sender,
        }
    }

    pub async fn thread(&'static self, mut ep: Earpiece) {
        while let Ok(v) = ep.wait().await {
            let v = self.conversations.dispatch(v);
            if let Some(remainder) = v {
                match remainder.content {
                    Content::Scribe(Note::MonoType { ty }) => {
                        self.handle(ty);
                    }
                    Content::Scribe(Note::MonoFunc { func }) => {
                        self.handle(func);
                    }
                    _ => unreachable!("got sent bad msg: {remainder:?}"),
                }
            }
        }
    }

    pub async fn resolve_name(
        &'static self,
        name: ScopedName,
        within_monomorphization: &'static Monomorphization,
    ) -> Result<CtxID, ImportError> {
        let nr = NameResolver {
            service: Service::Scribe(),
            name,
            based_in: within_monomorphization.of,
            reply_to: self.for_node,
        };

        let r = nr.using_context(&self.conversations).await;

        r
    }

    pub fn handle(&'static self, mono: Monomorphization) {
        let which = match &mono.of.resolve().inner {
            NodeUnion::Type(_) => Either::Right(self.local_transponster),
            NodeUnion::Function(_, _) => Either::Left(self.local_quark),
            _ => unreachable!(),
        };

        let leaked_mono = static_pinned_leaked(mono);

        unsafe {
            self.executor.install(
                async move {
                    let mut m = ScribeOne::new(which, leaked_mono, self);

                    m.codegen().await
                },
                "monomorphization worker",
            )
        };
    }

    #[async_recursion(?Send)]
    pub async fn resolve_typeref(
        &'static self,
        tr: SyntacticTypeReference,
        //generics: &HashMap<IStr, Monomorphization>,
        within_monomorphization: &'static Monomorphization,
    ) -> ResolvedType {
        match tr.inner {
            crate::cst::SyntacticTypeReferenceInner::Unconstrained() => {
                panic!("shouldn't be getting these in scribe")
            }
            crate::cst::SyntacticTypeReferenceInner::Tuple(_) => todo!(),
            crate::cst::SyntacticTypeReferenceInner::Single { name } => {
                let r = self.resolve_name(name, within_monomorphization).await;

                ResolvedType {
                    node: r.unwrap(),
                    generics: vec![],
                }
            }
            crate::cst::SyntacticTypeReferenceInner::Generic { label } => {
                println!("Looking for match for generic {label}");
                println!("Mono is {within_monomorphization:?}");
                //todo!("need to pass generics into typeref resolver, need to fetch them from scribe context")
                let (gn, gt) = within_monomorphization
                    .with
                    .iter()
                    .find(|v| v.0 == label)
                    .expect("no matching generic for tr")
                    .clone();

                gt
            }
            crate::cst::SyntacticTypeReferenceInner::Parameterized { name, generics } => {
                let base = self
                    .resolve_name(name, within_monomorphization)
                    .await
                    .expect("bad tr");

                let each = join_all(
                    generics
                        .into_iter()
                        .map(|sr| self.resolve_typeref(sr, within_monomorphization)),
                )
                .await;

                ResolvedType {
                    node: base,
                    generics: each,
                }
            }
            crate::cst::SyntacticTypeReferenceInner::Reference { to, mutable } => todo!(),
            crate::cst::SyntacticTypeReferenceInner::Pointer { to, mutable } => todo!(),
        }
    }
}

pub struct ScribeOne<'a> {
    using: Either<&'static Quark, &'a Transponster>,
    vars: Generator,

    binds: HashMap<VarID, Variable>,

    mono: &'static Monomorphization,

    within: &'static Scribe,
}

pub struct Generator {
    seed: RefCell<usize>,
}

impl Generator {
    pub fn new() -> Self {
        Self {
            seed: RefCell::new(1),
        }
    }

    pub fn new_temp(&self) -> SmallString<[u8; 10]> {
        let seed = self.next_seed();

        let mut ret = SmallString::new();
        use std::fmt::Write;
        write!(&mut ret, "v_{}", seed).unwrap();

        ret
    }

    fn next_seed(&self) -> usize {
        let mut b = self.seed.borrow_mut();
        *b = *b + 1;

        *b
    }
}

pub fn indents(amount: usize) -> String {
    (0..amount).map(|e| "  ").join("")
}

lazy_static! {}

impl<'a> ScribeOne<'a> {
    pub fn new(
        using: Either<&'static Quark, &'a Transponster>,
        mono: &'static Monomorphization,
        within: &'static Scribe,
    ) -> Self {
        Self {
            within,
            using,
            mono,
            vars: Generator::new(),
            binds: HashMap::new(),
        }
    }

    pub async fn codegen(&mut self) {
        match self.using {
            Either::Left(q) => self.codegen_fn_entry(q).await,
            Either::Right(t) => self.codegen_ty(t).await,
        }
    }

    #[async_recursion(?Send)]
    async fn to_lir_nondyn(
        &mut self,
        quark: &'static Quark,
        eid: ExpressionID,
        do_deref: bool,
    ) -> Variable {
        let exp = quark.acting_on.borrow().get(eid).clone();

        match exp {
            AnyExpression::StaticAccess(sa) => {
                if let Some(tid) = quark.meta.are_methods.borrow().get(&eid).copied() {
                    // we know which specific method it is, so can return that directly
                    let t = quark
                        .with_instance(tid, |inst| unsafe { inst.once_resolved.clone().wait() })
                        .await;
                    let mono = Monomorphization::from_resolved(t);

                    todo!()
                } else {
                    // it's a field, so emit a FieldAccess
                    let on = sa.on;
                    let on = self.to_lir_nondyn(quark, on, false).await;

                    let type_of_field = quark
                        .with_instance(quark.typeofs.get(eid), |inst| unsafe {
                            inst.once_resolved.clone().wait()
                        })
                        .await;

                    let into_ty = VarType::just(type_of_field).ptrto();

                    let into_v = Variable::temp(into_ty);

                    let fa = Lower::Field(on, sa.field, into_v.clone()); // need to forward this to
                                                                         // execute

                    into_v
                }
            }

            AnyExpression::Block(b) => {
                //
                todo!()
            }

            AnyExpression::Assign(a) => {
                let rhs_v = self.to_lir_nondyn(quark, a.rhs, true).await;
                let lhs_v = self.to_lir_nondyn(quark, a.lhs, false).await; // do lhs second so we avoid
                                                                           // overlapping field
                                                                           // instantiations which can
                                                                           // cause issues
                todo!()
            }

            AnyExpression::Invoke(c) => {
                let to_invoke = self.to_lir_nondyn(quark, c.target_fn, todo!()).await;

                //

                todo!()
            }
            AnyExpression::Binding(b) => {
                let Binding {
                    info,
                    name,
                    introduced_as,
                    has_type,
                    from_source,
                } = b;

                let src = self.to_lir_nondyn(quark, from_source, true);

                let ty = quark
                    .with_instance(
                        *quark.type_of_var.borrow().get(&introduced_as).unwrap(),
                        |inst| unsafe { inst.once_resolved.clone().wait() },
                    )
                    .await;

                let ty = VarType::just(ty);

                let v = Variable::temp(ty);

                todo!()
            }

            AnyExpression::Variable(var, _) => match do_deref {
                _ => todo!(),
            },
            _ => todo!(),
        }
    }

    #[async_recursion(?Send)]
    async fn to_rs_nondyn(
        &mut self,
        quark: &'static Quark,
        cur_eid: ExpressionID,
        preamble: &mut Vec<String>,
        code: &mut Vec<String>,
        indent: usize,
    ) -> Option<UntypedVar> {
        let exp = quark.acting_on.borrow().get(cur_eid).clone();
        let ind = indents(indent);

        match exp {
            AnyExpression::Block(b) => None,
            _ => {
                println!("Bad: unhandled exp");
                None
            }
        }
    }

    #[async_recursion(?Send)]
    async fn to_rs_dyn(
        &mut self,
        quark: &'static Quark,
        cur_eid: ExpressionID,
        preamble: &mut Vec<String>,
        code: &mut Vec<String>,
        indent: usize,
    ) -> Option<UntypedVar> {
        let exp = quark.acting_on.borrow().get(cur_eid).clone();
        let ind = indents(indent);

        match exp {
            AnyExpression::Block(b) => {
                let tv = UntypedVar::temp();

                code.push(format!("{ind}let mut {tv} = Value::Uninhabited();"));

                code.push(format!("{ind}{{"));

                for expr in b.expressions {
                    let r = self
                        .to_rs_dyn(quark, expr, preamble, code, indent + 1)
                        .await;
                    code.push(format!("{ind};"));
                    if let Some(r) = r {
                        code.push(format!("{ind}  {tv} = {r}.clone();"));
                    }
                }

                code.push(format!("{ind}}}"));

                Some(tv)
            }
            AnyExpression::Binding(b) => {
                let Binding {
                    info,
                    name,
                    introduced_as,
                    has_type,
                    from_source,
                } = b;

                let var_for = UntypedVar::from(introduced_as);

                // don't need this in preamble, since because of binding it can get dropped at end
                // of scope and this is naturally handled by rust itself
                code.push(format!(
                    "{ind}let mut {var_for}: Value = Value::Uninhabited();"
                ));

                let from = self
                    .to_rs_dyn(quark, from_source, preamble, code, indent + 1)
                    .await
                    .expect("tried to let from an expr with no rval");

                code.push(format!("{ind}{var_for} = {from}.clone();"));

                None
            }
            AnyExpression::Assign(a) => {
                let from = self
                    .to_rs_dyn(quark, a.rhs, preamble, code, indent + 1)
                    .await
                    .expect("no");
                let into = self
                    .to_rs_dyn(quark, a.lhs, preamble, code, indent + 1)
                    .await
                    .expect("huh");

                code.push(format!(
                    "{ind}__luma_assign(&mut {from} as *mut Value, &mut {into} as *mut Value);"
                ));

                Some(from) // assign through
            }
            AnyExpression::Composite(c) => {
                let Composite {
                    info,
                    base_type,
                    generics,
                    fields,
                } = c;

                let t_of = quark
                    .with_instance(quark.typeofs.get(cur_eid), |inst| unsafe {
                        inst.once_resolved.clone().wait()
                    })
                    .await;

                let mut hasher = DefaultHasher::new();
                t_of.hash(&mut hasher);

                let tid = hasher.finish();

                let v = UntypedVar::temp();

                code.push(format!("{ind}let mut {v} = DynamicObject::new({tid});"));

                for (name, exp) in fields {
                    let val = self
                        .to_rs_dyn(quark, exp, preamble, code, indent + 1)
                        .await
                        .expect("in composite the field arms should have values");

                    code.push(format!(
                        "{ind}__luma_assign(&mut {val} as *mut Value,
                        __luma_get_field(&mut {v} as *mut Value, \"{name}\".to_owned()));"
                    ));
                }

                self.emit_composite_builtup(preamble, code, indent, &v, t_of)
                    .await;

                //code.push(format!("todo!(\"emit methods in here into composite\");"));

                Some(v)
            }

            AnyExpression::StaticAccess(sa) => {
                let base = self
                    .to_rs_dyn(quark, sa.on, preamble, code, indent + 1)
                    .await
                    .expect("sa on something with no var?");

                let v = UntypedVar::temp();

                let fname = sa.field;
                code.push(format!(
                    "{ind}let mut {v} = Value::Ref(__luma_get_field(&mut {base}, \"{fname}\".to_owned()));"
                ));

                Some(v)
            }

            AnyExpression::Variable(v, _) => {
                let var_for = UntypedVar::from(v);

                Some(var_for)
            }
            AnyExpression::Invoke(i) => {
                let Invoke {
                    info,
                    target_fn,
                    args,
                } = i;

                code.push(format!("{ind}//finding what an access is on..."));

                let on = self
                    .to_rs_dyn(quark, target_fn, preamble, code, indent)
                    .await
                    .expect("can't call a nonexist");

                let mut argl = Vec::new();

                for arg in args.clone() {
                    let a = self
                        .to_rs_dyn(quark, arg, preamble, code, indent + 1)
                        .await
                        .expect("arg needs to be something");
                    argl.push(format!("{a}.clone()"))
                }

                let argl_s = argl.join(", ");

                let on_fn = UntypedVar::temp();

                code.push(format!("{ind}let mut {on_fn} = __luma_as_callable(&{on});"));

                let on_casted = UntypedVar::temp();

                let argl_t = args.iter().map(|a| "Value".to_owned()).join(", ");

                code.push(format!(
                    "{ind}let mut {on_casted}: fn({argl_t}) -> Value = unsafe {{ std::mem::transmute({on_fn}) }};"
                ));

                //let tl =
                let ret_v = UntypedVar::temp();

                code.push(format!("{ind}let mut {ret_v} = {on_casted}({argl_s});"));

                Some(ret_v)
            }
            AnyExpression::Literal(l) => {
                let Literal {
                    info,
                    has_type,
                    value,
                } = l;

                let uv = UntypedVar::temp();

                //let rt = self.
                /*code.push(format!(
                    "{ind}todo!(\"figure out the type of the literal thing to put object stuff\");"
                ));*/

                let methods_object = UntypedVar::temp();

                code.push(format!(
                    "{ind}let {methods_object}: ObjectHandle = DynamicObject::new_as_handle(0);"
                ));

                let methods_value = UntypedVar::temp();

                code.push(format!(
                    "{ind}let mut {methods_value} = Value::Object({methods_object}.clone());"
                ));

                let lit_tr = match value.contents {
                    cst::Literal::i32Literal(i) => {
                        code.push(format!(
                            "{ind}let mut {uv} = Value::I32({i}, {methods_object});"
                        ));

                        "std::primitive::i32"
                    }
                    cst::Literal::i64Literal(i) => {
                        code.push(format!(
                            "{ind}let mut {uv} = Value::I64({i}, {methods_object});"
                        ));

                        "std::primitive::i64"
                    }
                    cst::Literal::u64Literal(u) => {
                        code.push(format!(
                            "{ind}let mut {uv} = Value::I64({u}, {methods_object});"
                        ));

                        "std::primitive::u64"
                    }
                    cst::Literal::UnknownIntegerLiteral(u) => {
                        code.push(format!(
                            "{ind}let mut {uv} = Value::I64({u}, {methods_object});"
                        ));

                        "std::primitive::i64"
                    }
                    _ => todo!(),
                };

                let lit_res_ty = self
                    .within
                    .resolve_name(ScopedName::from_many(lit_tr), self.mono)
                    .await
                    .unwrap();

                self.emit_composite_builtup(
                    preamble,
                    code,
                    indent,
                    &methods_value,
                    ResolvedType {
                        node: lit_res_ty,
                        generics: vec![],
                    },
                )
                .await;

                Some(uv)
            }
            AnyExpression::OuterReference(sn, _) => {
                let refs = self
                    .within
                    .resolve_name(sn.clone(), self.mono)
                    .await
                    .unwrap();

                let now_mono = Monomorphization {
                    of: refs,
                    with: vec![],
                };

                let name = now_mono.encode_name();

                let stored_in = UntypedVar::temp();

                code.push(format!("{ind}//creating an outer reference from sn {sn:?}"));
                code.push(format!(
                    "{ind}let mut {stored_in} = Value::Callable({name} as *const fn());"
                ));

                Some(stored_in)
            }
            AnyExpression::If(ie) => {
                let If {
                    condition,
                    then_do,
                    else_do,
                } = ie;

                let res_var = UntypedVar::temp();

                let cond_var = UntypedVar::temp();

                code.push(format!("{ind}//eval cond of if expression into a var"));
                let cond_var_unbool = self
                    .to_rs_dyn(quark, condition, preamble, code, indent)
                    .await
                    .unwrap();

                code.push(format!(
                    "{ind}let mut {cond_var}: bool = {cond_var_unbool}.is_true();"
                ));

                code.push(format!("let mut {res_var} = {ind} if {cond_var} {{"));

                let then_var = self
                    .to_rs_dyn(quark, then_do, preamble, code, indent)
                    .await
                    .unwrap();

                code.push(format!("{ind}; {then_var} }} else {{"));

                //let else_var = else_do.map(|e| self.to_rs_dyn(quark, e, preamble, code, indent).await).flatten();

                let else_var = match else_do {
                    Some(e) => Some(self.to_rs_dyn(quark, e, preamble, code, indent).await),
                    None => None,
                }
                .flatten();

                let else_var = match else_var {
                    Some(v) => v,
                    None => {
                        let else_fillin = UntypedVar::temp();
                        code.push(format!("{ind}; let mut {else_fillin} = ();"));

                        else_fillin
                    }
                };

                code.push(format!("{ind}; {else_var} }};"));

                Some(res_var)
            }
            _ => todo!(),
        }
    }

    async fn emit_composite_builtup(
        &mut self,
        preamble: &mut Vec<String>,
        code: &mut Vec<String>,
        indent: usize,
        v: &UntypedVar,
        t_of: ResolvedType,
    ) {
        let ind = indents(indent);
        for child in t_of.node.resolve().children.iter() {
            let c = child.value();

            match &c.resolve().inner {
                NodeUnion::Function(f, _) => {
                    let inner_gens = c.resolve().generics.clone();
                    tracing::error!("this is almost definitely unsound but is enough for the demo, didn't have time to finish it");
                    let r = if !inner_gens.is_empty() {
                        assert!(
                            inner_gens.len() == t_of.node.resolve().generics.len(),
                            "so we can parameterize directly down"
                        );

                        let rt = ResolvedType {
                            node: *c,
                            generics: t_of.generics.clone(),
                        };

                        let mono = Monomorphization::from_resolved(rt);

                        mono
                    } else {
                        let mono = Monomorphization {
                            of: *c,
                            with: vec![],
                        };

                        mono
                    };

                    let proper_name = r.encode_name();

                    let mt = UntypedVar::temp();
                    code.push(format!(
                        "{ind}let mut {mt} = Value::Callable({proper_name} as *const fn());"
                    ));

                    let method_name = *child.key();

                    code.push(format!("{ind}__luma_assign(&mut {mt} as *mut Value,
                                __luma_get_field(&mut {v} as *mut Value, \"{method_name}\".to_owned()));"));
                }
                _ => {}
            }
        }
    }

    async fn codegen_builtin_fn(
        &mut self,
        params: Vec<(IStr, SyntacticTypeReference)>,
        returns: SyntacticTypeReference,
        builtin_name: String,
    ) {
    }

    async fn codegen_fn_entry(&mut self, quark: &'static Quark) {
        self.codegen_fn_real(quark).await
        /*match quark.meta.is_builtin.get().unwrap() {
            Some(br) => self.codegen_fn_builtin(quark).await,
            None => self.codegen_fn_real(quark).await,
        }*/
    }

    async fn codegen_fn_builtin(&mut self, quark: &'static Quark) {}

    async fn codegen_fn_real(&mut self, quark: &'static Quark) {
        let r: u64 = random();
        let r = r % 5;
        //std::thread::sleep(Duration::from_secs(r));

        let fname = self.mono.encode_name();
        println!("Encoding a function, named {fname}");

        let mut within = vec!["".to_owned()];

        //let ent = quark.acting_on.borrow().get(entry_fn_id);

        let is_builtin = quark.meta.is_builtin.get().unwrap();

        let ret_tid = quark.meta.returns.get().copied().unwrap();

        let params_tid = quark.meta.params.get().cloned().unwrap();
        println!("Resolving ret mono");
        let ret_mono = Monomorphization::from_resolved(quark.resolved_type_of(ret_tid).await);
        let ret = format!("{}*", ret_mono.encode_name());
        println!("Got ret mono: {ret}");

        if is_builtin.is_none() {
            /*
            let params_resolved = join_all(params_tid.clone().into_iter().map(
                |(name, tid, pid)| async move {
                    println!("Resolving param {name}");
                    // NOTE to self: just pass in generics here to resolved_type_of, and those can
                    // be used to constrain things
                    let mono = Monomorphization::from_resolved(quark.resolved_type_of(tid).await);
                    println!("Resolved param {name}");

                    format!("\n\t{}* {}", mono.encode_name(), name)
                },
            ))
            .await
            .join(",");*/

            //within.push(format!("{ret} {fname} ({params_resolved}) {{"));

            let entry_fn_id = quark.meta.entry_id.get().copied().unwrap();

            match OUTPUT_TYPE {
                OutputType::FullInf() => {
                    //let params = Vec::new();

                    let params = params_tid
                        .iter()
                        .map(|(pn, pt, pid)| {
                            let v = UntypedVar::from(*pid);
                            format!("mut {v}: Value")
                        })
                        .join(", ");
                    within.push(format!("pub fn {fname}({params}) -> Value {{"));
                    let mut preamble = Vec::new();
                    let mut code = Vec::new();
                    let res = self
                        .to_rs_nondyn(quark, entry_fn_id, &mut preamble, &mut code, 2)
                        .await;

                    if let Some(v) = res {
                        code.push(format!("    {v}"));
                    }

                    within.append(&mut preamble);
                    within.append(&mut code);
                }
                OutputType::AssumeTypeSafe() => todo!(),
                OutputType::AssumeTypeUnsafe() => {
                    let params = params_tid
                        .iter()
                        .map(|(pn, pt, pid)| {
                            let v = UntypedVar::from(*pid);
                            format!("mut {v}: Value")
                        })
                        .join(", ");
                    within.push(format!("pub fn {fname}({params}) -> Value {{"));
                    let mut preamble = Vec::new();
                    let mut code = Vec::new();
                    let res = self
                        .to_rs_dyn(quark, entry_fn_id, &mut preamble, &mut code, 2)
                        .await;

                    if let Some(v) = res {
                        code.push(format!("    {v}"));
                    }

                    within.append(&mut preamble);
                    within.append(&mut code);
                }
            }
            //self.codegen_fn_rec(quark, entry_fn_id, &mut within).await;

            within.push(format!("}}"));
        } else {
            within.push("// encoding a builtin".to_owned());
            within.push(format!("// builtin is {is_builtin:?}"));

            let builtin_inner = *is_builtin
                .as_ref()
                .unwrap()
                .impls
                .get(&OUTPUT_TYPE)
                .unwrap();

            match OUTPUT_TYPE {
                OutputType::FullInf() => {
                    println!("TODO: encode builtins for fullinf");
                }
                OutputType::AssumeTypeUnsafe() => {
                    let mut param_names = Vec::new();
                    let params = params_tid
                        .iter()
                        .map(|(pn, pt, pid)| {
                            let v = UntypedVar::from(*pid);
                            param_names.push(v.clone());

                            format!("mut {v}: Value")
                        })
                        .join(", ");

                    let args = param_names.iter().map(|v| v).join(", ");

                    within.push(format!("pub fn {fname}({params}) -> Value {{"));
                    within.push(format!("    {builtin_inner}({args})"));

                    let mut preamble = Vec::new();
                    let mut code = Vec::new();

                    within.append(&mut preamble);
                    within.append(&mut code);
                    within.push(format!("}}"));
                }
                OutputType::AssumeTypeSafe() => todo!(),
            }
        }

        println!("Done encoding a function");

        LINES.lock().unwrap().append(&mut within);
    }

    async fn codegen_fn_rec(
        &mut self,
        quark: &Quark,
        at_id: ExpressionID,
        within: &mut Vec<String>,
    ) {
    }

    async fn codegen_ty(&mut self, transponster: &Transponster) {
        println!("Encoding a type");

        let name = self.mono.encode_name();

        let mut lines: Vec<String> = vec!["\n".to_owned()];

        match OUTPUT_TYPE {
            OutputType::FullInf() => {
                lines.push(format!("struct {name} {{"));

                //lines.push("\tuint32_t _luma_private__refcount,".to_owned());

                for (name, dynf) in transponster.dynamic_fields.borrow().iter() {
                    if let Some(res) = unsafe { dynf.committed_type.clone().wait().await } {
                        let mono = Monomorphization::from_resolved(res);

                        let n = mono.encode_name();

                        let StructuralTyAttrs { is_ref, is_modif } = mono.of.resolve().get_struct_attrs();

                        if is_ref {
                            lines.push(format!("\t{name}: Option<Rc<{n}>>,"));
                        } else {
                            lines.push(format!("\t{name}: {n},"));
                        }
                    }
                }

                for (rfn, rft) in transponster.regular_fields.iter() {
                    if transponster.methods.contains(rfn) {
                        continue;
                    }

                    let ft = self
                        .within
                        .resolve_typeref(rft.resolve().unwrap(), self.mono)
                        .await;

                    let mono = Monomorphization::from_resolved(ft);

                    let mt = mono.encode_name();

                    let StructuralTyAttrs { is_ref, is_modif } = mono.of.resolve().get_struct_attrs();

                    if is_ref {
                        lines.push(format!("\t{rfn}: Option<Rc<{mt}>>,"));
                    } else {
                        lines.push(format!("\t{rfn}: {mt},"));
                    }
                }

                // do static fields

                //for (name, statf) in self.mono.of.resolve().

                lines.push(format!("}}"));

                /*lines.push(format!("\n{name}* default_{name}() {{"));
                lines.push(format!("}}"));*/

                //lines.push("struct s)
            }
            _ => {
                // don't encode anything for particular types here, they don't really exist!
            }
        }

        LINES.lock().unwrap().append(&mut lines);
    }
}

lazy_static! {
    static ref LINES: Mutex<Vec<String>> = Mutex::new(Vec::new());
}

pub fn get_lines() -> Vec<String> {
    let mut mg = LINES.lock().unwrap();
    let r = (&mut *mg).swap_with(Vec::new());

    r
}

//const OUTPUT_TYPE: OutputType = OutputType::AssumeTypeUnsafe();
const OUTPUT_TYPE: OutputType = OutputType::FullInf();

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
pub enum OutputType {
    FullInf(),
    AssumeTypeSafe(),
    AssumeTypeUnsafe(),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Monomorphization {
    pub of: CtxID,

    pub with: Vec<(IStr, ResolvedType)>,
}

impl Monomorphization {
    pub fn encode_ref(&self) -> IStr {
        todo!()
    }

    pub fn encode_name(&self) -> IStr {
        let tr = self.of.resolve().canonical_typeref().resolve().unwrap();

        let base_str = tr.as_c_id().resolve();

        let mut hasher = DefaultHasher::new();
        self.with.hash(&mut hasher);
        let hashed_tail = format!("{base_str}_{}", hasher.finish());

        let hashed_tail: String = hashed_tail
            .chars()
            .map(|c| match c {
                '*' => "mul".to_owned(),
                '/' => "div".to_owned(),
                '+' => "add".to_owned(),
                '_' => "_".to_owned(),
                '=' => "eq".to_owned(),
                '<' => "lt".to_owned(),
                '>' => "gt".to_owned(),
                '!' => "not".to_owned(),
                other if other.is_ascii_alphanumeric() => other.to_string(),
                _ => "".to_owned(),
            })
            //.filter(|c| c.is_ascii_alphanumeric() || *c == '_')
            .collect();

        hashed_tail.intern()
    }

    pub fn from_resolved(r: ResolvedType) -> Self {
        let generics = r
            .node
            .resolve()
            .generics
            .clone()
            .into_iter()
            .map(|(n, tr)| n)
            .zip(r.generics.into_iter())
            .collect();

        Self {
            of: r.node,
            with: generics,
        }
    }
}
