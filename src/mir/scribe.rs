use std::{
    borrow::Borrow,
    cell::RefCell,
    collections::{hash_map::DefaultHasher, HashMap, HashSet},
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
use once_cell::sync::OnceCell;
use rand::random;
use smallstr::SmallString;

use crate::{
    ast::{
        executor::{Executor, Thunk},
        resolver2::{ImportError, NameResolver},
        tree::{CtxID, NodeUnion, SubMap},
    },
    avec::AtomicVec,
    compile::per_module::{
        static_pinned_leaked, Content, ConversationContext, Destination, Earpiece, Message, Postal,
        Service,
    },
    cst::{self, LiteralExpression, ScopedName, StructuralTyAttrs, SyntacticTypeReference},
    helper::{
        interner::{IStr, Internable, SpurHelper},
        SwapWith,
    },
    lir::expressions::{Lower, UntypedVar, VarType, Variable},
    mir::expressions::{For, StaticAccess}, monitor::Monitor,
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

    mono_set: RefCell<HashSet<Monomorphization>>,

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
            mono_set: Default::default(),
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
        if !self.mono_set.borrow_mut().insert(mono.clone()) {
            //println!("Tried to re-mono a ty");
            return;
        }

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
                tracing::warn!("Looking for match for generic {label}");
                tracing::warn!("Mono is {within_monomorphization:?}");
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
    async fn to_rs_nondyn(
        &mut self,
        quark: &'static Quark,
        submap: &SubMap,
        cur_eid: ExpressionID,
        //preamble: &mut Vec<String>,
        //code: &mut Vec<String>,
        indent: usize,
        is_lval: bool,
    ) -> IStr {
        let exp = quark.acting_on.borrow().get(cur_eid).clone();
        let ind = indents(indent);

        use std::fmt::Write;

        let mut code = String::new();

        match exp {
            AnyExpression::Block(b) => {
                let br = UntypedVar::temp();

                writeln!(code, "{ind}{{").unwrap();

                //writeln!(code, "{ind}let mut {br} = Default::default();");

                for exp in b.statements {
                    let e = self
                        .to_rs_nondyn(quark, submap, exp, indent + 1, false)
                        .await;

                    writeln!(code, "{ind}{e};").unwrap();
                }

                if let Some(e) = b.final_expr {
                    let e = self
                        .to_rs_nondyn(quark, submap, e, indent + 1, is_lval)
                        .await;

                    writeln!(code, "{ind}{e}").unwrap();
                }

                //let exps = results.into_iter().join(format!("{ind};\n").as_str());

                //writeln!(code, "{ind}{exps}");

                writeln!(code, "{ind}}}").unwrap();
            }
            AnyExpression::Literal(l) => {
                assert!(!is_lval, "literal can't be an lval");

                let Literal {
                    info,
                    has_type,
                    value,
                } = l;

                let LiteralExpression {
                    node_info,
                    contents,
                } = value;

                match contents {
                    cst::Literal::i64Literal(i) => {
                        write!(code, "{i}").unwrap();
                    }
                    cst::Literal::UnknownIntegerLiteral(i) => {
                        write!(code, "{i}").unwrap();
                    }
                    cst::Literal::StringLiteral(sl) => {
                        write!(code, "{sl}.to_owned()").unwrap();
                    }
                    cst::Literal::Boolean(b) => {
                        write!(code, "{b}").unwrap();
                    }
                    cst::Literal::f64Literal(f) => {
                        let formatted = format!("{f}");
                        let formatted = if !formatted.contains(".") {
                            format!("{formatted}.0")
                        } else {
                            formatted
                        };
                        write!(code, "({formatted})").unwrap();
                    }
                    cst::Literal::UnitLiteral() => {
                        write!(code, "()").unwrap();
                    }
                    other => todo!("don't handle {other:?} literals"),
                }
            }
            AnyExpression::Variable(v, _i) => {
                let v = UntypedVar::from(v);

                let _ = write!(code, "({v})");
            }
            AnyExpression::Binding(b) => {
                let Binding {
                    info,
                    name,
                    introduced_as,
                    has_type,
                    from_source,
                } = b;

                let _ = writeln!(code, "{ind}//binds variable {name}");

                let v = UntypedVar::from(introduced_as);
                let e = self
                    .to_rs_nondyn(quark, submap, from_source, indent + 1, false)
                    .await;

                let _ = write!(code, "{ind}let mut {v} = {e}.clone();");
            }
            AnyExpression::OuterReference(sn, _i) => {
                /*let refs = self
                .within
                .resolve_name(sn.clone(), self.mono)
                .await
                .unwrap();*/

                let r = quark.resolved_type_of(quark.typeofs.get(cur_eid)).await;
                let r = submap.substitute_of(r);

                let now_mono = Monomorphization::from_resolved(r);

                let name = now_mono.encode_name();

                let _ = writeln!(code, "{name}");
            }
            AnyExpression::Assign(a) => {
                let Assign { info, rhs, lhs } = a;

                let rhs_e = self
                    .to_rs_nondyn(quark, submap, rhs, indent + 1, false)
                    .await;
                let lhs_e = self
                    .to_rs_nondyn(quark, submap, lhs, indent + 1, true)
                    .await;
                let _ = writeln!(code, "{ind}{lhs_e} = {rhs_e}.clone()");
            }
            AnyExpression::StaticAccess(sa) => {
                let StaticAccess { on, field, info } = sa;
                let on_s = self
                    .to_rs_nondyn(quark, submap, on, indent + 1, false)
                    .await;

                if let Some(tid) = quark.meta.are_methods.borrow().get(&cur_eid) {
                    // figure out what specific method this is
                    let rt = quark.resolved_type_of(*tid).await;

                    let mon = Monomorphization::from_resolved(rt);

                    let targ = mon.encode_name();

                    //let _ = write!(code, "/* s_acc is method */ {targ}");
                    let _ = write!(code, "{targ}");
                } else {
                    let base_type = quark.resolved_type_of(quark.typeofs.get(on)).await;

                    let meta_attrs = base_type.node.resolve().get_struct_attrs();

                    if meta_attrs.is_ref {
                        let _ = writeln!(code, "({on_s}).borrow_mut().{field}");
                    } else {
                        let _ = writeln!(code, "({on_s}).{field}");
                    }
                }
            }
            AnyExpression::If(i) => {
                let If {
                    condition,
                    then_do,
                    else_do,
                } = i;

                let if_is = self
                    .to_rs_nondyn(quark, submap, condition, indent + 1, false)
                    .await;
                let then_do = self
                    .to_rs_nondyn(quark, submap, then_do, indent + 1, is_lval)
                    .await;
                let else_do = if let Some(v) = else_do {
                    self.to_rs_nondyn(quark, submap, v, indent + 1, is_lval)
                        .await
                } else {
                    "()".intern()
                };

                let _ = writeln!(code, "if {if_is} {{ {then_do} }} else {{ {else_do} }}");
            }
            AnyExpression::Return(r) => {
                let v = self.to_rs_nondyn(quark, submap, r.inner_exp, indent, false).await;

                let _ = writeln!(code, "return {v}");
            }
            AnyExpression::For(f) => {
                let For {
                    body,
                    pre,
                    post,
                    condition,
                } = f;

                let pre_s = self
                    .to_rs_nondyn(quark, submap, pre, indent + 1, false)
                    .await;
                let cond_s = self
                    .to_rs_nondyn(quark, submap, condition, indent + 1, false)
                    .await;
                let post_s = self
                    .to_rs_nondyn(quark, submap, post, indent + 1, false)
                    .await;
                let body_s = self
                    .to_rs_nondyn(quark, submap, body, indent + 1, false)
                    .await;

                let _ = writeln!(code, "{ind}//for loop");

                let _ = writeln!(code, "{ind}{{");

                let _ = writeln!(code, "{ind}{pre_s};");

                let _ = writeln!(code, "{ind}while( ({cond_s}) ) {{;");

                let _ = writeln!(code, "{ind} {{ {body_s} }};");

                let _ = writeln!(code, "{ind}{post_s};");

                let _ = writeln!(code, "{ind}}}");

                let _ = writeln!(code, "{ind}}}");
            }
            AnyExpression::Invoke(i) => {
                let Invoke {
                    info,
                    target_fn,
                    args,
                } = i;

                let on = self
                    .to_rs_nondyn(quark, submap, target_fn, indent + 1, false)
                    .await;

                let mut args_s = Vec::new();

                for arg in args {
                    let arg = self
                        .to_rs_nondyn(quark, submap, arg, indent + 1, false)
                        .await;
                    let arg = format!("\n{arg}.clone()").intern();
                    args_s.push(arg);
                }

                let args = args_s.into_iter().join(",");

                let _ = writeln!(code, "{on}({args})");
            }
            AnyExpression::Composite(c) => {
                let Composite {
                    info,
                    base_type,
                    generics,
                    fields,
                } = c;

                assert!(generics.len() == 0, "we don't handle generics yet");

                /*let bt = self
                .within
                .resolve_name(base_type, self.mono)
                .await
                .unwrap();*/

                let bt_res = quark.resolved_type_of(quark.typeofs.get(cur_eid)).await;

                let bt_res = submap.substitute_of(bt_res);

                //let bt_mono = submap.substitute_of(ResolvedType { node: bt, generics: vec![] })

                let bt_mono = Monomorphization::from_resolved(bt_res);

                let into_var = UntypedVar::temp();

                let _ = writeln!(code, "{ind}{{");

                let base_mono_name = bt_mono.encode_name();

                let _ = writeln!(code, "let {into_var} = {ind}{base_mono_name} {{");

                for (fname, fexp) in fields {
                    let v = self.to_rs_nondyn(quark, submap, fexp, indent, false).await;

                    let _ = writeln!(code, "{ind}        {fname}: {v},");
                }

                let _ = writeln!(code, "{ind}    ..Default::default() }};");

                let _ = match bt_mono.of.resolve().get_struct_attrs().is_ref {
                    false => writeln!(code, "{ind}{into_var}"),
                    true => writeln!(code, "{ind}FastRefHandle::from_val({into_var})"),
                };

                let _ = writeln!(code, "{ind}}}");

                //Some(into_var)
            }
            _ => {
                println!("Bad: unhandled exp");
            }
        }

        code.intern()
    }

    #[async_recursion(?Send)]
    async fn to_rs_dyn(
        &mut self,
        quark: &'static Quark,
        cur_eid: ExpressionID,
        preamble: &mut Vec<String>,
        code: &mut Vec<String>,
        indent: usize,
        submap: &SubMap,
    ) -> Option<UntypedVar> {
        let exp = quark.acting_on.borrow().get(cur_eid).clone();
        let ind = indents(indent);

        match exp {
            AnyExpression::Block(b) => {
                let tv = UntypedVar::temp();
                code.push(format!("{ind}let mut {tv} = Value::Uninhabited();"));

                //
                if let Some(v) = b.final_expr {}

                code.push(format!("{ind}{{"));

                for expr in b.statements {
                    let r = self
                        .to_rs_dyn(quark, expr, preamble, code, indent + 1, submap)
                        .await;

                    code.push(format!("{ind};"));
                    if let Some(r) = r {
                        //code.push(format!("{ind}  {tv} = {r}.clone();"));
                    }
                }

                match b.final_expr {
                    Some(v) => {
                        let rv = self
                            .to_rs_dyn(quark, v, preamble, code, indent + 1, submap)
                            .await
                            .unwrap();

                        code.push(format!("{ind}{tv} = {rv};"));
                    }
                    None => {}
                };

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
                    .to_rs_dyn(quark, from_source, preamble, code, indent + 1, submap)
                    .await
                    .expect("tried to let from an expr with no rval");

                code.push(format!("{ind}{var_for} = {from}.clone();"));

                None
            }
            AnyExpression::Assign(a) => {
                let from = self
                    .to_rs_dyn(quark, a.rhs, preamble, code, indent + 1, submap)
                    .await
                    .expect("no");
                let into = self
                    .to_rs_dyn(quark, a.lhs, preamble, code, indent + 1, submap)
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
                        .to_rs_dyn(quark, exp, preamble, code, indent + 1, submap)
                        .await
                        .expect("in composite the field arms should have values");

                    code.push(format!(
                        "{ind}__luma_assign(&mut {val} as *mut Value,
                        __luma_get_field(&mut {v} as *mut Value, \"{name}\".to_owned()));"
                    ));
                }

                self.emit_composite_builtup(preamble, code, indent, &v, t_of, submap)
                    .await;

                //code.push(format!("todo!(\"emit methods in here into composite\");"));

                Some(v)
            }

            AnyExpression::StaticAccess(sa) => {
                let base = self
                    .to_rs_dyn(quark, sa.on, preamble, code, indent + 1, submap)
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
                    .to_rs_dyn(quark, target_fn, preamble, code, indent, submap)
                    .await
                    .expect("can't call a nonexist");

                let mut argl = Vec::new();

                for arg in args.clone() {
                    let a = self
                        .to_rs_dyn(quark, arg, preamble, code, indent + 1, submap)
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
                        code.push(format!("{ind}let mut {uv} = luma_slow_new_i32({i});"));

                        "std::i32"
                    }
                    cst::Literal::i64Literal(i) => {
                        code.push(format!(
                            //"{ind}let mut {uv} = Value::I64({i}, {methods_object});"
                            "{ind}let mut {uv} = luma_slow_new_i64({i});"
                        ));

                        "std::i64"
                    }
                    cst::Literal::UnitLiteral() => {
                        code.push(format!("{ind}let mut {uv} = Value::Uninhabited();"));

                        "std::Unit"
                    }
                    cst::Literal::u64Literal(u) => {
                        code.push(format!(
                            "{ind}let mut {uv} = Value::I64({u}, {methods_object});"
                        ));

                        "std::u64"
                    }
                    cst::Literal::UnknownIntegerLiteral(u) => {
                        code.push(format!(
                            //"{ind}let mut {uv} = Value::I64({u}, {methods_object});"
                            "{ind}let mut {uv} = luma_slow_new_i64({u});"
                        ));

                        "std::i64"
                    }
                    cst::Literal::Boolean(b) => {
                        code.push(format!(
                            //"{ind}let mut {uv} = Value::Bool({b}, {methods_object});"
                            "{ind}let mut {uv} = luma_slow_new_bool({b});"
                        ));

                        "std::bool"
                    }
                    cst::Literal::StringLiteral(s) => {
                        code.push(format!(
                            //"{ind}let mut {uv} = Value::String({s}.to_owned(), {methods_object});"
                            "{ind}let mut {uv} = luma_slow_new_string({s});"
                        ));

                        "std::String"
                    }
                    cst::Literal::f64Literal(v) => {
                        let formatted = format!("{v}");
                        let formatted = if !formatted.contains(".") {
                            format!("{formatted}.0")
                        } else {
                            formatted
                        };

                        code.push(format!(
                            //"{ind}let mut {uv} = Value::String({s}.to_owned(), {methods_object});"
                            "{ind}let mut {uv} = luma_slow_new_f64({formatted});"
                        ));

                        "std::f64"
                    }
                    o => todo!("unhandled literal {o:?}"),
                };

                /*
                let lit_res_ty = self
                    .within
                    .resolve_name(ScopedName::from_many(lit_tr), self.mono)
                    .await
                    .unwrap();
                    */

                /*self.emit_composite_builtup(
                    preamble,
                    code,
                    indent,
                    &methods_value,
                    ResolvedType {
                        node: lit_res_ty,
                        generics: vec![],
                    },
                    submap,
                )
                .await;*/

                Some(uv)
            }
            AnyExpression::OuterReference(sn, _) => {
                /*let refs = self
                .within
                .resolve_name(sn.clone(), self.mono)
                .await
                .unwrap();*/

                let ah = Monitor::instance().set_alert(format!("resolving type of eid {:?}", cur_eid).intern());
                let r = quark.resolved_type_of(quark.typeofs.get(cur_eid)).await;
                Monitor::instance().unset_alert(ah);
                let r = submap.substitute_of(r);

                let now_mono = Monomorphization::from_resolved(r);

                let name = now_mono.encode_name();

                let stored_in = UntypedVar::temp();

                code.push(format!("{ind}//creating an outer reference from sn {sn:?}"));
                code.push(format!(
                    "{ind}let mut {stored_in} = Value::Callable({name} as *const fn());"
                ));

                Some(stored_in)
            }
            AnyExpression::For(f) => {
                let For {
                    body,
                    pre,
                    post,
                    condition,
                } = f;

                let res_var = UntypedVar::temp();

                code.push(format!("{ind}let mut {res_var} = Value::Uninhabited();"));

                code.push(format!("{ind}{{"));

                self.to_rs_dyn(quark, pre, preamble, code, indent + 1, submap)
                    .await;

                code.push(format!("{ind}   while({{"));
                let r = self
                    .to_rs_dyn(quark, condition, preamble, code, indent + 1, submap)
                    .await
                    .unwrap();
                code.push(format!("{ind}{r}.is_true() }}) {{"));

                let r = self
                    .to_rs_dyn(quark, body, preamble, code, indent, submap)
                    .await;
                let r = self
                    .to_rs_dyn(quark, post, preamble, code, indent, submap)
                    .await;

                code.push(format!("{ind}}};"));

                code.push(format!("{ind}}};"));

                Some(res_var)
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
                    .to_rs_dyn(quark, condition, preamble, code, indent, submap)
                    .await
                    .unwrap();

                code.push(format!(
                    "{ind}let mut {cond_var}: bool = {cond_var_unbool}.is_true();"
                ));

                code.push(format!("let mut {res_var} = {ind} if {cond_var} {{"));

                let then_var = self
                    .to_rs_dyn(quark, then_do, preamble, code, indent, submap)
                    .await
                    .unwrap();

                code.push(format!("{ind}; {then_var} }} else {{"));

                //let else_var = else_do.map(|e| self.to_rs_dyn(quark, e, preamble, code, indent).await).flatten();

                let else_var = match else_do {
                    Some(e) => Some(
                        self.to_rs_dyn(quark, e, preamble, code, indent, submap)
                            .await,
                    ),
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
            other => todo!("handle {other:?}"),
        }
    }

    async fn emit_composite_builtup(
        &mut self,
        preamble: &mut Vec<String>,
        code: &mut Vec<String>,
        indent: usize,
        v: &UntypedVar,
        t_of: ResolvedType,
        submap: &SubMap,
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
                        let rt = submap.substitute_of(rt);

                        let mono = Monomorphization::from_resolved(rt);

                        mono
                    } else {
                        let rt = ResolvedType {
                            node: *c,
                            generics: vec![],
                        };
                        let rt = submap.substitute_of(rt);
                        let mono = Monomorphization::from_resolved(rt);

                        mono
                    };

                    //let r = submap.substitute_of(r);
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

        let submap = SubMap::new_in(quark, self.mono);
        let smr = &submap;

        let fname = match quark.meta.name.get().unwrap().resolve() {
            "main" => "_luma_main".intern(),
            _ => self.mono.encode_name(),
        };
        //let fname = self.mono.encode_name();
        println!("Encoding a function, named {fname}");

        let mut within = vec!["".to_owned()];

        //let ent = quark.acting_on.borrow().get(entry_fn_id);

        let is_builtin = quark.meta.is_builtin.get().unwrap();

        let ret_tid = quark.meta.returns.get().copied().unwrap();

        let params_tid = quark.meta.params.get().cloned().unwrap();
        tracing::warn!("Resolving ret mono");
        let ah = Monitor::instance().set_alert(format!("waiting for rtype of {fname} to resolve").intern());
        let ret_mono = quark.resolved_type_of(ret_tid).await;
        Monitor::instance().unset_alert(ah);
        let ret_mono = Monomorphization::from_resolved(smr.substitute_of(ret_mono));
        let ret = format!("{}*", ret_mono.encode_name());
        tracing::warn!("Got ret mono: {ret}");

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

            match output_type() {
                OutputType::FullInf() => {
                    //let params = Vec::new();

                    let params = join_all(params_tid.iter().map(|(pn, pt, pid)| {
                        let v = UntypedVar::from(*pid);

                        async move {
                            let ah = Monitor::instance().set_alert(format!("waiting to resolve pt of p by name {pn}").intern());
                            let t = quark.resolved_type_of(*pt).await;
                            Monitor::instance().unset_alert(ah);
                            let t = smr.substitute_of(t);
                            let m = Monomorphization::from_resolved(t);

                            format!("mut {v}: {}", m.encode_ref())
                        }
                    }))
                    .await
                    .join(", ");

                    let rt = ret_mono.encode_ref();

                    //within.push(format!("#[inline(never)]"));
                    within.push(format!("pub fn {fname}({params}) -> {rt}"));

                    let res = self
                        .to_rs_nondyn(quark, &submap, entry_fn_id, 2, false)
                        .await;

                    within.push(format!("{res}"));
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
                        .to_rs_dyn(quark, entry_fn_id, &mut preamble, &mut code, 2, smr)
                        .await;

                    if let Some(v) = res {
                        code.push(format!("    {v}"));
                    }

                    within.append(&mut preamble);
                    within.append(&mut code);
                    within.push(format!("}}"));
                }
            }
            //self.codegen_fn_rec(quark, entry_fn_id, &mut within).await;
        } else {
            within.push("// encoding a builtin".to_owned());
            within.push(format!("// builtin is {is_builtin:?}"));

            let builtin_inner = *is_builtin
                .as_ref()
                .unwrap()
                .impls
                .get(&output_type())
                .unwrap();

            match output_type() {
                OutputType::FullInf() => {
                    //println!("TODO: encode builtins for fullinf");

                    let mut param_vars = Vec::new();

                    let mut param_names = Vec::new();

                    let params = join_all(params_tid.iter().map(|(pn, pt, pid)| {
                        let v = UntypedVar::from(*pid);

                        param_vars.push(v.clone());

                        param_names.push((pn.resolve(), format!("{v}")));

                        async move {
                            let t = quark.resolved_type_of(*pt).await;
                            let t = smr.substitute_of(t);
                            let m = Monomorphization::from_resolved(t);

                            format!("mut {v}: {}", m.encode_ref())
                        }
                    }))
                    .await
                    .join(", ");

                    let rt = ret_mono.encode_ref();

                    within.push(format!("pub fn {fname}({params}) -> {rt}"));

                    let args = param_vars.iter().map(|v| v).join(", ");

                    //for //

                    let mut builtin_substituted = builtin_inner.resolve().to_owned();

                    for (pn, ps) in param_names {
                        builtin_substituted =
                            builtin_substituted.replace(format!("${pn}").as_str(), ps.as_str());
                    }

                    within.push(format!("{{ {builtin_substituted} }}"));

                    //let res = self.to_rs_nondyn(quark, entry_fn_id, 2).await;
                }
                OutputType::AssumeTypeUnsafe() => {
                    let mut param_vars = Vec::new();

                    let mut param_names = Vec::new();

                    let params = params_tid
                        .iter()
                        .map(|(pn, pt, pid)| {
                            let v = UntypedVar::from(*pid);
                            param_vars.push(v.clone());

                            param_names.push((pn.resolve(), format!("{v}")));

                            format!("mut {v}: Value")
                        })
                        .join(", ");

                    let args = param_vars.iter().map(|v| v).join(", ");

                    within.push(format!("pub fn {fname}({params}) -> Value {{"));
                    //within.push(format!("    {builtin_inner}({args})"));

                    let mut builtin_substituted = builtin_inner.resolve().to_owned();
                    for (pn, ps) in param_names {
                        builtin_substituted =
                            builtin_substituted.replace(format!("${pn}").as_str(), ps.as_str());
                    }

                    within.push(format!("{{ {builtin_substituted} }}"));

                    let mut preamble = Vec::new();
                    let mut code = Vec::new();

                    within.append(&mut preamble);
                    within.append(&mut code);
                    within.push(format!("}}"));
                }
                OutputType::AssumeTypeSafe() => todo!(),
            }
        }

        tracing::warn!("Done encoding a function");

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
        tracing::warn!("Encoding a type");

        let attrs = self.mono.of.resolve().get_struct_attrs();
        let mut lines: Vec<String> = vec!["\n".to_owned()];
        let name = self.mono.encode_name();

        for v in self.mono.flattened_contexts() {
            if let NodeUnion::Generic(_) = v.resolve().inner {
                println!("Tried to monomorphize with a generic param, can't do that dave");

                return;
            }
        }

        match output_type() {
            OutputType::FullInf() => {
                if let Some(b) = attrs.is_builtin {
                    let mut base = b.drop_front(1).drop_end(1).resolve().to_owned();

                    for (gen_name, gen_ty) in self.mono.with.clone() {
                        let mono = Monomorphization::from_resolved(gen_ty);
                        let mono_name = mono.encode_ref();
                        base = base.replace(format!("${gen_name}").as_str(), mono_name.resolve());
                    }

                    let base_ref = self.mono.encode_ref();

                    lines.push(format!("pub type {base_ref} = {base};"));
                    // already builtin
                    //lines.push(format!("pub type {name} = {b};"));
                } else {
                    lines.push(format!("#[derive(Default, Clone, Debug)]"));
                    lines.push(format!("pub struct {name} {{"));

                    //lines.push("\tuint32_t _luma_private__refcount,".to_owned());

                    for (name, dynf) in transponster.dynamic_fields.borrow().iter() {
                        if let Some(res) = unsafe { dynf.committed_type.clone().wait().await } {
                            let mono = Monomorphization::from_resolved(res);

                            let n = mono.encode_ref();

                            lines.push(format!("\t{name}: {n},"));
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

                        let mt = mono.encode_ref();

                        lines.push(format!("\t{rfn}: {mt},"));
                    }

                    // do static fields

                    //for (name, statf) in self.mono.of.resolve().

                    lines.push(format!("}}"));

                    /*lines.push(format!("\n{name}* default_{name}() {{"));
                    lines.push(format!("}}"));*/

                    //lines.push("struct s)
                }
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
//const OUTPUT_TYPE: OutputType = OutputType::FullInf();

lazy_static! {
    pub static ref OUTPUT_TYPE_ONCE: OnceCell<OutputType> = OnceCell::new();
}

pub fn output_type() -> OutputType {
    OUTPUT_TYPE_ONCE.get().copied().unwrap()
}

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
        let attrs = self.of.resolve().get_struct_attrs();
        /*let name = match attrs.is_builtin {
            Some(v) => v.as_plain_type(&self.local_submap()),
            None => self.encode_name(),
        };*/

        let name = self.encode_name();

        match attrs.is_ref {
            true => format!("FastRefHandle<{name}>").intern(),
            false => name,
        }
    }

    pub fn flattened_contexts(&self) -> Vec<CtxID> {
        fn inner(ar: Vec<ResolvedType>) -> Vec<CtxID> {
            let mut c = Vec::new();

            for t in ar {
                c.push(t.node);

                c.append(&mut inner(t.generics));
            }

            c
        }

        let mut v = vec![self.of];

        v.append(&mut inner(
            self.with.clone().into_iter().map(|e| e.1).collect_vec(),
        ));
        v
    }

    pub fn local_submap(&self) -> HashMap<IStr, IStr> {
        let gen_sub_map = self
            .with
            .clone()
            .into_iter()
            .map(|(gn, gt)| (gn, Monomorphization::from_resolved(gt).encode_name()))
            .collect();

        gen_sub_map
    }

    pub fn to_resolved(&self) -> ResolvedType {
        ResolvedType {
            node: self.of,
            generics: self.with.clone().into_iter().map(|(_, t)| t).collect_vec(),
        }
    }

    pub fn has_generics(&self) -> bool {
        fn s(r: &ResolvedType) -> bool {
            if let NodeUnion::Generic(g) = r.node.resolve().inner {
                println!("found a mono who is generic, gen is {g}");
                return true;
            } else {
                for rt in r.generics.iter() {
                    if s(rt) {
                        return true;
                    }
                }

                false
            }
        }

        s(&self.to_resolved())
    }

    pub fn encode_name(&self) -> IStr {
        let tr = self.of.resolve().canonical_typeref().resolve().unwrap();

        if !self.has_generics() {
            match &self.of.resolve().inner {
                NodeUnion::Generic(n) => todo!("Why are we encoding a generic's name? Name: {n}"),
                NodeUnion::Type(_) => Postal::instance().send_and_forget(
                    Destination::scribe(self.of),
                    Content::Scribe(Note::MonoType { ty: self.clone() }),
                ),
                NodeUnion::Function(_, _) => Postal::instance().send_and_forget(
                    Destination::scribe(self.of),
                    Content::Scribe(Note::MonoFunc { func: self.clone() }),
                ),
                _ => unreachable!(),
            }
        }

        let base_str = tr.as_c_id().resolve();

        let mut hasher = DefaultHasher::new();
        self.with.hash(&mut hasher);
        let hashed_tail = hasher.finish();

        let gen_summary = self
            .with
            .iter()
            .map(|(gn, gt)| {
                format!(
                    "g{gn}_{}",
                    gt.node
                        .resolve()
                        .canonical_typeref()
                        .resolve()
                        .unwrap()
                        .as_plain_type(&self.local_submap())
                )
            })
            .join("_");

        let s = if self.with.is_empty() {
            format!("{base_str}")
        } else {
            format!("{base_str}_{gen_summary}_{hashed_tail}")
        };

        let s: String = s
            .chars()
            .map(|c| match c {
                '*' => "mul".to_owned(),
                '/' => "div".to_owned(),
                '+' => "add".to_owned(),
                '-' => "sub".to_owned(),
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

        s.intern()
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
