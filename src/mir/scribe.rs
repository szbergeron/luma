use std::{collections::hash_map::DefaultHasher, hash::{Hash, Hasher}, sync::Mutex};

use either::Either;
use futures::future::join_all;
use itertools::Itertools;

use crate::{ast::tree::CtxID, helper::{interner::{IStr, SpurHelper, Internable}, SwapWith}};

use super::{quark::{Quark, ResolvedType}, transponster::Transponster, expressions::ExpressionID};

pub struct Scribe<'a> {
    using: Either<&'static Quark, &'a Transponster>,

    mono: Monomorphization,
}

impl<'a> Scribe<'a> {
    pub fn new(using: Either<&'static Quark, &'a Transponster>, mono: Monomorphization) -> Self {
        Self { using, mono }
    }

    pub async fn codegen(&mut self) {
        match self.using {
            Either::Left(q) => self.codegen_fn(q).await,
            Either::Right(t) => self.codegen_ty(t).await,
        }
    }

    async fn codegen_fn(&mut self, quark: &'static Quark) {
        println!("Encoding a function");

        let mut within = Vec::new();

        //let ent = quark.acting_on.borrow().get(entry_fn_id);

        let ret_tid = quark.returns.get().copied().unwrap();

        let params_tid = quark.params.get().cloned().unwrap();

        let ret_mono = Monomorphization::from_resolved(quark.resolved_type_of(ret_tid).await);
        let ret = format!("{}*", ret_mono.encode_name());

        let params_resolved = join_all(params_tid.into_iter().map(|(name, tid)| { async move {
            println!("Resolving param {name}");
            let mono = Monomorphization::from_resolved(quark.resolved_type_of(tid).await);

            format!("\n\t{}* {}", mono.encode_name(), name)
        }
        })).await.join(",");


        let fname = self.mono.encode_name();

        within.push(format!("{ret} {fname} ({params_resolved}) {{"));

        let entry_fn_id = quark.entry_id.get().copied().unwrap();

        self.codegen_fn_rec(quark, entry_fn_id, &mut within).await;

        within.push(format!("}}"));

        LINES.lock().unwrap().append(&mut within);


    }

    async fn codegen_fn_rec(&mut self, quark: &Quark, at_id: ExpressionID, within: &mut Vec<String>) {
    }

    async fn codegen_ty(&mut self, transponster: &Transponster) {
        println!("Encoding a type");

        let name = self.mono.encode_name();

        let mut lines: Vec<String> = Vec::new();

        lines.push(format!("struct {name} {{"));

        match OUTPUT_TYPE {
            OutputType::FullInf() => {

                for (name, dinf) in transponster.dynamic_fields.borrow().iter() {
                    if let Some(res) = unsafe { dinf.committed_type.clone().wait().await } {

                        let mono = Monomorphization::from_resolved(res);

                        let n = mono.encode_name();

                        lines.push(format!("\t{n}* {name},"));
                    }
                }



                //lines.push("struct s)
            }
            _ => todo!()
        }

        lines.push(format!("}}"));

        lines.push(format!("{name}* new_{name}() {{"));
        lines.push(format!("}}"));

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

const OUTPUT_TYPE: OutputType = OutputType::FullInf();

enum OutputType {
    FullInf(),
    AssumeTypeSafe(),
    AssumeTypeUnsafe(),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Monomorphization {
    of: CtxID,

    with: Vec<ResolvedType>,
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

        hashed_tail.intern()
    }

    pub fn from_resolved(r: ResolvedType) -> Self {
        Self { of: r.node, with: r.generics }
    }
}
