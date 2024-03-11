#![feature(
    new_uninit,
    iter_intersperse,
    vec_into_raw_parts,
    layout_for_ptr,
    ptr_metadata,
    inherent_associated_types,
    try_trait_v2,
    try_trait_v2_residual,
    min_specialization,
    type_changing_struct_update,
    let_chains,
    more_qualified_paths,
    if_let_guard,
    const_heap,
    const_mut_refs,
    never_type,
    backtrace_frames,
    unboxed_closures,
    fn_traits,
    array_chunks,
    assert_matches,
    exclusive_range_pattern,
    generic_const_exprs,
    const_trait_impl,
    box_patterns,
    map_many_mut,
    async_iterator,
    closure_track_caller
)]
#![allow(irrefutable_let_patterns)]
#![allow(dead_code)]
#![allow(incomplete_features)]
#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(unreachable_code)]

#[allow(unused_imports)]
#[macro_use]
extern crate static_assertions;

#[macro_use]
extern crate lazy_static;

use std::env;

use tracing::{dispatcher, span, Dispatch, Subscriber};

pub mod ast;
pub mod avec;
pub mod compile;
pub mod cst;
pub mod helper;
pub mod lex;
pub mod lir;
pub mod mir;
pub mod errors;
pub mod parse;
pub mod monitor;

fn main() {
    //console_subscriber::init();
    //tracing_subscriber::EnvFilter::builder().
    let ds = tracing_subscriber::fmt().finish();

    let ms = MySub {
        intsub: Box::new(ds),
    };

    let limit = true;
    if limit {
        dispatcher::set_global_default(Dispatch::new(ms)).unwrap();
    } else {
        //tracing::

        tracing_subscriber::fmt::init();
    }

    let args: Vec<String> = env::args().collect();
    println!("Args: {:?}", args);
    let sliced: Vec<&str> = args.iter().map(|s| &s[..]).collect();

    let sliced: &[&str] = &sliced[1..];
    println!("stripped args: {:?}", sliced);

    compile::launch(sliced);
}

unsafe impl Sync for MySub {}
unsafe impl Send for MySub {}

pub struct MySub {
    intsub: Box<dyn Subscriber>,
}

impl Subscriber for MySub {
    fn enabled(&self, metadata: &tracing::Metadata<'_>) -> bool {
        let allow_all = true;
        let allow_any = false;

        metadata.file().is_some_and(|f| {
            (allow_all
                || (true && f.contains("quark"))
                || (false && f.contains("transponster"))
                || (false && f.contains("sets"))
                || (true && f.contains("instance"))
                || (false && f.contains("executor"))
                || (false && f.contains("per_module")))
                && allow_any
        }) //|| metadata.level() > &Level::WARN
    }

    fn new_span(&self, span: &span::Attributes<'_>) -> span::Id {
        self.intsub.new_span(span)
    }

    fn record(&self, span: &span::Id, values: &span::Record<'_>) {
        self.intsub.record(span, values)
    }

    fn record_follows_from(&self, span: &span::Id, follows: &span::Id) {
        self.intsub.record_follows_from(span, follows)
    }

    fn event(&self, event: &tracing::Event<'_>) {
        self.intsub.event(event)
    }

    fn enter(&self, span: &span::Id) {
        self.intsub.enter(span)
    }

    fn exit(&self, span: &span::Id) {
        self.intsub.exit(span)
    }
}
