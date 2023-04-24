#![feature(min_specialization)]
#![allow(
    unused_mut,
    unused_variables,
    //redundant_semicolons,
    unused_imports,
    unused_assignments,
    non_snake_case,
    dead_code,
    non_camel_case_types,
    //unused_parens,
    //unused_braces
)]

//#[global_allocator]
//static GLOBAL: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

use std::cell::RefCell;
use std::time::Instant;

use crate::std2::Value;

use crate::gen::*;

fn main() {
    let before = Instant::now();
    //crate::gen::root_std_primitive_compose_13646096770106105413();
    //crate::gen::root_std_primitive_p_13646096770106105413();
    //crate::gen::root_std_primitive_fibonacci_13646096770106105413(Value::)
    //let fr = crate::gen::root_std_primitive_fib_test_13646096770106105413();
    _luma_main();

    let after = Instant::now();

    //let ar = root_std_primitive_ackermann_test_13646096770106105413();

    //println!("Fib returns {fr:?}");

    let dur = after - before;
    println!(
        "Took {:0>1}s.{:0>3}.{}",
        dur.as_secs(),
        dur.subsec_millis(),
        dur.subsec_micros()
    );

    //println!("Ack returns {ar:?}");
}

struct S {
    a: i64,
    b: i64,
}

impl S {
    pub fn sg(&mut self, v: i64) -> i64 {
        self.a = v;
        v
    }
}

fn test() {
    let mut b = Box::new(S { a: 10, b: 20 });

    b.a = b.b;

    b.b = b.sg(5) + b.sg(7);

    let mut r = RefCell::new(S { a: 30, b: 40 });

    r.borrow_mut().a = r.borrow().b;
}

pub mod gen;
pub mod std2;
