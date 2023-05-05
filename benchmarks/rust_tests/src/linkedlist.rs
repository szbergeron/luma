use std::{time::{Duration, Instant}, collections::LinkedList};

use crate::rand_i64;

pub fn linkedlist_entry() {
    let total_iters = 100;

    let mut contained = 0;

    for v in 0..total_iters {
        let iters = 100000;

        let (icontained, dur) = do_inner(iters);

        println!("{}", dur.as_secs_f64());

        contained += icontained;
    }

    println!("precontained: {contained}");
}

fn do_inner(iters: i32) -> (i32, Duration) {
    let mut contained = 0;
    let mut rands = vec![];

    for i in 0..iters {
        rands.push(rand_i64());
    }

    let mut ll = LinkedList::new();

    let start = Instant::now();

    for r in rands.iter().rev() {
        if !ll.contains(r) {
            contained += 1;
            ll.push_back(*r);
        }
    }

    let end = Instant::now();

    let dur = end - start;

    (contained, dur)
}
