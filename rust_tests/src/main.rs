use rand::{rngs::SmallRng, SeedableRng, Rng};

pub mod two_body;
pub mod linkedlist;
pub mod trie;

fn main() {
    crate::two_body::entry();
    //crate::linkedlist::linkedlist_entry();
    //trie::trie_entry();
}


pub fn rand_i64() -> i64 {
    let mut r = rand::thread_rng();
    let mut sng = SmallRng::from_rng(&mut r).unwrap();
    sng.gen()
}

pub fn rand_f64() -> f64 {
    let mut r = rand::thread_rng();
    let mut sng = SmallRng::from_rng(&mut r).unwrap();
    sng.gen()
}

