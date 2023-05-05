use std::{time::Instant};
use hashbrown::HashMap;

use crate::rand_i64;

#[derive(Default)]
pub struct TrieNode {
    /// Uses a standard map for fairness with
    /// the other tested languages,
    /// could opt to use a plain array
    /// for efficiency later
    children: HashMap<u8, TrieNode>,
    //is_final: bool,
    contains_str: Option<String>,
}

impl TrieNode {
    pub fn contains(&self, remainder: &[u8]) -> bool {
        match remainder {
            [] => self.contains_str.is_some(),
            [a, rest @ ..] => {
                self.children.get(a).map(|c| c.contains(rest)).unwrap_or(false)
            }
        }
    }

    pub fn insert(&mut self, remainder: &[u8], is: String) {
        match remainder {
            [] => self.contains_str = Some(is.clone()),
            [a, rest @ ..] => {
                //self.children.get_mut(a).map(|c| c.insert(rest)).unwrap_or(false)
                self.children.entry(*a).or_default().insert(rest, is);
            }
        }
    }
}

fn random_string(min: i64, max: i64) -> String {
    let len = (rand_i64() % (max - min) + min);

    let s = rand_of_len_fast(len);

    s
}

pub fn rand_of_len_fast(len: i64) -> String {
    let mut base = String::new();
    for i in 0..len {
        let c = ((rand_i64() % 26) + 97) as i8;
        //let c = c.abs; // make sure 0th bit is clear
        let c = c as u8 & 0x7F;

        base.push(c as char);
    }

    base
}

pub fn trie_entry() {


    let samples = 1000000;

    let total_iters = 100;
    let mut contained = 0;

    //for(let o = 0; o < total_iters; o = o + 1) {
    for o in 0..total_iters {

        let mut rand_strings = vec![];
        //for (let i = 0; i < samples; i = i + 1) {
        for i in 0..samples {
            //let strlen = i64::rand().abs().modulo(6) + 3; // range 5 through 10

            /*for (let j = 0; j < strlen; j = j + 1) {
            }*/
            //let s = String::rand_of_len(strlen);
            let s = random_string(3, 6);

            rand_strings.push(s);
        }

        let before = Instant::now();


        let mut t = TrieNode::default();

        //for (let i = 0; i < rand_strings.len(); i = i + 1) {
        for s in rand_strings.into_iter() {
            if t.contains(s.as_bytes()) {
                contained = contained + 1;
            } else {
                //t.insert(rand_strings.get(i));
                t.insert(s.as_bytes(), s.clone());
            }
        }


        let after = Instant::now();
        let dur = after - before;

        println!("{}", dur.as_secs_f64());
    }

    println!("contains of: {contained}");
}
