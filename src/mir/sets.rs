use std::{
    collections::{HashMap, HashSet},
    marker::PhantomData,
};

pub struct DisjointSet<T, K> {
    //_p: PhantomData<(T, K)>,
    elements: HashMap<K, Entry<T, K>>,
}

impl<T, K: Eq + PartialEq + std::hash::Hash + Copy> DisjointSet<T, K> {
    pub fn root_for(&self, mut elem_id: K) -> K {
        loop {
            let v = self.elements.get(&elem_id).unwrap();

            match v {
                Entry::Root(..) => break elem_id,
                Entry::Refers(_sk, other) => {
                    elem_id = *other;
                    continue;
                }
            }
        }
    }

    pub fn unify(
        &mut self,
        a: K,
        b: K,
        with_values: &dyn FnOnce(Option<T>, Option<T>) -> Option<T>,
    ) {
        let root_a = self.root_for(a);
        let root_b = self.root_for(b);
        if root_a == root_b {
            // already unified
        } else {
            let [ar, br] = self
                .elements
                .get_many_mut([&root_a, &root_b])
                .expect("should have been allowed");

            match (ar, br) {
                (Entry::Root(ta, rka, ra), Entry::Root(tb, rkb, rb)) => {
                },
                _ => unreachable!(),
            }
        }
    }

    pub fn has_v(&self, k: K) -> bool {
        match self.elements.get(&self.root_for(k)).unwrap() {
            Entry::Root(Some(_), ..) => true,
            Entry::Root(None, ..) => false,
            _ => unreachable!(),
        }
    }

    pub fn peers_of(&self, of: K) -> Vec<K> {
        todo!()
    }
}

pub enum Entry<T, K> {
    Root(K, Option<T>, HashSet<K>),
    Refers(K, K),
}

pub struct Entry<T, K> {
    //
}
