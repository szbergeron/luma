use std::{collections::HashMap, fmt::Debug};

#[derive(Debug)]
pub struct Unifier<K: std::fmt::Debug, T: std::fmt::Debug> {
    //_p: PhantomData<(T, K)>,
    elements: HashMap<K, Entry<K, T>>,
}

impl<T: Debug, K: Eq + PartialEq + std::hash::Hash + Copy + Debug> Unifier<K, T> {
    pub fn root_for(&self, mut elem_id: K) -> K {
        loop {
            let v = self.elements.get(&elem_id).unwrap();

            match v.inner {
                EntryInner::Root(..) | EntryInner::Free() => break elem_id,
                EntryInner::Refers(other) => {
                    elem_id = other;
                    continue;
                }
            }
        }
    }

    pub fn unify<F, E>(
        &mut self,
        a: K,
        b: K,
        with_values: F,
    ) -> Result<(), E> where F: FnOnce(T, T) -> Result<T, E> {
        let root_a = self.root_for(a);
        let root_b = self.root_for(b);
        if root_a == root_b {
            // already unified
            Ok(())
        } else {
            let [ar, br] = self
                .elements
                .get_many_mut([&root_a, &root_b])
                .expect("should have been allowed");

            tracing::info!("unifies {root_a:?} and {root_b:?} because {a:?} and {b:?} were unified");

            let ta = std::mem::take(&mut ar.inner);
            let tb = std::mem::take(&mut br.inner);

            match (ta, tb) {
                (EntryInner::Root(va), EntryInner::Root(vb)) => {
                    tracing::info!("they're both roots, need to merge val");
                    let new_v = with_values(va, vb)?;
                    ar.ref_from.push(root_b); // we're changing A to be authority
                    ar.inner = EntryInner::Root(new_v);
                    br.inner = EntryInner::Refers(root_a);
                }
                (EntryInner::Free(), bi) => {
                    tracing::info!("a was free, so do a noop merge");
                    br.ref_from.push(root_a);
                    ar.inner = EntryInner::Refers(br.keyed);
                    br.inner = bi;
                }
                (ai, EntryInner::Free()) => {
                    tracing::info!("b was free, so do a noop merge");
                    ar.ref_from.push(root_b);
                    br.inner = EntryInner::Refers(ar.keyed);
                    ar.inner = ai;
                }
                _ => unreachable!(),
            };

            Ok(())
        }
    }

    pub fn add_k(&mut self, k: K) {
        let prior = self.elements.insert(k, Entry { ref_from: smallvec::SmallVec::new(), keyed: k, inner: EntryInner::Free() });
        assert!(prior.is_none(), "tried to overwrite a k");
    }

    pub fn add_kv(&mut self, k: K, v: T) {
        let prior = self.elements.insert(k, Entry { ref_from: smallvec::SmallVec::new(), keyed: k, inner: EntryInner::Root(v) });
        assert!(prior.is_none(), "tried to overwrite a k");
    }

    pub fn has_v(&self, k: K) -> bool {
        match self.elements.get(&self.root_for(k)).unwrap().inner {
            EntryInner::Root(_) => true,
            EntryInner::Free() => false,
            _ => unreachable!(),
        }
    }

    /*pub fn v_for_or_insert_with<F>(&mut self, k: K, v: F) -> &T where F: FnOnce() -> T {
        if let Some(v) = self.v_for(k) {
            v
        } else {
            self.
        }
    }*/

    pub fn v_for(&self, k: K) -> Option<&T> {
        let root = self.root_for(k);
        if let Some(EntryInner::Root(v)) = self.elements.get(&k).map(|e| &e.inner) {
            Some(v)
        } else {
            None
        }
    }

    pub fn peers_of(&self, of: K) -> Vec<K> {
        let root_of = self.root_for(of);

        let mut peers = Vec::new();

        self.rec_children_of(root_of, &mut peers);

        peers
    }

    fn rec_children_of(&self, top: K, put_in: &mut Vec<K>) {
        put_in.push(top);

        for val in self.elements.get(&top).unwrap().ref_from.iter() {
            self.rec_children_of(*val, put_in);
        }
    }

    pub fn new() -> Self {
        Self { elements: HashMap::new() }
    }
}

#[derive(Clone, Debug)]
pub enum EntryInner<K: Debug, T: Debug> {
    Root(T),
    Refers(K),
    
    Free(),
}

impl<K: Debug, T: Debug> Default for EntryInner<K, T> {
    fn default() -> Self {
        EntryInner::Free()
    }
}

#[derive(Clone, Debug)]
pub struct Entry<K: Debug, T: Debug> {
    ref_from: smallvec::SmallVec<[K; 3]>,

    keyed: K,

    inner: EntryInner<K, T>,
}
