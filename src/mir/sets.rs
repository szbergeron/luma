use std::{collections::HashMap, fmt::Debug};

use itertools::Itertools;

#[derive(Debug)]
pub struct Unifier<K: std::fmt::Debug, T: std::fmt::Debug> {
    //_p: PhantomData<(T, K)>,
    elements: HashMap<K, Entry<K, T>>,
}

impl<T: Debug, K: Eq + PartialEq + std::hash::Hash + Copy + Debug> Unifier<K, T> {
    pub fn root_for(&self, mut elem_id: K) -> K {
        debug_assert!(self.elements.contains_key(&elem_id));
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

    #[track_caller]
    pub fn unify<F, P, E>(&mut self, a: K, b: K, with_values: F) -> Result<Option<P>, E>
    where
        F: FnOnce(T, T) -> Result<(T, P), E>,
    {
        if !self.elements.contains_key(&a) {
            self.add_k(a);
        }

        if !self.elements.contains_key(&b) {
            self.add_k(b);
        }

        debug_assert!(self.elements.contains_key(&a));
        debug_assert!(self.elements.contains_key(&b));

        let root_a = self.root_for(a);
        let root_b = self.root_for(b);

        if root_a == root_b {
            // already unified
            Ok(None)
        } else {
            debug_assert!(!self.children_of(root_a).into_iter().contains(&root_b));
            debug_assert!(!self.children_of(root_b).into_iter().contains(&root_a));

            let [ar, br] = self
                .elements
                .get_many_mut([&root_a, &root_b])
                .expect("should have been allowed");

            tracing::info!(
                "unifies {root_a:?} and {root_b:?} because {a:?} and {b:?} were unified"
            );

            let ta = std::mem::take(&mut ar.inner);
            let tb = std::mem::take(&mut br.inner);

            let r = match (ta, tb) {
                (EntryInner::Root(va), EntryInner::Root(vb)) => {
                    tracing::info!("they're both roots, need to merge val");
                    let (t, p) = with_values(va, vb)?;
                    ar.ref_from.push(root_b); // we're changing A to be authority
                    ar.inner = EntryInner::Root(t);
                    br.inner = EntryInner::Refers(root_a);

                    Some(p)
                }
                (EntryInner::Free(), bi) => {
                    tracing::info!("a was free, so do a noop merge");
                    br.ref_from.push(root_a);
                    ar.inner = EntryInner::Refers(br.keyed);
                    br.inner = bi;
                    None
                }
                (ai, EntryInner::Free()) => {
                    tracing::info!("b was free, so do a noop merge");
                    ar.ref_from.push(root_b);
                    br.inner = EntryInner::Refers(ar.keyed);
                    ar.inner = ai;
                    None
                }
                _ => unreachable!(),
            };

            Ok(r)
        }
    }

    pub fn add_k(&mut self, k: K) {
        let prior = self.elements.insert(
            k,
            Entry {
                ref_from: smallvec::SmallVec::new(),
                keyed: k,
                inner: EntryInner::Free(),
            },
        );
        assert!(prior.is_none(), "tried to overwrite a k");
    }

    pub fn add_kv(&mut self, k: K, v: T) {
        let prior = self.elements.insert(
            k,
            Entry {
                ref_from: smallvec::SmallVec::new(),
                keyed: k,
                inner: EntryInner::Root(v),
            },
        );
        assert!(prior.is_none(), "tried to overwrite a k");
    }

    pub fn has_v(&self, k: K) -> bool {
        debug_assert!(self.elements.contains_key(&k));
        match self.elements.get(&self.root_for(k)).unwrap().inner {
            EntryInner::Root(_) => true,
            EntryInner::Free() => false,
            _ => unreachable!(),
        }
    }

    pub fn gather_free(&self) -> Vec<K> {
        let mut free = Vec::new();

        for (k, e) in self.elements.iter() {
            match &e.inner {
                EntryInner::Root(_) => (),
                EntryInner::Refers(_) => (),
                EntryInner::Free() => free.push(*k),
            }
        }

        free
    }

    pub fn map_roots<F, R>(&self, mut f: F) -> Vec<R>
    where
        F: FnMut(&K, &T) -> R,
    {
        let mut out = Vec::new();
        for (k, e) in self.elements.iter() {
            match &e.inner {
                EntryInner::Root(r) => {
                    out.push(f(&e.keyed, &r));
                }
                _ => {
                    // do nothing
                }
            }
        }
        out
    }

    pub fn reduce_roots<F, R>(&self, f: F) -> Vec<R>
    where
        F: FnMut(&K, &T) -> Option<R>,
    {
        let iter = self.map_roots(f).into_iter().filter_map(|e| e);

        iter.collect_vec()
    }

    /*pub fn v_for_or_insert_with<F>(&mut self, k: K, v: F) -> &T where F: FnOnce() -> T {
        if let Some(v) = self.v_for(k) {
            v
        } else {
            self.
        }
    }*/

    pub fn v_for(&self, k: K) -> Option<&T> {
        debug_assert!(self.elements.contains_key(&k));
        let root = self.root_for(k);
        if let Some(EntryInner::Root(v)) = self.elements.get(&k).map(|e| &e.inner) {
            Some(v)
        } else {
            None
        }
    }

    pub fn peers_of(&self, of: K) -> Vec<K> {
        debug_assert!(self.elements.contains_key(&of));
        let root_of = self.root_for(of);

        self.children_of(root_of)
    }

    fn children_of(&self, of: K) -> Vec<K> {
        let mut peers = Vec::new();

        self.rec_children_of(of, &mut peers);

        peers
    }

    fn rec_children_of(&self, top: K, put_in: &mut Vec<K>) {
        put_in.push(top);

        for val in self.elements.get(&top).unwrap().ref_from.iter() {
            self.rec_children_of(*val, put_in);
        }
    }

    pub fn new() -> Self {
        Self {
            elements: HashMap::new(),
        }
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
