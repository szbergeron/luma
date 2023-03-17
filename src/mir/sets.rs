use std::{collections::HashMap, fmt::Debug};

pub struct Unifier<K, T> {
    //_p: PhantomData<(T, K)>,
    elements: HashMap<K, Entry<K, T>>,
}

impl<T: Clone, K: Eq + PartialEq + std::hash::Hash + Copy + Debug> Unifier<K, T> {
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

            (ar.inner, br.inner) = match (&mut ar.inner, &mut br.inner) {
                (EntryInner::Root(va), EntryInner::Root(vb)) => {
                    tracing::info!("they're both roots, need to merge val");
                    let new_v = with_values(va.clone(), vb.clone())?;
                    ar.ref_from.push(root_b); // we're changing A to be authority
                    //ar.inner = EntryInner::Root(with_values(va, vb));
                    (EntryInner::Root(new_v), EntryInner::Refers(root_a))
                }
                (EntryInner::Free(), bi) => {
                    tracing::info!("a was free, so do a noop merge");
                    br.ref_from.push(root_a);
                    //ar.inner = EntryInner::Refers(br.keyed);
                    (EntryInner::Refers(br.keyed), bi.clone())
                }
                (ai, EntryInner::Free()) => {
                    tracing::info!("b was free, so do a noop merge");
                    ar.ref_from.push(root_b);
                    //br.inner = EntryInner::Refers(ar.keyed);
                    (ai.clone(), EntryInner::Refers(ar.keyed))
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

#[derive(Clone)]
pub enum EntryInner<K, T> {
    Root(T),
    Refers(K),
    Free(),
}

#[derive(Clone)]
pub struct Entry<K, T> {
    ref_from: smallvec::SmallVec<[K; 3]>,

    keyed: K,

    inner: EntryInner<K, T>,
}
