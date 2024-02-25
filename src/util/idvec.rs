use const_random::const_random;
use std::{
    collections::BTreeMap,
    sync::atomic::{self, AtomicUsize},
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id<const ContainerId: u64> {
    element_id: usize,
}

const fn next_id() -> u64 {
    let id = const_random!(u64);
    id
}

#[derive(Clone)]
pub struct IdVec<T, const ContainerId: u64 = { next_id() }> {
    elements: BTreeMap<Id<ContainerId>, T>,
    next_element_id: usize,
}

impl<T, const ContainerId: u64> IdVec<T, ContainerId> {
    pub fn new() -> IdVec<T, ContainerId> {
        static VEC_ID_COUNTER: AtomicUsize = AtomicUsize::new(0);
        let vec_id = VEC_ID_COUNTER.fetch_add(1, atomic::Ordering::SeqCst);

        IdVec {
            elements: BTreeMap::new(),
            next_element_id: 0,
        }
    }
}

impl<T, const ContainerId: u64> Default for IdVec<T, ContainerId> {
    fn default() -> Self {
        Self {
            elements: Default::default(),
            next_element_id: Default::default(),
        }
    }
}
