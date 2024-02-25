use std::{collections::BTreeMap, sync::atomic::AtomicUsize};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id {
    element_id: usize,
    container_id: usize,
}

#[derive(Clone)]
pub struct IdVec<T> {
    elements: BTreeMap<Id, T>,
    next_element_id: usize,
    container_id: usize,
}

impl<T> IdVec<T> {
    pub fn new() -> IdVec<T> {
        static VEC_ID_COUNTER: AtomicUsize = AtomicUsize::new(0);

        IdVec {
            elements: BTreeMap::new(),
            next_element_id: 0,
            container_id: VEC_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
        }
    }

    fn next_id(&mut self) -> Id {
        let id = Id {
            element_id: self.next_element_id,
            container_id: self.container_id,
        };
        self.next_element_id += 1;
        id
    }

    pub fn push(&mut self, val: T) -> Id {
        let id = self.next_id();
        self.elements.insert(id, val);
        id
    }

    pub fn get(&mut self, id: Id) -> &T {
        assert_eq!(id.container_id, self.container_id);
        self.elements.get(&id).unwrap()
    }
}

impl<T> Default for IdVec<T> {
    fn default() -> Self {
        Self::new()
    }
}
