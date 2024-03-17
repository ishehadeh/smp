use std::{collections::BTreeMap, hash::Hash, marker::PhantomData, sync::atomic::AtomicUsize};

#[derive(Debug)]
pub struct Id<Marker> {
    element_id: usize,
    container_id: usize,
    marker: PhantomData<Marker>,
}

impl<Marker> Clone for Id<Marker> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<Marker> Hash for Id<Marker> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.element_id.hash(state);
        self.container_id.hash(state);
    }
}

impl<Marker> Copy for Id<Marker> {}

impl<Marker> PartialEq for Id<Marker> {
    fn eq(&self, other: &Self) -> bool {
        self.element_id == other.element_id && self.container_id == other.container_id
    }
}

impl<Marker> Eq for Id<Marker> {}

impl<Marker> PartialOrd for Id<Marker> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<Marker> Ord for Id<Marker> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.element_id.cmp(&other.element_id)
    }
}

#[derive(Clone, Debug)]
pub struct IdVec<T, Marker = T> {
    elements: BTreeMap<Id<Marker>, T>,
    next_element_id: usize,
    container_id: usize,
}

impl<T, Marker> IdVec<T, Marker> {
    pub fn new() -> IdVec<T, Marker> {
        static VEC_ID_COUNTER: AtomicUsize = AtomicUsize::new(0);

        IdVec {
            elements: BTreeMap::new(),
            next_element_id: 0,
            container_id: VEC_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
        }
    }

    fn next_id(&mut self) -> Id<Marker> {
        let id = Id {
            element_id: self.next_element_id,
            container_id: self.container_id,
            marker: PhantomData,
        };
        self.next_element_id += 1;
        id
    }

    pub fn push(&mut self, val: T) -> Id<Marker> {
        let id = self.next_id();
        self.elements.insert(id, val);
        id
    }

    pub fn iter(&self) -> impl Iterator<Item = (Id<Marker>, &T)> {
        self.elements.iter().map(|(id, x)| (*id, x))
    }

    pub fn get(&self, id: Id<Marker>) -> &T {
        assert_eq!(id.container_id, self.container_id);
        self.elements.get(&id).unwrap()
    }

    pub fn get_mut(&mut self, id: Id<Marker>) -> &mut T {
        assert_eq!(id.container_id, self.container_id);
        self.elements.get_mut(&id).unwrap()
    }
}

impl<T, Marker> Default for IdVec<T, Marker> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    use super::IdVec;

    #[test]
    fn idvec_push_get() {
        let mut vec: IdVec<usize> = IdVec::new();
        let a = vec.push(1);
        let b = vec.push(2);
        let c = vec.push(3);
        let d = vec.push(4);

        assert_eq!(*vec.get(a), 1);
        assert_eq!(*vec.get(b), 2);
        assert_eq!(*vec.get(c), 3);
        assert_eq!(*vec.get(d), 4);
    }
}
