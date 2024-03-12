use std::collections::BTreeMap;

// NOTE: usingbtree here because preserving ordering is essential for tests, should probably rename this struct, or offer another option for release at some point.
#[derive(Debug, Clone, Default)]
pub struct BidiHashMap<L, R>
where
    L: Copy + Eq + Ord,
    R: Copy + Eq + Ord,
{
    pub forward: BTreeMap<L, R>,
    pub reverse: BTreeMap<R, L>,
}

impl<L, R> BidiHashMap<L, R>
where
    L: Copy + Eq + Ord,
    R: Copy + Eq + Ord,
{
    pub fn new() -> BidiHashMap<L, R> {
        BidiHashMap {
            forward: BTreeMap::new(),
            reverse: BTreeMap::new(),
        }
    }

    pub fn forward(&self) -> &BTreeMap<L, R> {
        &self.forward
    }

    pub fn reverse(&self) -> &BTreeMap<R, L> {
        &self.reverse
    }

    pub fn insert(&mut self, l: L, r: R) {
        self.forward.insert(l, r);
        self.reverse.insert(r, l);
    }

    pub fn remove_by_left(&mut self, l: L) -> Option<(L, R)> {
        let r = self.forward.remove(&l)?;
        self.reverse.remove(&r);
        Some((l, r))
    }

    pub fn remove_by_right(&mut self, r: R) -> Option<(L, R)> {
        let l = self.reverse.remove(&r)?;
        self.forward.remove(&l);
        Some((l, r))
    }

    pub fn len(&self) -> usize {
        assert_eq!(self.forward.len(), self.reverse.len());

        self.forward.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
