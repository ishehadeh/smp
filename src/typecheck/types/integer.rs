#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IntegerType {
    // TODO: allow for integers larger than a single pointer
    // FIXME: sync these field names with the AST integer range names
    pub lo: i32,
    pub hi: i32,
}

impl IntegerType {
    pub fn new(lo: i32, hi: i32) -> IntegerType {
        IntegerType { lo, hi }
    }

    pub fn intersect(&self, other: &IntegerType) -> Option<IntegerType> {
        let lo_max = self.lo.max(other.lo);
        let hi_min = self.hi.min(other.hi);
        if lo_max <= hi_min {
            Some(IntegerType::new(lo_max, hi_min))
        } else {
            None
        }
    }

    pub fn is_subset(&self, other: &IntegerType) -> bool {
        self.lo >= other.lo && self.hi <= other.hi
    }
}
