#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SourceSpan {
    pub start: usize,
    pub end: usize,
}

impl SourceSpan {
    pub fn new(start: usize, end: usize) -> SourceSpan {
        SourceSpan { start, end }
    }
}

pub trait Spanned {
    fn span(&self) -> &SourceSpan;
}

impl From<(usize, usize)> for SourceSpan {
    fn from((start, end): (usize, usize)) -> Self {
        Self { start, end }
    }
}
