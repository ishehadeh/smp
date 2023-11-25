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

impl From<(usize, usize)> for SourceSpan {
    fn from((start, end): (usize, usize)) -> Self {
        Self { start, end }
    }
}
