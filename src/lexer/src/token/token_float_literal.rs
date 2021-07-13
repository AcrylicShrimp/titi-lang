#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct TokenFloatLiteral {
    suffix_len: usize,
}

impl TokenFloatLiteral {
    pub fn new(suffix_len: usize) -> Self {
        Self { suffix_len }
    }

    pub fn suffix_len(&self) -> usize {
        self.suffix_len
    }
}
