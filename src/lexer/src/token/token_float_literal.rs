pub struct TokenFloatLiteral {
    suffix_len: bool,
}

impl TokenFloatLiteral {
    pub fn new(suffix_len: bool) -> Self {
        Self { suffix_len }
    }

    pub fn suffix_len(&self) -> bool {
        &self.suffix_len
    }
}
