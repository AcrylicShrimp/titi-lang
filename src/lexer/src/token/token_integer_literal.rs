use crate::TokenIntegerLiteralKind;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct TokenIntegerLiteral {
    kind: TokenIntegerLiteralKind,
    suffix_len: usize,
}

impl TokenIntegerLiteral {
    pub fn new(kind: TokenIntegerLiteralKind, suffix_len: usize) -> Self {
        Self { kind, suffix_len }
    }

    pub fn kind(&self) -> TokenIntegerLiteralKind {
        self.kind
    }

    pub fn suffix_len(&self) -> usize {
        self.suffix_len
    }
}
