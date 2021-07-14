use crate::TokenNumberLiteralKind;

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct TokenNumberLiteral {
    kind: TokenNumberLiteralKind,
    suffix_start: usize,
}

impl TokenNumberLiteral {
    pub fn new(kind: TokenNumberLiteralKind, suffix_start: usize) -> Self {
        Self { kind, suffix_start }
    }

    pub fn kind(&self) -> TokenNumberLiteralKind {
        self.kind
    }

    pub fn suffix_start(&self) -> usize {
        self.suffix_start
    }
}
