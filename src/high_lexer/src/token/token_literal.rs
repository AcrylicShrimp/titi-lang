use crate::TokenLiteralKind;
use str_interner::StrIdx;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TokenLiteral {
    kind: TokenLiteralKind,
    str: StrIdx,
    suffix: Option<StrIdx>,
}

impl TokenLiteral {
    pub fn new(kind: TokenLiteralKind, str: StrIdx, suffix: Option<StrIdx>) -> Self {
        Self { kind, str, suffix }
    }

    pub fn kind(&self) -> TokenLiteralKind {
        self.kind
    }

    pub fn str(&self) -> StrIdx {
        self.str
    }

    pub fn suffix(&self) -> Option<StrIdx> {
        self.suffix
    }
}
