use crate::{Symbol, TokenLiteralKind};
use str_interner::StrIdx;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TokenLiteral {
    kind: TokenLiteralKind,
    str: Symbol,
    suffix: Option<Symbol>,
}

impl TokenLiteral {
    pub fn new(kind: TokenLiteralKind, str: Symbol, suffix: Option<Symbol>) -> Self {
        Self { kind, str, suffix }
    }

    pub fn kind(&self) -> TokenLiteralKind {
        self.kind
    }

    pub fn str(&self) -> Symbol {
        self.str
    }

    pub fn suffix(&self) -> Option<Symbol> {
        self.suffix
    }
}
