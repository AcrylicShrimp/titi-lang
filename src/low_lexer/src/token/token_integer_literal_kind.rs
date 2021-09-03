#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum TokenIntegerLiteralKind {
    Binary,
    Octal,
    Hexadecimal,
    Decimal,
}

impl TokenIntegerLiteralKind {
    pub fn is_decimal(&self) -> bool {
        match self {
            Self::Decimal => true,
            _ => false,
        }
    }
}
