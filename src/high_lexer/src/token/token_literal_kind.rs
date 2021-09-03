#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenLiteralKind {
    Bool,
    IntegerBinary,
    IntegerOctal,
    IntegerHexadecimal,
    IntegerDecimal,
    Float,
    SingleQuotedStr,
    DoubleQuotedStr,
}

impl TokenLiteralKind {
    pub fn is_number(&self) -> bool {
        match self {
            Self::IntegerBinary
            | Self::IntegerOctal
            | Self::IntegerHexadecimal
            | Self::IntegerDecimal
            | Self::Float => true,
            _ => false,
        }
    }
}
