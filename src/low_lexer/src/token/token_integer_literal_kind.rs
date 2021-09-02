#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum TokenIntegerLiteralKind {
    Binary,
    Octal,
    Hexadecimal,
    Decimal,
}
