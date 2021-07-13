#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum TokenIntegerLiteralKind {
    Binary,
    Hexadecimal,
    Decimal,
}
