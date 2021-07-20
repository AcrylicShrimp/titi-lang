#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenLiteralKind {
    Bool,
    Byte,
    Char,
    Integer,
    Float,
    Str,
}
