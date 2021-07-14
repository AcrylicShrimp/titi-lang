use crate::TokenIntegerLiteralKind;

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum TokenNumberLiteralKind {
    Integer(TokenIntegerLiteralKind),
    Float,
}
