use crate::TokenNumberLiteral;
use crate::TokenStrLiteral;

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum TokenLiteralKind {
    Number(TokenNumberLiteral),
    SingleQuotedStr(TokenStrLiteral),
    DoubleQuotedStr(TokenStrLiteral),
}
