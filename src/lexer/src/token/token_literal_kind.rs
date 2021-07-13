use crate::TokenFloatLiteral;
use crate::TokenIntegerLiteral;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum TokenLiteralKind {
    Bool,
    Integer(TokenIntegerLiteral),
    Byte(TokenIntegerLiteral),
    Char(TokenIntegerLiteral),
    I64(TokenIntegerLiteral),
    U64(TokenIntegerLiteral),
    ISize(TokenIntegerLiteral),
    USize(TokenIntegerLiteral),
    F64(TokenFloatLiteral),
    Str,
}
