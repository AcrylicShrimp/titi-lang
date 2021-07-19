use crate::{TokenAssignOpKind, TokenCmpOpKind};

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum TokenBinaryOpKind {
    Assign(TokenAssignOpKind),
    Cmp(TokenCmpOpKind),
    Add,    // "+"
    Sub,    // "-"
    Mul,    // "*"
    Div,    // "/"
    Mod,    // "%"
    Shl,    // "<<"
    Shr,    // ">>"
    BitOr,  // "|"
    BitAnd, // "&"
    BitXor, // "^"
    LogOr,  // "||"
    LogAnd, // "&&"
}
