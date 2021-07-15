#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum TokenBinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shl,
    Shr,
    And,
    Or,
    Xor,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}
