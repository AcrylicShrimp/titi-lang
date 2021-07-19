#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum TokenAssignOpKind {
    Add,    // "+="
    Sub,    // "-="
    Mul,    // "*="
    Div,    // "/="
    Mod,    // "%="
    Shl,    // "<<="
    Shr,    // ">>="
    BitOr,  // "|="
    BitAnd, // "&="
    BitXor, // "^="
    BitNot, // "~="
}
