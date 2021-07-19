#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum TokenUnaryOpKind {
    Plus,   // "+"
    Minus,  // "-"
    BitNot, // "~"
    LogNot, // "!"
}
