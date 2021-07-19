#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum TokenCmpOpKind {
    Eq, // "=="
    Ne, // "!="
    Lt, // "<"
    Gt, // ">"
    Le, // "<="
    Ge, // ">="
}
