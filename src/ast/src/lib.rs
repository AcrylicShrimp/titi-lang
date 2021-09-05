use high_lexer::{Symbol, TokenLiteral};

// TODO: Add spans to the AST to represent the source code location of each node.

#[derive(Debug, Clone, Hash)]
pub enum TopLevel {
    Fn(Fn),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Vis {
    Pub,
    None,
}

#[derive(Debug, Clone, Hash)]
pub enum Ty {
    Bool,
    Byte,
    Char,
    I64,
    U64,
    Isize,
    Usize,
    F64,
    Str,
    Cptr(Box<Ty>),
    Mptr(Box<Ty>),
}

#[derive(Debug, Clone, Hash)]
pub struct Fn {
    pub vis: Vis,
    pub name: Symbol,
    pub params: Vec<FnParam>,
    pub return_ty: Option<Ty>,
    pub body: Block,
}

#[derive(Debug, Clone, Hash)]
pub struct FnParam {
    pub name: Symbol,
    pub ty: Ty,
}

#[derive(Debug, Clone, Hash)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, Hash)]
pub enum Stmt {
    Expr(Expr),
}

#[derive(Debug, Clone, Hash)]
pub enum Expr {
    Assign(Box<Expr>, Box<Expr>),
    AssignAdd(Box<Expr>, Box<Expr>),
    AssignSub(Box<Expr>, Box<Expr>),
    AssignMul(Box<Expr>, Box<Expr>),
    AssignDiv(Box<Expr>, Box<Expr>),
    AssignMod(Box<Expr>, Box<Expr>),
    AssignShl(Box<Expr>, Box<Expr>),
    AssignShr(Box<Expr>, Box<Expr>),
    AssignBitOr(Box<Expr>, Box<Expr>),
    AssignBitAnd(Box<Expr>, Box<Expr>),
    AssignBitXor(Box<Expr>, Box<Expr>),
    AssignBitNot(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Shl(Box<Expr>, Box<Expr>),
    Shr(Box<Expr>, Box<Expr>),
    BitOr(Box<Expr>, Box<Expr>),
    BitAnd(Box<Expr>, Box<Expr>),
    BitXor(Box<Expr>, Box<Expr>),
    BitNot(Box<Expr>),
    LogOr(Box<Expr>, Box<Expr>),
    LogAnd(Box<Expr>, Box<Expr>),
    LogNot(Box<Expr>),
    Cast(Box<Expr>, Ty),
    Call(Box<Expr>, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Id(Symbol),
    Literal(TokenLiteral),
}
