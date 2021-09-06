use high_lexer::{Symbol, TokenLiteral};
use span::Span;

#[derive(Debug, Clone, Hash)]
pub struct TopLevel {
    pub kind: TopLevelKind,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub enum TopLevelKind {
    Fn(Fn),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolWithSpan {
    pub symbol: Symbol,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Vis {
    pub kind: VisKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VisKind {
    Pub,
}

#[derive(Debug, Clone, Hash)]
pub struct Ty {
    pub kind: TyKind,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub enum TyKind {
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
    pub vis: Option<Vis>,
    pub name: SymbolWithSpan,
    pub params: Vec<FnParam>,
    pub return_ty: Option<Ty>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct FnParam {
    pub name: SymbolWithSpan,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub enum StmtKind {
    Break(Break),
    Continue(Continue),
    Return(Return),
    Expr(Expr),
}

#[derive(Debug, Clone, Hash)]
pub struct Break {
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct Continue {
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct Return {
    pub expr: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub enum ExprKind {
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
    Id(SymbolWithSpan),
    Literal(Literal),
}

#[derive(Debug, Clone, Hash)]
pub struct Literal {
    pub lit: TokenLiteral,
    pub span: Span,
}
