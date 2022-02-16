use high_lexer::{Symbol, TokenLiteral};
use span::{Source, Span};
use std::sync::Arc;

#[derive(Debug, Clone, Hash)]
pub struct Module {
    pub source: Arc<Source>,
    pub top_levels: Vec<TopLevel>,
}

#[derive(Debug, Clone, Hash)]
pub struct TopLevel {
    pub kind: TopLevelKind,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub enum TopLevelKind {
    Use(Use),
    Struct(TopLevelItem<Struct>),
    Fn(TopLevelItem<Fn>),
    FnHeader(TopLevelFnHeader),
}

#[derive(Debug, Clone, Hash)]
pub struct TopLevelItem<T> {
    pub prefix: Option<TopLevelItemPrefix>,
    pub item: T,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct TopLevelFnHeader {
    pub ext: Extern,
    pub header: FnHeader,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct TopLevelItemPrefix {
    pub kind: TopLevelItemPrefixKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TopLevelItemPrefixKind {
    Extern(Extern),
    Vis(Vis),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolWithSpan {
    pub symbol: Symbol,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Extern {
    pub kind: ExternKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExternKind {
    Extern,
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
    pub is_ref: bool,
    pub sub_ty: SubTy,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct SubTy {
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
    Ptr(Box<SubTy>),
    Fn(Box<FnHeaderWithoutName>),
    UserDef(TyUserDef),
}

#[derive(Debug, Clone, Hash)]
pub struct TyUserDef {
    pub module: Option<SymbolWithSpan>,
    pub id: SymbolWithSpan,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct Use {
    pub segments: Vec<SymbolWithSpan>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct Struct {
    pub name: SymbolWithSpan,
    pub fields: Vec<StructField>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct StructField {
    pub vis: Option<Vis>,
    pub name: SymbolWithSpan,
    pub kind: StructFieldKind,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub enum StructFieldKind {
    Plain(Ty),
    Struct(InnerStruct),
}

#[derive(Debug, Clone, Hash)]
pub struct InnerStruct {
    pub fields: Vec<InnerStructField>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct InnerStructField {
    pub name: SymbolWithSpan,
    pub kind: StructFieldKind,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct Fn {
    pub header: FnHeader,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct FnHeader {
    pub name: SymbolWithSpan,
    pub params: Vec<FnParam>,
    pub return_ty: Option<Ty>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct FnHeaderWithoutName {
    pub params: Vec<Ty>,
    pub return_ty: Option<Ty>,
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
    Block(Block),
    Struct(Struct),
    Fn(Fn),
    Let(Let),
    If(If),
    For(For),
    Break(Break),
    Continue(Continue),
    Return(Return),
    Assign(Assign),
    Expr(Expr),
}

#[derive(Debug, Clone, Hash)]
pub struct Let {
    pub name: SymbolWithSpan,
    pub kind: LetKind,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub enum LetKind {
    Ty(Ty),
    Expr(Expr),
    TyExpr(Ty, Expr),
}

#[derive(Debug, Clone, Hash)]
pub struct If {
    pub cond: Expr,
    pub then_body: Block,
    pub else_kind: Option<Box<ElseKind>>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub enum ElseKind {
    Else(Block),
    ElseIf(If),
}

#[derive(Debug, Clone, Hash)]
pub struct For {
    pub kind: ForKind,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub enum ForKind {
    Loop,
    While(Expr),
    ForIn(SymbolWithSpan, Expr),
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
pub struct Assign {
    pub lhs: Expr,
    pub rhs: Expr,
    pub kind: AssignKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AssignKind {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shl,
    Shr,
    BitOr,
    BitAnd,
    BitXor,
    BitNot,
}

#[derive(Debug, Clone, Hash)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub enum ExprKind {
    Rng(Box<Expr>, Box<Expr>),
    RngInclusive(Box<Expr>, Box<Expr>),
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
    Object(Object),
    Call(Box<Expr>, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Member(Box<Expr>, SymbolWithSpan),
    ModuleMember(SymbolWithSpan, SymbolWithSpan),
    SizeOf(Ty),
    AddrOf(Box<Expr>),
    TakeRef(Box<Expr>),
    Deref(Box<Expr>),
    Id(SymbolWithSpan),
    Literal(Literal),
}

#[derive(Debug, Clone, Hash)]
pub struct Object {
    pub ty: TyUserDef,
    pub fields: Vec<ObjectField>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ObjectField {
    pub name: SymbolWithSpan,
    pub kind: ObjectFieldKind,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub enum ObjectFieldKind {
    Expr(Expr),
    InnerObject(InnerObject),
}

#[derive(Debug, Clone, Hash)]
pub struct InnerObject {
    pub fields: Vec<ObjectField>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct Literal {
    pub lit: TokenLiteral,
    pub span: Span,
}
