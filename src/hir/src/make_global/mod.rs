mod expr;
mod function;
mod object;
mod stmt;
mod r#struct;
mod ty;

pub use expr::*;
pub use function::*;
pub use object::*;
pub use r#struct::*;
pub use stmt::*;
pub use ty::*;

use crate::{
    ExprDef, FunctionDef, FunctionHeaderDef, InnerStructDef, ModuleDef, ScopeDef, StmtDef,
    StructDef, TyRef, TyRefUserDef,
};
use ast::*;
use span::Span;

#[derive(Debug)]
pub struct GlobalScope {
    pub module: ModuleDef,
    pub parent: Option<ScopeDef>,
    pub kind: GlobalScopeKind,
}

#[derive(Debug)]
pub enum GlobalScopeKind {
    Struct(StructDef),
    Fn(FunctionDef),
    Let(StmtDef),
    For(StmtDef),
}

#[derive(Debug)]
pub struct GlobalStruct {
    pub name: SymbolWithSpan,
    pub fields: Vec<GlobalStructField>,
    pub span: Span,
}

#[derive(Debug)]
pub struct GlobalStructField {
    pub vis: Option<Vis>,
    pub name: SymbolWithSpan,
    pub kind: GlobalStructFieldKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum GlobalStructFieldKind {
    Plain(TyRef),
    Struct(InnerStructDef),
}

#[derive(Debug)]
pub struct GlobalInnerStruct {
    pub fields: Vec<GlobalInnerStructField>,
    pub span: Span,
}

#[derive(Debug)]
pub struct GlobalInnerStructField {
    pub name: SymbolWithSpan,
    pub kind: GlobalStructFieldKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct GlobalFn {
    pub header: FunctionHeaderDef,
    pub body: StmtDef,
    pub span: Span,
}

#[derive(Debug)]
pub struct GlobalFnHeader {
    pub name: SymbolWithSpan,
    pub params: Vec<GlobalFnParam>,
    pub return_ty: Option<TyRef>,
    pub span: Span,
}

#[derive(Debug)]
pub struct GlobalFnParam {
    pub name: SymbolWithSpan,
    pub ty: TyRef,
    pub span: Span,
}

#[derive(Debug)]
pub struct GlobalStmt {
    pub scope: ScopeDef,
    pub kind: GlobalStmtKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum GlobalStmtKind {
    Let(GlobalStmtLet),
    If(GlobalStmtIf),
    For(GlobalStmtFor),
    Else(GlobalStmtElse),
    Block(GlobalStmtBlock),
    Break(GlobalStmtBreak),
    Continue(GlobalStmtContinue),
    Return(GlobalStmtReturn),
    Expr(ExprDef),
}

#[derive(Debug)]
pub struct GlobalStmtLet {
    pub name: SymbolWithSpan,
    pub kind: GlobalStmtLetKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum GlobalStmtLetKind {
    Ty(TyRef),
    Expr(ExprDef),
    TyExpr(TyRef, ExprDef),
}

#[derive(Debug)]
pub struct GlobalStmtFor {
    pub kind: GlobalStmtForKind,
    pub body: StmtDef,
    pub span: Span,
}

#[derive(Debug)]
pub enum GlobalStmtForKind {
    Loop,
    While(ExprDef),
    ForIn(SymbolWithSpan, ExprDef),
}

#[derive(Debug)]
pub struct GlobalStmtIf {
    pub cond: ExprDef,
    pub then_body: StmtDef,
    pub else_kind: Option<StmtDef>,
    pub span: Span,
}

#[derive(Debug)]
pub struct GlobalStmtElse {
    pub kind: GlobalStmtElseKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum GlobalStmtElseKind {
    Else(StmtDef),
    ElseIf(StmtDef),
}

#[derive(Debug)]
pub struct GlobalStmtBlock {
    pub stmts: Vec<StmtDef>,
    pub span: Span,
}

#[derive(Debug)]
pub struct GlobalStmtBreak {
    pub span: Span,
}

#[derive(Debug)]
pub struct GlobalStmtContinue {
    pub span: Span,
}

#[derive(Debug)]
pub struct GlobalStmtReturn {
    pub expr: Option<ExprDef>,
    pub span: Span,
}

#[derive(Debug)]
pub struct GlobalExpr {
    pub scope: ScopeDef,
    pub kind: GlobalExprKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum GlobalExprKind {
    Assign(ExprDef, ExprDef),
    AssignAdd(ExprDef, ExprDef),
    AssignSub(ExprDef, ExprDef),
    AssignMul(ExprDef, ExprDef),
    AssignDiv(ExprDef, ExprDef),
    AssignMod(ExprDef, ExprDef),
    AssignShl(ExprDef, ExprDef),
    AssignShr(ExprDef, ExprDef),
    AssignBitOr(ExprDef, ExprDef),
    AssignBitAnd(ExprDef, ExprDef),
    AssignBitXor(ExprDef, ExprDef),
    AssignBitNot(ExprDef, ExprDef),
    Rng(ExprDef, ExprDef),
    RngInclusive(ExprDef, ExprDef),
    Eq(ExprDef, ExprDef),
    Ne(ExprDef, ExprDef),
    Lt(ExprDef, ExprDef),
    Gt(ExprDef, ExprDef),
    Le(ExprDef, ExprDef),
    Ge(ExprDef, ExprDef),
    Neg(ExprDef),
    Add(ExprDef, ExprDef),
    Sub(ExprDef, ExprDef),
    Mul(ExprDef, ExprDef),
    Div(ExprDef, ExprDef),
    Mod(ExprDef, ExprDef),
    Shl(ExprDef, ExprDef),
    Shr(ExprDef, ExprDef),
    BitOr(ExprDef, ExprDef),
    BitAnd(ExprDef, ExprDef),
    BitXor(ExprDef, ExprDef),
    BitNot(ExprDef),
    LogOr(ExprDef, ExprDef),
    LogAnd(ExprDef, ExprDef),
    LogNot(ExprDef),
    Cast(ExprDef, TyRef),
    Object(GlobalObject),
    Call(ExprDef, Vec<ExprDef>),
    Index(ExprDef, ExprDef),
    Member(ExprDef, SymbolWithSpan),
    Deref(ExprDef),
    Id(SymbolWithSpan),
    Literal(Literal),
}

#[derive(Debug)]
pub struct GlobalObject {
    pub ty: TyRefUserDef,
    pub fields: Vec<GlobalObjectField>,
    pub span: Span,
}

#[derive(Debug)]
pub struct GlobalObjectField {
    pub name: SymbolWithSpan,
    pub kind: GlobalObjectFieldKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum GlobalObjectFieldKind {
    Expr(ExprDef),
    InnerObject(GlobalInnerObject),
}

#[derive(Debug)]
pub struct GlobalInnerObject {
    pub fields: Vec<GlobalObjectField>,
    pub span: Span,
}
