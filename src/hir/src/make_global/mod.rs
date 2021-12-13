mod expr;
mod object;

pub use expr::*;
pub use object::*;

use crate::{
    ExprDef, FunctionDef, FunctionHeaderDef, InnerStructDef, LetDef, ModuleDef, ScopeDef,
    StructDef, TyDef,
};
use ast::*;
use span::Span;

// #[derive(Debug)]
// pub struct GlobalScope {
//     pub module: ModuleDef,
//     pub kind: Option<GlobalScopeKind>,
//     pub parent: Option<ScopeDef>,
// }

// #[derive(Debug)]
// pub enum GlobalScopeKind {
//     Struct(StructDef),
//     Fn(FunctionDef),
//     Let(LetDef),
//     For(ScopeFor),
// }

// #[derive(Debug)]
// pub struct GlobalStruct {
//     pub name: SymbolWithSpan,
//     pub fields: Vec<GlobalStructField>,
//     pub span: Span,
// }

// #[derive(Debug)]
// pub struct GlobalStructField {
//     pub vis: Option<Vis>,
//     pub name: SymbolWithSpan,
//     pub kind: GlobalStructFieldKind,
//     pub span: Span,
// }

// #[derive(Debug)]
// pub enum GlobalStructFieldKind {
//     Plain(TyDef),
//     Struct(InnerStructDef),
// }

// #[derive(Debug)]
// pub struct GlobalInnerStruct {
//     pub fields: Vec<GlobalInnerStructField>,
//     pub span: Span,
// }

// #[derive(Debug)]
// pub struct GlobalInnerStructField {
//     pub name: SymbolWithSpan,
//     pub kind: GlobalStructFieldKind,
//     pub span: Span,
// }

// #[derive(Debug)]
// pub struct GlobalFn {
//     pub header: GlobalFnHeader,
//     pub body: ScopeBlock,
//     pub span: Span,
// }

// #[derive(Debug)]
// pub struct GlobalFnHeader {
//     pub name: SymbolWithSpan,
//     pub params: Vec<GlobalFnParam>,
//     pub return_ty: Option<TyDef>,
//     pub span: Span,
// }

// #[derive(Debug)]
// pub struct GlobalFnParam {
//     pub name: SymbolWithSpan,
//     pub ty: TyDef,
//     pub span: Span,
// }

// #[derive(Debug)]
// pub struct GlobalLet {
//     pub name: SymbolWithSpan,
//     pub kind: GlobalLetKind,
//     pub span: Span,
// }

// #[derive(Debug)]
// pub enum GlobalLetKind {
//     Ty(TyDef),
//     Expr(ExprDef),
//     TyExpr(TyDef, ExprDef),
// }

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
    Cast(ExprDef, Ty),
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
    pub ty: TyUserDef,
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

// #[derive(Debug)]
// pub enum GlobalTy {
//     Bool,
//     Char,
//     I64,
//     U64,
//     Isize,
//     Usize,
//     F64,
//     Str,
//     Mptr(TyDef),
//     Cptr(TyDef),
//     Struct(StructDef),
//     Fn(FunctionDef),
//     FnHeader(FunctionHeaderDef),
// }

// #[derive(Debug)]
// pub struct ScopeStruct {
//     pub name: SymbolWithSpan,
//     pub def: StructDef,
// }

// #[derive(Debug)]
// pub struct ScopeFn {
//     pub name: SymbolWithSpan,
//     pub def: FunctionDef,
// }

// #[derive(Debug)]
// pub struct ScopeLet {
//     pub name: SymbolWithSpan,
//     pub def: LetDef,
// }

// #[derive(Debug)]
// pub struct ScopeIf {
//     pub cond: Expr,
//     pub then_body: ScopeBlock,
//     pub else_kind: Option<Box<ScopeElseKind>>,
//     pub span: Span,
// }

// #[derive(Debug)]
// pub enum ScopeElseKind {
//     Else(ScopeBlock),
//     ElseIf(ScopeIf),
// }

// #[derive(Debug)]
// pub struct ScopeFor {
//     pub kind: ScopeForKind,
//     pub body: ScopeBlock,
//     pub span: Span,
// }

// #[derive(Debug)]
// pub enum ScopeForKind {
//     Loop,
//     While(ExprDef),
//     ForIn(SymbolWithSpan, ExprDef),
// }

// #[derive(Debug)]
// pub struct ScopeBlock {
//     pub stmts: Vec<ScopeStmt>,
//     pub span: Span,
// }

// #[derive(Debug)]
// pub struct ScopeStmt {
//     pub kind: ScopeStmtKind,
//     pub span: Span,
// }

// #[derive(Debug)]
// pub enum ScopeStmtKind {
//     Scope(ScopeDef),
//     Block(ScopeBlock),
//     If(ScopeIf),
//     Break(Break),
//     Continue(Continue),
//     Return(Return),
//     Expr(ExprDef),
// }
