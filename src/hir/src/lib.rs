mod analysis;

pub use analysis::*;

use ast::*;
use span::{Source, SourceMap, Span};
use std::sync::Arc;

#[derive(Debug)]
pub struct ResolvedType {
    pub kind: ResolvedTypeKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ResolvedTypeKind {
    None,
    Bool,
    Byte,
    Char,
    I64,
    U64,
    Isize,
    Usize,
    F64,
    Str,
    Cptr(Box<ResolvedType>),
    Mptr(Box<ResolvedType>),
    Range,
    RangeEq,
    Struct(usize),
    InnerStruct(usize),
    Fn(usize),
    FnHeader(usize),
    Let(usize),
}

#[derive(Debug)]
pub struct SymbolTable {
    pub source_map: SourceMap,
    pub modules: Vec<ResolvedModule>,
    pub scope: Vec<GlobalScope>,
    pub structs: Vec<GlobalStruct>,
    pub inner_structs: Vec<GlobalInnerStruct>,
    pub fns: Vec<GlobalFn>,
    pub fn_headers: Vec<FnHeader>,
    pub lets: Vec<GlobalLet>,
    pub exprs: Vec<GlobalExpr>,
    pub types: Vec<GlobalTy>,
}

#[derive(Debug)]
pub struct ResolvedModule {
    pub def: usize,
    pub source: Arc<Source>,
    pub uses: Vec<ResolvedModuleUse>,
    pub structs: Vec<ResolvedModuleStruct>,
    pub fns: Vec<ResolvedModuleFn>,
    pub fn_headers: Vec<ResolvedModuleFnHeader>,
}

#[derive(Debug)]
pub struct ResolvedModuleUse {
    pub name: SymbolWithSpan,
    pub def: usize,
    pub span: Span,
}

#[derive(Debug)]
pub struct ResolvedModuleStruct {
    pub name: SymbolWithSpan,
    pub prefix: Option<TopLevelItemPrefix>,
    pub def: usize,
    pub span: Span,
}

#[derive(Debug)]
pub struct ResolvedModuleFn {
    pub name: SymbolWithSpan,
    pub prefix: Option<TopLevelItemPrefix>,
    pub def: usize,
    pub span: Span,
}

#[derive(Debug)]
pub struct ResolvedModuleFnHeader {
    pub name: SymbolWithSpan,
    pub def: usize,
    pub span: Span,
}

#[derive(Debug)]
pub struct GlobalScope {
    pub module: usize,
    pub kind: Option<ScopeKind>,
    pub parent: Option<usize>,
}

#[derive(Debug)]
pub enum ScopeKind {
    Struct(ScopeStruct),
    Fn(ScopeFn),
    Let(ScopeLet),
    For(ScopeFor),
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
    Plain(usize),
    Struct(usize),
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
    pub header: GlobalFnHeader,
    pub body: ScopeBlock,
    pub span: Span,
}

#[derive(Debug)]
pub struct GlobalFnHeader {
    pub name: SymbolWithSpan,
    pub params: Vec<GlobalFnParam>,
    pub return_ty: Option<usize>,
    pub span: Span,
}

#[derive(Debug)]
pub struct GlobalFnParam {
    pub name: SymbolWithSpan,
    pub ty: usize,
    pub span: Span,
}

#[derive(Debug)]
pub struct GlobalLet {
    pub name: SymbolWithSpan,
    pub kind: GlobalLetKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum GlobalLetKind {
    Ty(usize),
    Expr(usize),
    TyExpr(usize, usize),
}

#[derive(Debug)]
pub struct GlobalExpr {
    pub kind: GlobalExprKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum GlobalExprKind {
    Assign(usize, usize),
    AssignAdd(usize, usize),
    AssignSub(usize, usize),
    AssignMul(usize, usize),
    AssignDiv(usize, usize),
    AssignMod(usize, usize),
    AssignShl(usize, usize),
    AssignShr(usize, usize),
    AssignBitOr(usize, usize),
    AssignBitAnd(usize, usize),
    AssignBitXor(usize, usize),
    AssignBitNot(usize, usize),
    Rng(usize, usize),
    RngInclusive(usize, usize),
    Eq(usize, usize),
    Ne(usize, usize),
    Lt(usize, usize),
    Gt(usize, usize),
    Le(usize, usize),
    Ge(usize, usize),
    Neg(usize),
    Add(usize, usize),
    Sub(usize, usize),
    Mul(usize, usize),
    Div(usize, usize),
    Mod(usize, usize),
    Shl(usize, usize),
    Shr(usize, usize),
    BitOr(usize, usize),
    BitAnd(usize, usize),
    BitXor(usize, usize),
    BitNot(usize),
    LogOr(usize, usize),
    LogAnd(usize, usize),
    LogNot(usize),
    Cast(usize, usize),
    Object(GlobalObject),
    Call(usize, Vec<usize>),
    Index(usize, usize),
    Member(usize, SymbolWithSpan),
    Id(SymbolWithSpan),
    Literal(Literal),
}

#[derive(Debug)]
pub struct GlobalObject {
    pub ty: usize,
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
    Expr(usize),
    InnerObject(GlobalInnerObject),
}

#[derive(Debug)]
pub struct GlobalInnerObject {
    pub fields: Vec<GlobalObjectField>,
    pub span: Span,
}

#[derive(Debug)]
pub enum GlobalTy {
    Bool,
    Char,
    I64,
    U64,
    Isize,
    Usize,
    F64,
    Str,
    Mptr(usize),
    Cptr(usize),
    Struct(usize),
    Fn(usize),
    FnHeader(usize),
}

#[derive(Debug)]
pub struct ScopeStruct {
    pub name: SymbolWithSpan,
    pub def: usize,
}

#[derive(Debug)]
pub struct ScopeFn {
    pub name: SymbolWithSpan,
    pub def: usize,
}

#[derive(Debug)]
pub struct ScopeLet {
    pub name: SymbolWithSpan,
    pub def: usize,
}

#[derive(Debug)]
pub struct ScopeIf {
    pub cond: Expr,
    pub then_body: ScopeBlock,
    pub else_kind: Option<Box<ScopeElseKind>>,
    pub span: Span,
}

#[derive(Debug)]
pub enum ScopeElseKind {
    Else(ScopeBlock),
    ElseIf(ScopeIf),
}

#[derive(Debug)]
pub struct ScopeFor {
    pub kind: ScopeForKind,
    pub body: ScopeBlock,
    pub span: Span,
}

#[derive(Debug)]
pub enum ScopeForKind {
    Loop,
    While(usize),
    ForIn(SymbolWithSpan, usize),
}

#[derive(Debug)]
pub struct ScopeBlock {
    pub stmts: Vec<ScopeStmt>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ScopeStmt {
    pub kind: ScopeStmtKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ScopeStmtKind {
    Scope(usize),
    Block(ScopeBlock),
    If(ScopeIf),
    Break(Break),
    Continue(Continue),
    Return(Return),
    Expr(usize),
}
