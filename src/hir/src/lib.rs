mod analysis;

use ast::*;
use high_lexer::Symbol;
use span::{Source, SourceMap, Span};
use std::sync::Arc;

#[derive(Debug)]
pub struct SymbolTable {
    pub source_map: SourceMap,
    pub modules: Vec<ResolvedModule>,
    pub scope: Vec<GlobalScope>,
    pub structs: Vec<GlobalStruct>,
    pub inner_structs: Vec<GlobalInnerStruct>,
    pub fns: Vec<GlobalFn>,
    pub fn_headers: Vec<FnHeader>,
    pub types: Vec<GlobalTy>,
}

#[derive(Debug)]
pub struct ResolvedModule {
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
    pub kind: ScopeKind,
    pub parent: Option<usize>,
}

#[derive(Debug)]
pub enum ScopeKind {
    Block,
    For(For),
    Struct(ScopeStruct),
    Fn(ScopeFn),
    Let(ScopeLet),
}

#[derive(Debug)]
pub struct GlobalStruct {
    // TODO: Add source or module to represent its origin
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
    pub kind: LetKind,
    pub span: Span,
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
    While(Expr),
    ForIn(SymbolWithSpan, Expr),
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
    If(ScopeIf),
    Break(Break),
    Continue(Continue),
    Return(Return),
    Expr(Expr),
}
