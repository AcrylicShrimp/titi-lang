use ast::SymbolWithSpan;
use high_lexer::TokenLiteral;
use span::{Source, SourceMap, Span};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

#[derive(Debug)]
pub struct Context {
    source_map: SourceMap,
    modules: HashMap<PathBuf, ParsedModule>,
    scopes: Vec<ParsedScope>,
    structs: Vec<ParsedStruct>,
    functions: Vec<ParsedFunction>,
}

#[derive(Debug)]
pub struct ParsedModule {
    pub source: Arc<Source>,
    pub top_level_structs: Vec<ParsedTopLevelStruct>,
    pub top_level_functions: Vec<ParsedTopLevelFunction>,
}

#[derive(Debug)]
pub struct ParsedVis {
    pub kind: ParsedVisKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ParsedVisKind {
    None,
    Pub,
}

#[derive(Debug)]
pub struct ParsedTy {
    pub kind: ParsedTyKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ParsedTyKind {
    Bool,
    Byte,
    Char,
    I64,
    U64,
    Isize,
    Usize,
    F64,
    Str,
    Cptr(Box<ParsedTyKind>),
    Mptr(Box<ParsedTyKind>),
    Struct(usize),
}

#[derive(Debug)]
pub struct ParsedScope {
    pub parent: Option<usize>,
    pub kind: ParsedScopeKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ParsedScopeKind {
    Block,
    Loop,
    Struct(ParsedScopeStruct),
    Function(ParsedScopeFunction),
}

#[derive(Debug, Clone)]
pub struct ParsedScopeStruct {
    pub name: SymbolWithSpan,
    pub def: usize,
}

#[derive(Debug, Clone)]
pub struct ParsedScopeFunction {
    pub name: SymbolWithSpan,
    pub def: usize,
}

#[derive(Debug)]
pub struct ParsedStruct {
    pub name: SymbolWithSpan,
    pub fields: Vec<(SymbolWithSpan, ParsedVis, ParsedTy)>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsedStructField {
    pub name: SymbolWithSpan,
    pub ty: ParsedTy,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsedFunction {
    pub name: SymbolWithSpan,
    pub params: Vec<(SymbolWithSpan, ParsedTy)>,
    pub return_ty: Option<ParsedTy>,
    pub body: Vec<ParsedStmt>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsedTopLevelStruct {
    pub name: SymbolWithSpan,
    pub vis: ParsedVis,
    pub def: usize,
}

#[derive(Debug)]
pub struct ParsedTopLevelFunction {
    pub name: SymbolWithSpan,
    pub vis: ParsedVis,
    pub def: usize,
}

#[derive(Debug)]
pub struct ParsedStmt {
    pub kind: ParsedStmtKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ParsedStmtKind {
    Struct(ParsedStmtStruct),
    Fn(ParsedStmtFn),
    Let(ParsedStmtLet),
    If(ParsedStmtIf),
    For(ParsedStmtFor),
    Block(ParsedStmtBlock),
    Break(ParsedStmtBreak),
    Continue(ParsedStmtContinue),
    Return(ParsedStmtReturn),
    Expr(ParsedExpr),
}

#[derive(Debug)]
pub struct ParsedStmtStruct {
    pub name: SymbolWithSpan,
    pub def: usize,
    pub scope: usize,
}

#[derive(Debug)]
pub struct ParsedStmtFn {
    pub name: SymbolWithSpan,
    pub def: usize,
    pub scope: usize,
}

#[derive(Debug)]
pub struct ParsedStmtLet {
    pub name: SymbolWithSpan,
    pub kind: ParsedStmtLetKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ParsedStmtLetKind {
    Ty(ParsedTy),
    Expr(ParsedExpr),
    TyExpr(ParsedTy, ParsedExpr),
}

#[derive(Debug)]
pub struct ParsedStmtIf {
    pub cond: ParsedExpr,
    pub then_body: ParsedStmtBlock,
    pub else_kind: Option<ParsedStmtElseKind>,
    pub span: Span,
}

#[derive(Debug)]
pub enum ParsedStmtElseKind {
    Else(ParsedStmtBlock),
    ElseIf(Box<ParsedStmtIf>),
}

#[derive(Debug)]
pub struct ParsedStmtFor {
    pub kind: ParsedStmtForKind,
    pub body: ParsedStmtBlock,
    pub span: Span,
}

#[derive(Debug)]
pub enum ParsedStmtForKind {
    Loop,
    While(ParsedExpr),
    ForIn(ParsedStmtForIn),
}

#[derive(Debug)]
pub struct ParsedStmtForIn {
    pub local: SymbolWithSpan,
    pub expr: ParsedExpr,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsedStmtBlock {
    pub stmts: Vec<ParsedStmt>,
    pub scope: usize,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsedStmtBreak {
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsedStmtContinue {
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsedStmtReturn {
    pub expr: Option<ParsedExpr>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsedExpr {
    pub kind: ParsedExprKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ParsedExprKind {
    Assign(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignAdd(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignSub(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignMul(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignDiv(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignMod(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignShl(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignShr(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignBitOr(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignBitAnd(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignBitXor(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignBitNot(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Rng(Box<ParsedExprKind>, Box<ParsedExprKind>),
    RngInclusive(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Eq(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Ne(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Lt(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Gt(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Le(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Ge(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Neg(Box<ParsedExprKind>),
    Add(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Sub(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Mul(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Div(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Mod(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Shl(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Shr(Box<ParsedExprKind>, Box<ParsedExprKind>),
    BitOr(Box<ParsedExprKind>, Box<ParsedExprKind>),
    BitAnd(Box<ParsedExprKind>, Box<ParsedExprKind>),
    BitXor(Box<ParsedExprKind>, Box<ParsedExprKind>),
    BitNot(Box<ParsedExprKind>),
    LogOr(Box<ParsedExprKind>, Box<ParsedExprKind>),
    LogAnd(Box<ParsedExprKind>, Box<ParsedExprKind>),
    LogNot(Box<ParsedExprKind>),
    Cast(Box<ParsedExprKind>, ParsedTy),
    Call(Box<ParsedExprKind>, Vec<ParsedExprKind>),
    Index(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Member(Box<ParsedExprKind>, SymbolWithSpan),
    Object(ParsedExprObject),
    Id(SymbolWithSpan),
    Literal(ParsedExprLiteral),
}

#[derive(Debug)]
pub struct ParsedExprObject {
    pub name: SymbolWithSpan,
    pub fields: Vec<ParsedExprObjectField>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsedExprObjectField {
    pub name: SymbolWithSpan,
    pub kind: ParsedExprObjectFieldKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ParsedExprObjectFieldKind {
    Expr(ParsedExpr),
    InnerObject(ParsedExprObjectFieldInnerObject),
}

#[derive(Debug)]
pub struct ParsedExprObjectFieldInnerObject {
    pub fields: Vec<ParsedExprObjectField>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsedExprLiteral {
    pub literal: TokenLiteral,
    pub span: Span,
}
