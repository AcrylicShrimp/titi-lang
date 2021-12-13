// mod analysis;
mod make_global;
mod resolve;
// mod deduce;
// mod lookup;
// mod ops;

pub use resolve::*;

// pub use analysis::*;
// pub use deduce::*;
// pub use lookup::*;

use ast::*;
use span::{Source, SourceMap, Span};
use std::sync::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScopeDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InnerStructDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionHeaderDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LetDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExprDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TyDef(pub usize);

#[derive(Debug, Clone, Hash)]
pub struct TyRef {
    pub scope: ScopeRef,
    pub ty: Ty,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ScopeRef {
    Module(ModuleDef),
    Scope(ScopeDef),
}

// #[derive(Debug, Clone)]
// pub struct ResolvedType {
//     pub kind: ResolvedTypeKind,
//     pub addressable: bool,
//     pub assignable: bool,
//     pub ref_kind: Option<TyRefKind>,
//     pub span: Span,
// }

// #[derive(Debug, Clone, Copy)]
// pub enum ResolvedTypeKind {
//     None,
//     Bool,
//     Byte,
//     Char,
//     I64,
//     U64,
//     Isize,
//     Usize,
//     F64,
//     Str,
//     Cptr(TyDef),
//     Mptr(TyDef),
//     Range(TyDef, TyDef),
//     RangeInclusive(TyDef, TyDef),
//     Struct(StructDef),
//     InnerStruct(InnerStructDef),
//     Fn(FunctionDef),
//     FnHeader(FunctionHeaderDef),
//     Let(LetDef),
// }

// #[derive(Debug)]
// pub struct SymbolTable {
//     pub source_map: SourceMap,
//     pub modules: Vec<ResolvedModule>,
//     pub scope: Vec<GlobalScope>,
//     pub structs: Vec<GlobalStruct>,
//     pub inner_structs: Vec<GlobalInnerStruct>,
//     pub fns: Vec<GlobalFn>,
//     pub fn_headers: Vec<FnHeader>,
//     pub lets: Vec<GlobalLet>,
//     pub exprs: Vec<GlobalExpr>,
//     pub types: Vec<GlobalTy>,
// }

// #[derive(Debug)]
// pub struct ResolvedModule {
//     pub def: ModuleDef,
//     pub source: Arc<Source>,
//     pub uses: Vec<ResolvedModuleUse>,
//     pub structs: Vec<ResolvedModuleStruct>,
//     pub fns: Vec<ResolvedModuleFn>,
//     pub fn_headers: Vec<ResolvedModuleFnHeader>,
// }

// #[derive(Debug)]
// pub struct ResolvedModuleUse {
//     pub name: SymbolWithSpan,
//     pub def: ModuleDef,
//     pub span: Span,
// }

// #[derive(Debug)]
// pub struct ResolvedModuleStruct {
//     pub name: SymbolWithSpan,
//     pub prefix: Option<TopLevelItemPrefix>,
//     pub def: StructDef,
//     pub span: Span,
// }

// #[derive(Debug)]
// pub struct ResolvedModuleFn {
//     pub name: SymbolWithSpan,
//     pub prefix: Option<TopLevelItemPrefix>,
//     pub def: FunctionDef,
//     pub span: Span,
// }

// #[derive(Debug)]
// pub struct ResolvedModuleFnHeader {
//     pub name: SymbolWithSpan,
//     pub def: FunctionHeaderDef,
//     pub span: Span,
// }

// #[derive(Debug)]
// pub struct GlobalScope {
//     pub module: ModuleDef,
//     pub kind: Option<ScopeKind>,
//     pub parent: Option<ScopeDef>,
// }

// #[derive(Debug)]
// pub enum ScopeKind {
//     Struct(ScopeStruct),
//     Fn(ScopeFn),
//     Let(ScopeLet),
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

// #[derive(Debug)]
// pub struct GlobalExpr {
//     pub scope: ScopeDef,
//     pub kind: GlobalExprKind,
//     pub span: Span,
// }

// #[derive(Debug)]
// pub enum GlobalExprKind {
//     Assign(ExprDef, ExprDef),
//     AssignAdd(ExprDef, ExprDef),
//     AssignSub(ExprDef, ExprDef),
//     AssignMul(ExprDef, ExprDef),
//     AssignDiv(ExprDef, ExprDef),
//     AssignMod(ExprDef, ExprDef),
//     AssignShl(ExprDef, ExprDef),
//     AssignShr(ExprDef, ExprDef),
//     AssignBitOr(ExprDef, ExprDef),
//     AssignBitAnd(ExprDef, ExprDef),
//     AssignBitXor(ExprDef, ExprDef),
//     AssignBitNot(ExprDef, ExprDef),
//     Rng(ExprDef, ExprDef),
//     RngInclusive(ExprDef, ExprDef),
//     Eq(ExprDef, ExprDef),
//     Ne(ExprDef, ExprDef),
//     Lt(ExprDef, ExprDef),
//     Gt(ExprDef, ExprDef),
//     Le(ExprDef, ExprDef),
//     Ge(ExprDef, ExprDef),
//     Neg(ExprDef),
//     Add(ExprDef, ExprDef),
//     Sub(ExprDef, ExprDef),
//     Mul(ExprDef, ExprDef),
//     Div(ExprDef, ExprDef),
//     Mod(ExprDef, ExprDef),
//     Shl(ExprDef, ExprDef),
//     Shr(ExprDef, ExprDef),
//     BitOr(ExprDef, ExprDef),
//     BitAnd(ExprDef, ExprDef),
//     BitXor(ExprDef, ExprDef),
//     BitNot(ExprDef),
//     LogOr(ExprDef, ExprDef),
//     LogAnd(ExprDef, ExprDef),
//     LogNot(ExprDef),
//     Cast(ExprDef, TyDef),
//     Object(GlobalObject),
//     Call(ExprDef, Vec<ExprDef>),
//     Index(ExprDef, ExprDef),
//     Member(ExprDef, SymbolWithSpan),
//     Id(SymbolWithSpan),
//     Literal(Literal),
// }

// #[derive(Debug)]
// pub struct GlobalObject {
//     pub ty: StructDef,
//     pub fields: Vec<GlobalObjectField>,
//     pub span: Span,
// }

// #[derive(Debug)]
// pub struct GlobalObjectField {
//     pub name: SymbolWithSpan,
//     pub kind: GlobalObjectFieldKind,
//     pub span: Span,
// }

// #[derive(Debug)]
// pub enum GlobalObjectFieldKind {
//     Expr(ExprDef),
//     InnerObject(GlobalInnerObject),
// }

// #[derive(Debug)]
// pub struct GlobalInnerObject {
//     pub ty: InnerStructDef,
//     pub fields: Vec<GlobalObjectField>,
//     pub span: Span,
// }

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
