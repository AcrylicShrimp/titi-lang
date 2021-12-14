mod expr;
mod function;
mod object;
mod stmt;
mod r#struct;

use crate::{
    ExprDef, FunctionDef, FunctionHeaderDef, InnerStructDef, ModuleDef, ResolvedContext,
    ResolvedModule, ScopeDef, ScopeRef, StmtDef, StructDef, TyRef, TyRefUserDef,
};
use ast::*;
use expr::*;
use function::*;
use object::*;
use r#struct::*;
use span::Span;
use stmt::*;

#[derive(Default, Debug)]
pub struct GlobalContext {
    pub modules: Vec<ResolvedModule>,
    pub scopes: Vec<GlobalScope>,
    pub structs: Vec<GlobalStruct>,
    pub inner_structs: Vec<GlobalInnerStruct>,
    pub fns: Vec<GlobalFn>,
    pub fn_headers: Vec<GlobalFnHeader>,
    pub stmts: Vec<GlobalStmt>,
    pub exprs: Vec<GlobalExpr>,
}

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
    pub scope: ScopeRef,
    pub kind: GlobalStmtKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum GlobalStmtKind {
    Block(GlobalStmtBlock),
    Let(GlobalStmtLet),
    If(GlobalStmtIf),
    Else(GlobalStmtElse),
    For(GlobalStmtFor),
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
    pub scope: ScopeRef,
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

pub fn make_global(resolved: ResolvedContext) -> GlobalContext {
    let mut structs = resolved.structs.into_iter().map(Some).collect::<Vec<_>>();
    let mut fns = resolved.fns.into_iter().map(Some).collect::<Vec<_>>();
    let mut fn_headers = resolved
        .fn_headers
        .into_iter()
        .map(Some)
        .collect::<Vec<_>>();

    let mut context = GlobalContext::default();
    context.modules = resolved.modules;

    for module in &mut context.modules {
        for r#struct in &mut module.structs {
            r#struct.def = make_global_struct(
                &mut context.structs,
                &mut context.inner_structs,
                ScopeRef::Module(module.def),
                structs[r#struct.def.0].take().unwrap().1,
            );
        }

        for r#fn in &mut module.fns {
            r#fn.def = make_global_fn(
                &mut context.scopes,
                &mut context.structs,
                &mut context.inner_structs,
                &mut context.fns,
                &mut context.fn_headers,
                &mut context.stmts,
                &mut context.exprs,
                ScopeRef::Module(module.def),
                fns[r#fn.def.0].take().unwrap().1,
            );
        }

        for header in &mut module.fn_headers {
            header.def = make_global_fn_header(
                &mut context.fn_headers,
                ScopeRef::Module(module.def),
                fn_headers[header.def.0].take().unwrap().1,
            );
        }
    }

    context
}
