mod transform;
mod transform_expr;
mod ty;

pub use transform::*;
pub use ty::*;

use ast::SymbolWithSpan;
use span::Span;
use ty::Ty;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MirStructDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MirInnerStructDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MirFunctionDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MirFunctionHeaderDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MirLetDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MirStmtDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MirExprDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MirScopeDef(pub usize);

#[derive(Default, Debug)]
pub struct MirContext {
    pub structs: Vec<MirStruct>,
    pub inner_structs: Vec<MirInnerStruct>,
    pub fns: Vec<MirFn>,
    pub fn_headers: Vec<MirFnHeader>,
}

impl MirContext {
    pub fn push_struct(&mut self, r#struct: MirStruct) -> MirStructDef {
        let def = MirStructDef(self.structs.len());
        self.structs.push(r#struct);
        def
    }

    pub fn push_inner_struct(&mut self, inner_struct: MirInnerStruct) -> MirInnerStructDef {
        let def = MirInnerStructDef(self.inner_structs.len());
        self.inner_structs.push(inner_struct);
        def
    }

    pub fn push_fn(&mut self, r#fn: MirFn) -> MirFunctionDef {
        let def = MirFunctionDef(self.fns.len());
        self.fns.push(r#fn);
        def
    }

    pub fn push_fn_header(&mut self, fn_header: MirFnHeader) -> MirFunctionHeaderDef {
        let def = MirFunctionHeaderDef(self.fn_headers.len());
        self.fn_headers.push(fn_header);
        def
    }
}

#[derive(Debug)]
pub struct MirFunctionContext {
    pub header: MirFunctionHeaderDef,
    pub lets: Vec<MirLet>,
    pub stmts: Vec<MirStmt>,
    pub exprs: Vec<MirExpr>,
}

#[derive(Debug)]
pub struct MirStruct {
    pub name: SymbolWithSpan,
    pub fields: Vec<MirStructField>,
    pub span: Span,
}

#[derive(Debug)]
pub struct MirStructField {
    pub name: SymbolWithSpan,
    pub kind: MirStructFieldKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum MirStructFieldKind {
    Plain(Ty),
    Struct(MirInnerStructDef),
}

#[derive(Debug)]
pub struct MirInnerStruct {
    pub fields: Vec<MirInnerStructField>,
    pub span: Span,
}

#[derive(Debug)]
pub struct MirInnerStructField {
    pub name: SymbolWithSpan,
    pub kind: MirStructFieldKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct MirFn {
    pub header: MirFunctionHeaderDef,
    pub body: Vec<MirStmt>,
    pub span: Span,
}

#[derive(Debug)]
pub struct MirFnHeader {
    pub name: SymbolWithSpan,
    pub params: Vec<MirFnParam>,
    pub return_ty: Option<Ty>,
    pub span: Span,
}

#[derive(Debug)]
pub struct MirFnParam {
    pub name: SymbolWithSpan,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug)]
pub struct MirStmt {
    pub scope: MirScopeDef,
    pub kind: MirStmtKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct MirLet {
    pub name: SymbolWithSpan,
    pub kind: MirLetKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum MirLetKind {
    Ty(Ty),
    Expr(MirExprDef),
    TyExpr(Ty, MirExprDef),
}

#[derive(Debug)]
pub struct MirScope {
    pub kind: MirScopeKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum MirScopeKind {
    Block,
    Let(MirLetDef),
    Label(MirStmtDef),
}

#[derive(Debug)]
pub enum MirStmtKind {
    Scope(MirScope),
    Expr(MirExprDef),
    Goto(MirStmtDef, GotoDirection),
    GotoIf(MirExprDef, MirStmtDef, GotoDirection),
    Return(Option<MirExprDef>),
}

#[derive(Debug)]
pub enum GotoDirection {
    Before,
    After,
}

#[derive(Debug)]
pub struct MirExpr {
    pub ty: Ty,
    pub kind: MirExprKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum MirExprKind {
    Struct(MirStructDef),
    Fn(MirFunctionDef),
    FnHeader(MirFunctionHeaderDef),
    Let(MirLetDef),
    Param(MirFnHeader, usize),
    Rng(MirExprDef, MirExprDef),
    RngInclusive(MirExprDef, MirExprDef),
    Eq(MirExprDef, MirExprDef),
    Ne(MirExprDef, MirExprDef),
    Lt(MirExprDef, MirExprDef),
    Gt(MirExprDef, MirExprDef),
    Le(MirExprDef, MirExprDef),
    Ge(MirExprDef, MirExprDef),
    Neg(MirExprDef),
    Add(MirExprDef, MirExprDef),
    Sub(MirExprDef, MirExprDef),
    Mul(MirExprDef, MirExprDef),
    Div(MirExprDef, MirExprDef),
    Mod(MirExprDef, MirExprDef),
    Shl(MirExprDef, MirExprDef),
    Shr(MirExprDef, MirExprDef),
    BitOr(MirExprDef, MirExprDef),
    BitAnd(MirExprDef, MirExprDef),
    BitXor(MirExprDef, MirExprDef),
    BitNot(MirExprDef),
    LogOr(MirExprDef, MirExprDef),
    LogAnd(MirExprDef, MirExprDef),
    LogNot(MirExprDef),
    Cast(MirExprDef, Ty),
    Object(MirObject),
    Call(MirExprDef, Vec<MirExprDef>),
    Index(MirExprDef, MirExprDef),
    Member(MirExprDef, SymbolWithSpan),
    SizeOf(Ty),
    AddrOf(MirExprDef),
    TakeRef(MirExprDef),
    Deref(MirExprDef),
    Literal(MirExprLit),
}

#[derive(Debug, Clone, Hash)]
pub struct MirObject {
    pub ty: Ty,
    pub fields: Vec<MirObjectField>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct MirObjectField {
    pub name: SymbolWithSpan,
    pub kind: MirObjectFieldKind,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub enum MirObjectFieldKind {
    Expr(MirExprDef),
    InnerObject(MirInnerObject),
}

#[derive(Debug, Clone, Hash)]
pub struct MirInnerObject {
    pub ty: Ty,
    pub fields: Vec<MirObjectField>,
    pub span: Span,
}

#[derive(Debug)]
pub struct MirExprLit {
    pub span: Span,
}
