mod deduce;
mod transform;
mod transform_expr;
mod transform_expr_lhs;

pub use transform::*;

use ast::{SymbolWithSpan, Vis};
use hir::ModuleDef;
use span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MirStructDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MirInnerStructDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MirFunctionDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MirFunctionHeaderDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MirTyDef(pub usize);

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
    pub tys: Vec<MirTy>,
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

    pub fn push_ty(&mut self, ty: MirTy) -> MirTyDef {
        let def = MirTyDef(self.tys.len());
        self.tys.push(ty);
        def
    }
}

#[derive(Debug)]
pub struct MirFunctionContext {
    pub module: ModuleDef,
    pub header: MirFunctionHeaderDef,
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
    pub vis: Option<Vis>,
    pub name: SymbolWithSpan,
    pub kind: MirStructFieldKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum MirStructFieldKind {
    Plain(MirTyDef),
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
    pub return_ty: Option<MirTyDef>,
    pub span: Span,
}

#[derive(Debug)]
pub struct MirFnParam {
    pub name: SymbolWithSpan,
    pub ty: MirTyDef,
    pub span: Span,
}

#[derive(Debug)]
pub struct MirStmt {
    pub scope: MirScopeDef,
    pub kind: MirStmtKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct MirScope {
    pub kind: MirScopeKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum MirScopeKind {
    Let(MirScopeLet),
    Label(MirStmtDef),
}

#[derive(Debug)]
pub struct MirScopeLet {
    pub name: SymbolWithSpan,
    pub ty: MirTyDef,
    pub span: Span,
}

#[derive(Debug)]
pub enum MirStmtKind {
    Scope(MirScope),
    Expr(MirExprDef),
    Goto(MirStmtDef),
    GotoIf(MirStmtDef, MirExprDef),
    Return(Option<MirExprDef>),
}

#[derive(Debug, Clone)]
pub struct MirTy {
    pub kind: MirTyKind,
    pub ref_kind: Option<MirTyRefKind>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MirTyKind {
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
    Cptr(MirTyDef),
    Mptr(MirTyDef),
    Range(MirTyDef, MirTyDef),
    RangeInclusive(MirTyDef, MirTyDef),
    Struct(MirStructDef),
    InnerStruct(MirInnerStructDef),
    Fn(MirFunctionDef),
    FnHeader(MirFunctionHeaderDef),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MirTyRefKind {
    Cref,
    Mref,
}

#[derive(Debug)]
pub struct MirExpr {
    pub ty: MirTyDef,
    pub kind: MirExprKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum MirExprKind {
    Call(MirExprCall),
    Lit(MirExprLit),
}

#[derive(Debug)]
pub struct MirExprCall {
    pub target_kind: MirExprCallTargetKind,
    pub params: Vec<MirExprDef>,
}

#[derive(Debug)]
pub enum MirExprCallTargetKind {
    Fn(MirFunctionDef),
    FnHeader(MirFunctionHeaderDef),
    Intrinsic(MirIntrinsic),
}

#[derive(Debug)]
pub enum MirIntrinsic {
    Alloc,
    Load,
    Store,
    AddUnchecked,
    SubUnchecked,
}

#[derive(Debug)]
pub struct MirExprLit {
    pub span: Span,
}
