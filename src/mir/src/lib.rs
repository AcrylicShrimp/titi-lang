mod transform;
mod transform_expr;
mod ty;

pub use transform::*;

use ast::{SymbolWithSpan, Vis};
use hir::ModuleDef;
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
    pub module: ModuleDef,
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
    pub vis: Option<Vis>,
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
    Let(MirLetDef),
    Label(MirStmtDef),
}

#[derive(Debug)]
pub enum MirStmtKind {
    Scope(MirScope),
    Expr(MirExprDef),
    Goto(MirStmtDef),
    GotoIf(MirStmtDef, MirExprDef),
    Return(Option<MirExprDef>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MirTy {
    pub kind: MirTyKind,
    pub source_kind: Option<MirTySourceKind>,
    pub is_ref: bool,
}

impl MirTy {
    pub fn is_none(&self) -> bool {
        self.kind == MirTyKind::None
    }

    pub fn is_addressable(&self) -> bool {
        self.source_kind.is_some() || self.is_ref
    }

    pub fn is_same_with(&self, other: &MirTy) -> bool {
        self.kind == other.kind && self.is_ref == other.is_ref
    }

    pub fn is_assignable_from(&self, other: &MirTy) -> bool {
        if self.is_none() || other.is_none() {
            return false;
        }

        if self.is_ref {
            if !other.is_ref {
                return false;
            }

            return self.kind == other.kind;
        }

        if other.is_ref {
            return false;
        }

        match &self.kind {
            MirTyKind::None => false,
            MirTyKind::Bool => match other.kind {
                MirTyKind::Bool
                | MirTyKind::Byte
                | MirTyKind::Char
                | MirTyKind::I64
                | MirTyKind::U64
                | MirTyKind::Isize
                | MirTyKind::Usize => true,
                _ => false,
            },
            MirTyKind::Byte => match other.kind {
                MirTyKind::Bool | MirTyKind::Byte => true,
                _ => false,
            },
            MirTyKind::Char => match other.kind {
                MirTyKind::Bool | MirTyKind::Byte | MirTyKind::Char => true,
                _ => false,
            },
            MirTyKind::I64 => match other.kind {
                MirTyKind::Bool | MirTyKind::Byte | MirTyKind::Char | MirTyKind::I64 => true,
                _ => false,
            },
            MirTyKind::U64 => match other.kind {
                MirTyKind::Bool | MirTyKind::Byte | MirTyKind::Char | MirTyKind::U64 => true,
                _ => false,
            },
            MirTyKind::Isize => match other.kind {
                MirTyKind::Bool | MirTyKind::Byte | MirTyKind::Isize => true,
                _ => false,
            },
            MirTyKind::Usize => match other.kind {
                MirTyKind::Bool | MirTyKind::Byte | MirTyKind::Usize => true,
                _ => false,
            },
            MirTyKind::F64 => match other.kind {
                MirTyKind::F64 => true,
                _ => false,
            },
            MirTyKind::Str => match other.kind {
                MirTyKind::Str => true,
                _ => false,
            },
            MirTyKind::Ptr(lhs_ty) => match other.kind {
                MirTyKind::Bool | MirTyKind::Byte | MirTyKind::Usize => true,
                MirTyKind::Ptr(rhs_ty) => lhs_ty.as_ty().is_same_with(rhs_ty.as_ty()),
                _ => false,
            },
            MirTyKind::Range(lhs_ty0, lhs_ty1) => match other.kind {
                MirTyKind::Range(rhs_ty0, rhs_ty1) => {
                    lhs_ty0.as_ty().is_same_with(rhs_ty0.as_ty())
                        && lhs_ty1.as_ty().is_same_with(rhs_ty1.as_ty())
                }
                _ => false,
            },
            MirTyKind::RangeInclusive(lhs_ty0, lhs_ty1) => match other.kind {
                MirTyKind::RangeInclusive(rhs_ty0, rhs_ty1) => {
                    lhs_ty0.as_ty().is_same_with(rhs_ty0.as_ty())
                        && lhs_ty1.as_ty().is_same_with(rhs_ty1.as_ty())
                }
                _ => false,
            },
            &MirTyKind::Struct(lhs_def) => match other.kind {
                MirTyKind::Struct(rhs_def) => lhs_def == rhs_def,
                _ => false,
            },
            &MirTyKind::InnerStruct(lhs_def) => match other.kind {
                MirTyKind::InnerStruct(rhs_def) => lhs_def == rhs_def,
                _ => false,
            },
            MirTyKind::Fn(lhs_fn) => match &other.kind {
                MirTyKind::Fn(rhs_fn) => {
                    match lhs_fn.return_ty {
                        Some(lhs_return_ty) => {
                            return match rhs_fn.return_ty {
                                Some(rhs_return_ty) => {
                                    lhs_return_ty.as_ty().is_same_with(rhs_return_ty.as_ty())
                                }
                                None => false,
                            };
                        }
                        None => {
                            if !rhs_fn.return_ty.is_none() {
                                return false;
                            }
                        }
                    }

                    if lhs_fn.params.len() != rhs_fn.params.len() {
                        return false;
                    }

                    for index in 0..lhs_fn.params.len() {
                        if !lhs_fn.params[index]
                            .as_ty()
                            .is_same_with(&rhs_fn.params[index].as_ty())
                        {
                            return false;
                        }
                    }

                    true
                }
                _ => false,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    Ptr(Ty),
    Range(Ty, Ty),
    RangeInclusive(Ty, Ty),
    Struct(MirStructDef),
    InnerStruct(MirInnerStructDef),
    Fn(MirTyFn),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MirTySourceKind {
    Deref,
    Let(MirScopeDef),
    Struct(MirStructDef, usize),
    InnerStruct(MirInnerStructDef, usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MirTyFn {
    pub params: Vec<Ty>,
    pub return_ty: Option<Ty>,
}

#[derive(Debug)]
pub struct MirExpr {
    pub ty: Ty,
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
