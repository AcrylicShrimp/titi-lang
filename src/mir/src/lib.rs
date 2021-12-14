use span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionHeaderDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TyDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MirDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MirLetDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MirExprDef(pub usize);

pub struct Mir {
    pub kind: MirKind,
    pub span: Span,
}

pub enum MirKind {
    Let(TyDef),
    Assign(MirLetDef, MirExprDef),
    Goto(MirDef),
    GotoIf(MirDef, MirExprDef),
    Return(Option<MirExprDef>),
}

pub struct MirExpr {
    pub kind: MirExprKind,
    pub span: Span,
}

pub enum MirExprKind {
    Let(MirLetDef),
    Unary(MirExprUnary),
    Binary(MirExprBinary),
    Call(MirExprCall),
}

pub struct MirExprUnary {
    pub kind: MirExprUnaryKind,
    pub lhs: MirExprDef,
}

pub enum MirExprUnaryKind {
    Neg,
    BitNot,
    LogNot,
}

pub struct MirExprBinary {
    pub kind: MirExprBinaryKind,
    pub lhs: MirExprDef,
    pub rhs: MirExprDef,
}

pub enum MirExprBinaryKind {
    Add,
    Sub,
}

pub struct MirExprCall {
    pub target_kind: MirExprCallTargetKind,
    pub params: Vec<MirExprDef>,
}

pub enum MirExprCallTargetKind {
    Fn(FunctionDef),
    FnHeader(FunctionHeaderDef),
    Intrinsic(),
}
