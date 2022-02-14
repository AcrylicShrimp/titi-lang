mod expr;
mod object;
mod stmt;

pub use expr::*;
pub use object::*;
pub use stmt::*;

use crate::{FunctionDef, ModuleDef, StructDef, TyRef, TyRefUserDef};
use ast::{Literal, SymbolWithSpan};
use span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InFnScopeDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InFnBlockDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InFnLetDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InFnIfDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InFnElseDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InFnForDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InFnStmtDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InFnExprDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InFnLitDef(pub usize);

#[derive(Debug)]
pub struct InFnContext {
    pub module: ModuleDef,
    pub scopes: Vec<InFnScope>,
    pub blocks: Vec<InFnBlock>,
    pub lets: Vec<InFnLet>,
    pub ifs: Vec<InFnIf>,
    pub elses: Vec<InFnElse>,
    pub fors: Vec<InFnFor>,
    pub stmts: Vec<InFnStmt>,
    pub exprs: Vec<InFnExpr>,
    pub lits: Vec<Literal>,
}

impl InFnContext {
    pub fn new(module: ModuleDef) -> Self {
        Self {
            module,
            scopes: Vec::new(),
            blocks: Vec::new(),
            lets: Vec::new(),
            ifs: Vec::new(),
            elses: Vec::new(),
            fors: Vec::new(),
            stmts: Vec::new(),
            exprs: Vec::new(),
            lits: Vec::new(),
        }
    }

    pub fn push_scope(&mut self, scope: InFnScope) -> InFnScopeDef {
        let def = self.scopes.len();
        self.scopes.push(scope);
        InFnScopeDef(def)
    }

    pub fn push_block(&mut self, block: InFnBlock) -> InFnBlockDef {
        let def = self.blocks.len();
        self.blocks.push(block);
        InFnBlockDef(def)
    }

    pub fn push_let(&mut self, r#let: InFnLet) -> InFnLetDef {
        let def = self.lets.len();
        self.lets.push(r#let);
        InFnLetDef(def)
    }

    pub fn push_if(&mut self, r#if: InFnIf) -> InFnIfDef {
        let def = self.ifs.len();
        self.ifs.push(r#if);
        InFnIfDef(def)
    }

    pub fn push_else(&mut self, r#else: InFnElse) -> InFnElseDef {
        let def = self.elses.len();
        self.elses.push(r#else);
        InFnElseDef(def)
    }

    pub fn push_for(&mut self, r#for: InFnFor) -> InFnForDef {
        let def = self.fors.len();
        self.fors.push(r#for);
        InFnForDef(def)
    }

    pub fn push_stmt(&mut self, stmt: InFnStmt) -> InFnStmtDef {
        let def = self.stmts.len();
        self.stmts.push(stmt);
        InFnStmtDef(def)
    }

    pub fn push_expr(&mut self, expr: InFnExpr) -> InFnExprDef {
        let def = self.exprs.len();
        self.exprs.push(expr);
        InFnExprDef(def)
    }

    pub fn push_lit(&mut self, lit: Literal) -> InFnLitDef {
        let def = self.lits.len();
        self.lits.push(lit);
        InFnLitDef(def)
    }
}

#[derive(Debug)]
pub struct InFnScope {
    pub module: ModuleDef,
    pub parent: Option<InFnScopeDef>,
    pub kind: InFnScopeKind,
}

#[derive(Debug, Copy, Clone)]
pub enum InFnScopeKind {
    Block,
    Struct(StructDef),
    Fn(FunctionDef),
    Let(InFnLetDef),
    For(InFnForDef),
}

#[derive(Debug)]
pub struct InFnBlock {
    pub stmts: Vec<InFnStmtDef>,
    pub span: Span,
}

#[derive(Debug)]
pub struct InFnLet {
    pub name: SymbolWithSpan,
    pub kind: InFnLetKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum InFnLetKind {
    Ty(TyRef),
    Expr(InFnExprDef),
    TyExpr(TyRef, InFnExprDef),
}

#[derive(Debug)]
pub struct InFnIf {
    pub cond: InFnExprDef,
    pub then_body: InFnBlockDef,
    pub else_kind: Option<InFnElseDef>,
    pub span: Span,
}

#[derive(Debug)]
pub struct InFnElse {
    pub kind: InFnElseKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum InFnElseKind {
    Else(InFnBlockDef),
    ElseIf(InFnIfDef),
}

#[derive(Debug)]
pub struct InFnFor {
    pub kind: InFnForKind,
    pub body: InFnBlockDef,
    pub span: Span,
}

#[derive(Debug)]
pub enum InFnForKind {
    Loop,
    While(InFnExprDef),
    ForIn(SymbolWithSpan, InFnExprDef),
}

#[derive(Debug)]
pub struct InFnStmt {
    pub scope: InFnScopeDef,
    pub kind: InFnStmtKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum InFnStmtKind {
    ScopeBegin(InFnScopeDef),
    ScopeEnd(InFnScopeDef),
    Let(InFnLetDef),
    Block(InFnBlockDef),
    If(InFnIfDef),
    For(InFnForDef),
    Break(InFnStmtBreak),
    Continue(InFnStmtContinue),
    Return(InFnStmtReturn),
    Assign(InFnStmtAssign),
    Expr(InFnExprDef),
}

#[derive(Debug)]
pub struct InFnStmtBreak {
    pub span: Span,
}

#[derive(Debug)]
pub struct InFnStmtContinue {
    pub span: Span,
}

#[derive(Debug)]
pub struct InFnStmtReturn {
    pub expr: Option<InFnExprDef>,
    pub span: Span,
}

#[derive(Debug)]
pub struct InFnStmtAssign {
    pub lhs: InFnExprDef,
    pub rhs: InFnExprDef,
    pub kind: InFnStmtAssignKind,
    pub span: Span,
}

#[derive(Debug, Copy, Clone)]
pub enum InFnStmtAssignKind {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shl,
    Shr,
    BitOr,
    BitAnd,
    BitXor,
    BitNot,
}

#[derive(Debug)]
pub struct InFnExpr {
    pub scope: InFnScopeDef,
    pub kind: InFnExprKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum InFnExprKind {
    Rng(InFnExprDef, InFnExprDef),
    RngInclusive(InFnExprDef, InFnExprDef),
    Eq(InFnExprDef, InFnExprDef),
    Ne(InFnExprDef, InFnExprDef),
    Lt(InFnExprDef, InFnExprDef),
    Gt(InFnExprDef, InFnExprDef),
    Le(InFnExprDef, InFnExprDef),
    Ge(InFnExprDef, InFnExprDef),
    Neg(InFnExprDef),
    Add(InFnExprDef, InFnExprDef),
    Sub(InFnExprDef, InFnExprDef),
    Mul(InFnExprDef, InFnExprDef),
    Div(InFnExprDef, InFnExprDef),
    Mod(InFnExprDef, InFnExprDef),
    Shl(InFnExprDef, InFnExprDef),
    Shr(InFnExprDef, InFnExprDef),
    BitOr(InFnExprDef, InFnExprDef),
    BitAnd(InFnExprDef, InFnExprDef),
    BitXor(InFnExprDef, InFnExprDef),
    BitNot(InFnExprDef),
    LogOr(InFnExprDef, InFnExprDef),
    LogAnd(InFnExprDef, InFnExprDef),
    LogNot(InFnExprDef),
    Cast(InFnExprDef, TyRef),
    Object(InFnObject),
    Call(InFnExprDef, Vec<InFnExprDef>),
    Index(InFnExprDef, InFnExprDef),
    Member(InFnExprDef, SymbolWithSpan),
    SizeOf(TyRef),
    AddrOf(InFnExprDef),
    TakeRef(InFnExprDef),
    Deref(InFnExprDef),
    Id(SymbolWithSpan),
    Literal(InFnLitDef),
}

#[derive(Debug)]
pub struct InFnObject {
    pub ty: TyRefUserDef,
    pub fields: Vec<InFnObjectField>,
    pub span: Span,
}

#[derive(Debug)]
pub struct InFnObjectField {
    pub name: SymbolWithSpan,
    pub kind: InFnObjectFieldKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum InFnObjectFieldKind {
    Expr(InFnExprDef),
    InnerObject(InFnInnerObject),
}

#[derive(Debug)]
pub struct InFnInnerObject {
    pub fields: Vec<InFnObjectField>,
    pub span: Span,
}
