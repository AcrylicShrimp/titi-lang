use crate::{make_global::GlobalStmt, ScopeDef};
use crate::{ScopeRef, StmtDef, TyRef};
use ast::{Let, LetKind, Stmt, StmtKind};

use super::{
    make_global_expr, make_global_fn, make_global_struct, GlobalExpr, GlobalFn, GlobalFnHeader,
    GlobalInnerStruct, GlobalScope, GlobalScopeKind, GlobalStmtKind, GlobalStmtLet,
    GlobalStmtLetKind, GlobalStruct,
};

pub enum MakeGlobalStmtResult {
    Scope(ScopeDef),
    Stmt(StmtDef),
    ScopeStmt(ScopeDef, StmtDef),
}

pub fn make_global_stmt(
    global_scopes: &mut Vec<GlobalScope>,
    global_structs: &mut Vec<GlobalStruct>,
    global_inner_structs: &mut Vec<GlobalInnerStruct>,
    global_fns: &mut Vec<GlobalFn>,
    global_fn_headers: &mut Vec<GlobalFnHeader>,
    global_stmts: &mut Vec<GlobalStmt>,
    global_exprs: &mut Vec<GlobalExpr>,
    scope: ScopeDef,
    stmt: Stmt,
) -> MakeGlobalStmtResult {
    let stmt = GlobalStmt {
        scope,
        kind: match stmt.kind {
            StmtKind::Struct(r#struct) => {
                let new_scope = ScopeDef(global_scopes.len());
                let def = make_global_struct(
                    global_structs,
                    global_inner_structs,
                    ScopeRef::Scope(new_scope),
                    r#struct,
                );
                global_scopes.push(GlobalScope {
                    module: global_scopes[def.0].module,
                    parent: Some(scope),
                    kind: GlobalScopeKind::Struct(def),
                });
                return MakeGlobalStmtResult::Scope(new_scope);
            }
            StmtKind::Fn(function) => {
                let new_scope = ScopeDef(global_scopes.len());
                let def = make_global_fn(
                    global_fns,
                    global_fn_headers,
                    ScopeRef::Scope(new_scope),
                    function,
                );
                global_scopes.push(GlobalScope {
                    module: global_scopes[def.0].module,
                    parent: Some(scope),
                    kind: GlobalScopeKind::Fn(def),
                });
                return MakeGlobalStmtResult::Scope(new_scope);
            }
            StmtKind::Let(r#let) => {
                let new_scope = ScopeDef(global_scopes.len());
                let def = make_global_stmt_let(global_stmts, global_exprs, new_scope, r#let);
                global_scopes.push(GlobalScope {
                    module: global_scopes[def.0].module,
                    parent: Some(scope),
                    kind: GlobalScopeKind::Let(def),
                });
                return MakeGlobalStmtResult::ScopeStmt(new_scope, def);
            }
            StmtKind::If(r#if) => todo!(),
            StmtKind::For(_) => todo!(),
            StmtKind::Block(_) => todo!(),
            StmtKind::Break(_) => todo!(),
            StmtKind::Continue(_) => todo!(),
            StmtKind::Return(_) => todo!(),
            StmtKind::Expr(_) => todo!(),
        },
        span: stmt.span,
    };
    let def = global_stmts.len();
    global_stmts.push(stmt);
    MakeGlobalStmtResult::Stmt(StmtDef(def))
}

fn make_global_stmt_let(
    global_stmts: &mut Vec<GlobalStmt>,
    global_exprs: &mut Vec<GlobalExpr>,
    scope: ScopeDef,
    r#let: Let,
) -> StmtDef {
    let r#let = GlobalStmtLet {
        name: r#let.name,
        kind: match r#let.kind {
            LetKind::Ty(ty) => GlobalStmtLetKind::Ty(TyRef {
                scope: ScopeRef::Scope(scope),
                ty,
            }),
            LetKind::Expr(expr) => {
                GlobalStmtLetKind::Expr(make_global_expr(global_exprs, scope, expr))
            }
            LetKind::TyExpr(ty, expr) => GlobalStmtLetKind::TyExpr(
                TyRef {
                    scope: ScopeRef::Scope(scope),
                    ty,
                },
                make_global_expr(global_exprs, scope, expr),
            ),
        },
        span: r#let.span,
    };
    let def = global_stmts.len();
    global_stmts.push(GlobalStmt {
        scope,
        span: r#let.span,
        kind: GlobalStmtKind::Let(r#let),
    });
    StmtDef(def)
}
