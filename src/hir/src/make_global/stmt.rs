use crate::make_global::{
    make_global_expr, make_global_fn, make_global_struct, GlobalExpr, GlobalFn, GlobalFnHeader,
    GlobalInnerStruct, GlobalScope, GlobalScopeKind, GlobalStmt, GlobalStmtBlock, GlobalStmtBreak,
    GlobalStmtContinue, GlobalStmtElse, GlobalStmtElseKind, GlobalStmtFor, GlobalStmtForKind,
    GlobalStmtIf, GlobalStmtKind, GlobalStmtLet, GlobalStmtLetKind, GlobalStmtReturn, GlobalStruct,
};
use crate::{ScopeDef, ScopeRef, StmtDef, TyRef};
use ast::{Block, ElseKind, For, ForKind, If, Let, LetKind, Stmt, StmtKind};

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
    scope: ScopeRef,
    stmt: Stmt,
) -> MakeGlobalStmtResult {
    match stmt.kind {
        StmtKind::Block(block) => MakeGlobalStmtResult::Stmt(make_global_stmt_block(
            global_scopes,
            global_structs,
            global_inner_structs,
            global_fns,
            global_fn_headers,
            global_stmts,
            global_exprs,
            scope,
            block,
        )),
        StmtKind::Struct(r#struct) => {
            let new_scope = ScopeDef(global_scopes.len());
            let def = make_global_struct(
                global_structs,
                global_inner_structs,
                ScopeRef::Scope(new_scope),
                r#struct,
            );
            global_scopes.push(GlobalScope {
                module: match scope {
                    ScopeRef::Module(module) => module,
                    ScopeRef::Scope(def) => global_scopes[def.0].module,
                },
                parent: match scope {
                    ScopeRef::Module(..) => None,
                    ScopeRef::Scope(def) => Some(def),
                },
                kind: GlobalScopeKind::Struct(def),
            });
            MakeGlobalStmtResult::Scope(new_scope)
        }
        StmtKind::Fn(function) => {
            let new_scope = ScopeDef(global_scopes.len());
            let def = make_global_fn(
                global_scopes,
                global_structs,
                global_inner_structs,
                global_fns,
                global_fn_headers,
                global_stmts,
                global_exprs,
                ScopeRef::Scope(new_scope),
                function,
            );
            global_scopes.push(GlobalScope {
                module: match scope {
                    ScopeRef::Module(module) => module,
                    ScopeRef::Scope(def) => global_scopes[def.0].module,
                },
                parent: match scope {
                    ScopeRef::Module(..) => None,
                    ScopeRef::Scope(def) => Some(def),
                },
                kind: GlobalScopeKind::Fn(def),
            });
            MakeGlobalStmtResult::Scope(new_scope)
        }
        StmtKind::Let(r#let) => {
            let new_scope = ScopeDef(global_scopes.len());
            let def = make_global_stmt_let(
                global_stmts,
                global_exprs,
                ScopeRef::Scope(new_scope),
                r#let,
            );
            global_scopes.push(GlobalScope {
                module: match scope {
                    ScopeRef::Module(module) => module,
                    ScopeRef::Scope(def) => global_scopes[def.0].module,
                },
                parent: match scope {
                    ScopeRef::Module(..) => None,
                    ScopeRef::Scope(def) => Some(def),
                },
                kind: GlobalScopeKind::Let(def),
            });
            MakeGlobalStmtResult::ScopeStmt(new_scope, def)
        }
        StmtKind::If(r#if) => MakeGlobalStmtResult::Stmt(make_global_stmt_if(
            global_scopes,
            global_structs,
            global_inner_structs,
            global_fns,
            global_fn_headers,
            global_stmts,
            global_exprs,
            scope,
            r#if,
        )),
        StmtKind::For(r#for) => {
            let new_scope = ScopeDef(global_scopes.len());
            let def = make_global_stmt_for(
                global_scopes,
                global_structs,
                global_inner_structs,
                global_fns,
                global_fn_headers,
                global_stmts,
                global_exprs,
                ScopeRef::Scope(new_scope),
                r#for,
            );
            global_scopes.push(GlobalScope {
                module: match scope {
                    ScopeRef::Module(module) => module,
                    ScopeRef::Scope(def) => global_scopes[def.0].module,
                },
                parent: match scope {
                    ScopeRef::Module(..) => None,
                    ScopeRef::Scope(def) => Some(def),
                },
                kind: GlobalScopeKind::For(def),
            });
            MakeGlobalStmtResult::ScopeStmt(new_scope, def)
        }
        StmtKind::Break(r#break) => {
            let r#break = GlobalStmtBreak { span: r#break.span };
            let def = global_stmts.len();
            global_stmts.push(GlobalStmt {
                scope,
                span: r#break.span,
                kind: GlobalStmtKind::Break(r#break),
            });
            MakeGlobalStmtResult::Stmt(StmtDef(def))
        }
        StmtKind::Continue(r#continue) => {
            let r#continue = GlobalStmtContinue {
                span: r#continue.span,
            };
            let def = global_stmts.len();
            global_stmts.push(GlobalStmt {
                scope,
                span: r#continue.span,
                kind: GlobalStmtKind::Continue(r#continue),
            });
            MakeGlobalStmtResult::Stmt(StmtDef(def))
        }
        StmtKind::Return(r#return) => {
            let r#return = GlobalStmtReturn {
                expr: r#return
                    .expr
                    .map(|expr| make_global_expr(global_exprs, scope, expr)),
                span: r#return.span,
            };
            let def = global_stmts.len();
            global_stmts.push(GlobalStmt {
                scope,
                span: r#return.span,
                kind: GlobalStmtKind::Return(r#return),
            });
            MakeGlobalStmtResult::Stmt(StmtDef(def))
        }
        StmtKind::Expr(expr) => {
            let stmt = GlobalStmt {
                scope,
                span: expr.span,
                kind: GlobalStmtKind::Expr(make_global_expr(global_exprs, scope, expr)),
            };
            let def = global_stmts.len();
            global_stmts.push(stmt);
            MakeGlobalStmtResult::Stmt(StmtDef(def))
        }
    }
}

pub fn make_global_stmt_block(
    global_scopes: &mut Vec<GlobalScope>,
    global_structs: &mut Vec<GlobalStruct>,
    global_inner_structs: &mut Vec<GlobalInnerStruct>,
    global_fns: &mut Vec<GlobalFn>,
    global_fn_headers: &mut Vec<GlobalFnHeader>,
    global_stmts: &mut Vec<GlobalStmt>,
    global_exprs: &mut Vec<GlobalExpr>,
    mut scope: ScopeRef,
    block: Block,
) -> StmtDef {
    let mut stmts = Vec::with_capacity(block.stmts.len());

    for stmt in block.stmts {
        match make_global_stmt(
            global_scopes,
            global_structs,
            global_inner_structs,
            global_fns,
            global_fn_headers,
            global_stmts,
            global_exprs,
            scope,
            stmt,
        ) {
            MakeGlobalStmtResult::Scope(new_scope) => {
                scope = ScopeRef::Scope(new_scope);
            }
            MakeGlobalStmtResult::Stmt(def) => {
                stmts.push(def);
            }
            MakeGlobalStmtResult::ScopeStmt(new_scope, def) => {
                scope = ScopeRef::Scope(new_scope);
                stmts.push(def);
            }
        }
    }

    let block = GlobalStmtBlock {
        stmts,
        span: block.span,
    };
    let def = global_stmts.len();
    global_stmts.push(GlobalStmt {
        scope,
        span: block.span,
        kind: GlobalStmtKind::Block(block),
    });
    StmtDef(def)
}

fn make_global_stmt_let(
    global_stmts: &mut Vec<GlobalStmt>,
    global_exprs: &mut Vec<GlobalExpr>,
    scope: ScopeRef,
    r#let: Let,
) -> StmtDef {
    let r#let = GlobalStmtLet {
        name: r#let.name,
        kind: match r#let.kind {
            LetKind::Ty(ty) => GlobalStmtLetKind::Ty(TyRef { scope, ty }),
            LetKind::Expr(expr) => {
                GlobalStmtLetKind::Expr(make_global_expr(global_exprs, scope, expr))
            }
            LetKind::TyExpr(ty, expr) => GlobalStmtLetKind::TyExpr(
                TyRef { scope, ty },
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

fn make_global_stmt_if(
    global_scopes: &mut Vec<GlobalScope>,
    global_structs: &mut Vec<GlobalStruct>,
    global_inner_structs: &mut Vec<GlobalInnerStruct>,
    global_fns: &mut Vec<GlobalFn>,
    global_fn_headers: &mut Vec<GlobalFnHeader>,
    global_stmts: &mut Vec<GlobalStmt>,
    global_exprs: &mut Vec<GlobalExpr>,
    scope: ScopeRef,
    r#if: If,
) -> StmtDef {
    let r#if = GlobalStmtIf {
        cond: make_global_expr(global_exprs, scope, r#if.cond),
        then_body: make_global_stmt_block(
            global_scopes,
            global_structs,
            global_inner_structs,
            global_fns,
            global_fn_headers,
            global_stmts,
            global_exprs,
            scope,
            r#if.then_body,
        ),
        else_kind: r#if.else_kind.map(|else_kind| {
            let r#else = match *else_kind {
                ElseKind::Else(r#else) => GlobalStmtElse {
                    span: r#else.span,
                    kind: GlobalStmtElseKind::Else(make_global_stmt_block(
                        global_scopes,
                        global_structs,
                        global_inner_structs,
                        global_fns,
                        global_fn_headers,
                        global_stmts,
                        global_exprs,
                        scope,
                        r#else,
                    )),
                },
                ElseKind::ElseIf(else_if) => GlobalStmtElse {
                    span: else_if.span,
                    kind: GlobalStmtElseKind::ElseIf(make_global_stmt_if(
                        global_scopes,
                        global_structs,
                        global_inner_structs,
                        global_fns,
                        global_fn_headers,
                        global_stmts,
                        global_exprs,
                        scope,
                        else_if,
                    )),
                },
            };
            let def = global_stmts.len();
            global_stmts.push(GlobalStmt {
                scope,
                span: r#else.span,
                kind: GlobalStmtKind::Else(r#else),
            });
            StmtDef(def)
        }),
        span: r#if.span,
    };
    let def = global_stmts.len();
    global_stmts.push(GlobalStmt {
        scope,
        span: r#if.span,
        kind: GlobalStmtKind::If(r#if),
    });
    StmtDef(def)
}

fn make_global_stmt_for(
    global_scopes: &mut Vec<GlobalScope>,
    global_structs: &mut Vec<GlobalStruct>,
    global_inner_structs: &mut Vec<GlobalInnerStruct>,
    global_fns: &mut Vec<GlobalFn>,
    global_fn_headers: &mut Vec<GlobalFnHeader>,
    global_stmts: &mut Vec<GlobalStmt>,
    global_exprs: &mut Vec<GlobalExpr>,
    scope: ScopeRef,
    r#for: For,
) -> StmtDef {
    let r#for = GlobalStmtFor {
        kind: match r#for.kind {
            ForKind::Loop => GlobalStmtForKind::Loop,
            ForKind::While(expr) => {
                GlobalStmtForKind::While(make_global_expr(global_exprs, scope, expr))
            }
            ForKind::ForIn(ident, expr) => {
                GlobalStmtForKind::ForIn(ident, make_global_expr(global_exprs, scope, expr))
            }
        },
        body: make_global_stmt_block(
            global_scopes,
            global_structs,
            global_inner_structs,
            global_fns,
            global_fn_headers,
            global_stmts,
            global_exprs,
            scope,
            r#for.body,
        ),
        span: r#for.span,
    };
    let def = global_stmts.len();
    global_stmts.push(GlobalStmt {
        scope,
        span: r#for.span,
        kind: GlobalStmtKind::For(r#for),
    });
    StmtDef(def)
}
