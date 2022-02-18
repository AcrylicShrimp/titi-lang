use super::{
    expr::make_in_fn_expr, InFnBlock, InFnBlockDef, InFnContext, InFnElse, InFnElseDef,
    InFnElseKind, InFnFor, InFnForDef, InFnForKind, InFnIf, InFnIfDef, InFnLet, InFnLetDef,
    InFnLetKind, InFnScope, InFnScopeDef, InFnScopeKind, InFnStmt, InFnStmtBreak, InFnStmtContinue,
    InFnStmtDef, InFnStmtKind, InFnStmtReturn,
};
use crate::{
    make_global::{function::make_global_fn, r#struct::make_global_struct},
    GlobalContextWithoutModule, InFnStmtAssign, InFnStmtAssignKind, ScopeRef, TyRef,
};
use ast::{AssignKind, Block, ElseKind, For, ForKind, If, Let, LetKind, Stmt, StmtKind};

pub fn make_in_fn_stmt(
    global_ctx: &mut GlobalContextWithoutModule,
    ctx: &mut InFnContext,
    scope: InFnScopeDef,
    stmt: Stmt,
) -> InFnStmtDef {
    match stmt.kind {
        StmtKind::Block(block) => {
            let block = make_in_fn_block(global_ctx, ctx, scope, block);
            ctx.push_stmt(InFnStmt {
                scope,
                kind: InFnStmtKind::Block(block),
                span: stmt.span,
            })
        }
        StmtKind::Struct(r#struct) => {
            let new_scope = InFnScopeDef(ctx.scopes.len());
            let def = make_global_struct(
                global_ctx,
                ScopeRef {
                    module: ctx.module,
                    function: Some(ctx.function),
                    scope: Some(new_scope),
                },
                r#struct,
            );
            ctx.scopes.push(InFnScope {
                module: ctx.module,
                parent: Some(scope),
                kind: InFnScopeKind::Struct(def),
            });
            ctx.push_stmt(InFnStmt {
                scope: new_scope,
                kind: InFnStmtKind::ScopeBegin(new_scope),
                span: stmt.span,
            })
        }
        StmtKind::Fn(function) => {
            let new_scope = InFnScopeDef(ctx.scopes.len());
            let def = make_global_fn(
                global_ctx,
                ScopeRef {
                    module: ctx.module,
                    function: Some(ctx.function),
                    scope: Some(new_scope),
                },
                function,
            );
            ctx.scopes.push(InFnScope {
                module: ctx.module,
                parent: Some(scope),
                kind: InFnScopeKind::Fn(def),
            });
            ctx.push_stmt(InFnStmt {
                scope: new_scope,
                kind: InFnStmtKind::ScopeBegin(new_scope),
                span: stmt.span,
            })
        }
        StmtKind::Let(r#let) => {
            let new_scope = InFnScopeDef(ctx.scopes.len());
            let def = make_in_fn_let(ctx, new_scope, r#let);
            ctx.scopes.push(InFnScope {
                module: ctx.module,
                parent: Some(scope),
                kind: InFnScopeKind::Let(def),
            });
            ctx.push_stmt(InFnStmt {
                scope: new_scope,
                kind: InFnStmtKind::ScopeBegin(new_scope),
                span: stmt.span,
            })
        }
        StmtKind::If(r#if) => {
            let r#if = make_in_fn_if(global_ctx, ctx, scope, r#if);
            ctx.push_stmt(InFnStmt {
                scope,
                kind: InFnStmtKind::If(r#if),
                span: stmt.span,
            })
        }
        StmtKind::For(r#for) => {
            let new_scope = InFnScopeDef(ctx.scopes.len());
            let def = make_in_fn_for(global_ctx, ctx, new_scope, r#for);
            ctx.scopes.push(InFnScope {
                module: ctx.module,
                parent: Some(scope),
                kind: InFnScopeKind::For(def),
            });
            ctx.push_stmt(InFnStmt {
                scope: new_scope,
                kind: InFnStmtKind::ScopeBegin(new_scope),
                span: stmt.span,
            })
        }
        StmtKind::Break(r#break) => {
            let r#break = InFnStmtBreak { span: r#break.span };
            ctx.push_stmt(InFnStmt {
                scope,
                kind: InFnStmtKind::Break(r#break),
                span: stmt.span,
            })
        }
        StmtKind::Continue(r#continue) => {
            let r#continue = InFnStmtContinue {
                span: r#continue.span,
            };
            ctx.push_stmt(InFnStmt {
                scope,
                kind: InFnStmtKind::Continue(r#continue),
                span: stmt.span,
            })
        }
        StmtKind::Return(r#return) => {
            let r#return = InFnStmtReturn {
                expr: r#return.expr.map(|expr| make_in_fn_expr(ctx, scope, expr)),
                span: r#return.span,
            };
            ctx.push_stmt(InFnStmt {
                scope,
                kind: InFnStmtKind::Return(r#return),
                span: stmt.span,
            })
        }
        StmtKind::Assign(assign) => {
            let assign = InFnStmtAssign {
                lhs: make_in_fn_expr(ctx, scope, assign.lhs),
                rhs: make_in_fn_expr(ctx, scope, assign.rhs),
                kind: match assign.kind {
                    AssignKind::Assign => InFnStmtAssignKind::Assign,
                    AssignKind::Add => InFnStmtAssignKind::Add,
                    AssignKind::Sub => InFnStmtAssignKind::Sub,
                    AssignKind::Mul => InFnStmtAssignKind::Mul,
                    AssignKind::Div => InFnStmtAssignKind::Div,
                    AssignKind::Mod => InFnStmtAssignKind::Mod,
                    AssignKind::Shl => InFnStmtAssignKind::Shl,
                    AssignKind::Shr => InFnStmtAssignKind::Shr,
                    AssignKind::BitOr => InFnStmtAssignKind::BitOr,
                    AssignKind::BitAnd => InFnStmtAssignKind::BitAnd,
                    AssignKind::BitXor => InFnStmtAssignKind::BitXor,
                    AssignKind::BitNot => InFnStmtAssignKind::BitNot,
                },
                span: assign.span,
            };
            ctx.push_stmt(InFnStmt {
                scope,
                kind: InFnStmtKind::Assign(assign),
                span: stmt.span,
            })
        }
        StmtKind::Expr(expr) => {
            let stmt = InFnStmt {
                scope,
                span: expr.span,
                kind: InFnStmtKind::Expr(make_in_fn_expr(ctx, scope, expr)),
            };
            ctx.push_stmt(stmt)
        }
    }
}

pub fn make_in_fn_block(
    global_ctx: &mut GlobalContextWithoutModule,
    ctx: &mut InFnContext,
    mut scope: InFnScopeDef,
    block: Block,
) -> InFnBlockDef {
    let old_scope = scope;
    let mut stmts = Vec::with_capacity(block.stmts.len() + 2);

    stmts.push({
        let new_scope = InFnScopeDef(ctx.scopes.len());
        let def = ctx.stmts.len();
        ctx.stmts.push(InFnStmt {
            scope,
            kind: InFnStmtKind::ScopeBegin(new_scope),
            span: block.span,
        });
        ctx.scopes.push(InFnScope {
            module: ctx.module,
            parent: Some(scope),
            kind: InFnScopeKind::Block,
        });
        scope = new_scope;
        InFnStmtDef(def)
    });

    for stmt in block.stmts {
        let def = make_in_fn_stmt(global_ctx, ctx, scope, stmt);
        match &ctx.stmts[def.0].kind {
            &InFnStmtKind::ScopeBegin(new_scope) => {
                scope = new_scope;
            }
            &InFnStmtKind::ScopeEnd(old_scope) => {
                scope = old_scope;
            }
            _ => {}
        }
    }

    stmts.push({
        let def = ctx.stmts.len();
        ctx.stmts.push(InFnStmt {
            scope,
            kind: InFnStmtKind::ScopeEnd(old_scope),
            span: block.span,
        });
        InFnStmtDef(def)
    });

    let block = InFnBlock {
        stmts,
        span: block.span,
    };
    let def = ctx.blocks.len();
    ctx.blocks.push(block);
    InFnBlockDef(def)
}

fn make_in_fn_let(ctx: &mut InFnContext, scope: InFnScopeDef, r#let: Let) -> InFnLetDef {
    let r#let = InFnLet {
        name: r#let.name,
        kind: match r#let.kind {
            LetKind::Ty(ty) => InFnLetKind::Ty(TyRef {
                scope: ScopeRef {
                    module: ctx.module,
                    function: Some(ctx.function),
                    scope: Some(scope),
                },
                ty,
            }),
            LetKind::Expr(expr) => InFnLetKind::Expr(make_in_fn_expr(ctx, scope, expr)),
            LetKind::TyExpr(ty, expr) => InFnLetKind::TyExpr(
                TyRef {
                    scope: ScopeRef {
                        module: ctx.module,
                        function: Some(ctx.function),
                        scope: Some(scope),
                    },
                    ty,
                },
                make_in_fn_expr(ctx, scope, expr),
            ),
        },
        span: r#let.span,
    };
    let def = ctx.lets.len();
    ctx.lets.push(r#let);
    InFnLetDef(def)
}

fn make_in_fn_if(
    global_ctx: &mut GlobalContextWithoutModule,
    ctx: &mut InFnContext,
    scope: InFnScopeDef,
    r#if: If,
) -> InFnIfDef {
    let r#if = InFnIf {
        cond: make_in_fn_expr(ctx, scope, r#if.cond),
        then_body: make_in_fn_block(global_ctx, ctx, scope, r#if.then_body),
        else_kind: r#if.else_kind.map(|else_kind| {
            let r#else = match *else_kind {
                ElseKind::Else(r#else) => InFnElse {
                    span: r#else.span,
                    kind: InFnElseKind::Else(make_in_fn_block(global_ctx, ctx, scope, r#else)),
                },
                ElseKind::ElseIf(else_if) => InFnElse {
                    span: else_if.span,
                    kind: InFnElseKind::ElseIf(make_in_fn_if(global_ctx, ctx, scope, else_if)),
                },
            };
            let def = ctx.elses.len();
            ctx.elses.push(r#else);
            InFnElseDef(def)
        }),
        span: r#if.span,
    };
    let def = ctx.ifs.len();
    ctx.ifs.push(r#if);
    InFnIfDef(def)
}

fn make_in_fn_for(
    global_ctx: &mut GlobalContextWithoutModule,
    ctx: &mut InFnContext,
    scope: InFnScopeDef,
    r#for: For,
) -> InFnForDef {
    let r#for = InFnFor {
        kind: match r#for.kind {
            ForKind::Loop => InFnForKind::Loop,
            ForKind::While(expr) => InFnForKind::While(make_in_fn_expr(ctx, scope, expr)),
            ForKind::ForIn(ident, expr) => {
                InFnForKind::ForIn(ident, make_in_fn_expr(ctx, scope, expr))
            }
        },
        body: make_in_fn_block(global_ctx, ctx, scope, r#for.body),
        span: r#for.span,
    };
    let def = ctx.fors.len();
    ctx.fors.push(r#for);
    InFnForDef(def)
}
