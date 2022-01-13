use super::{make_in_fn_object, InFnContext, InFnExpr, InFnExprDef, InFnExprKind, InFnScopeDef};
use crate::{ScopeRef, TyRef};
use ast::{Expr, ExprKind};

macro_rules! make_in_fn_expr_impl {
    ($expr_ty:expr, $lhs:ident => $ctx:ident, $scope:ident, $expr:expr) => {
        let lhs = make_in_fn_expr($ctx, $scope, *$lhs);
        $ctx.push_expr(InFnExpr {
            $scope,
            kind: $expr_ty(lhs),
            span: $expr.span,
        })
    };
    ($expr_ty:expr, $lhs:ident, $rhs:ident => $ctx:ident, $scope:ident, $expr:expr) => {
        let lhs = make_in_fn_expr($ctx, $scope, *$lhs);
        let rhs = make_in_fn_expr($ctx, $scope, *$rhs);
        $ctx.push_expr(InFnExpr {
            $scope,
            kind: $expr_ty(lhs, rhs),
            span: $expr.span,
        })
    };
}

pub fn make_in_fn_expr(ctx: &mut InFnContext, scope: InFnScopeDef, expr: Expr) -> InFnExprDef {
    match expr.kind {
        ExprKind::Assign(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::Assign, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::AssignAdd(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::AssignAdd, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::AssignSub(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::AssignSub, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::AssignMul(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::AssignMul, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::AssignDiv(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::AssignDiv, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::AssignMod(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::AssignMod, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::AssignShl(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::AssignShl, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::AssignShr(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::AssignShr, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::AssignBitOr(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::AssignBitOr, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::AssignBitAnd(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::AssignBitAnd, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::AssignBitXor(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::AssignBitXor, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::AssignBitNot(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::AssignBitNot, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::Rng(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::Rng, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::RngInclusive(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::RngInclusive, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::Eq(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::Eq, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::Ne(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::Ne, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::Lt(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::Lt, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::Gt(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::Gt, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::Le(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::Le, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::Ge(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::Ge, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::Neg(lhs) => {
            make_in_fn_expr_impl! {InFnExprKind::Neg, lhs => ctx, scope, expr}
        }
        ExprKind::Add(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::Add, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::Sub(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::Sub, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::Mul(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::Mul, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::Div(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::Div, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::Mod(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::Mod, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::Shl(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::Shl, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::Shr(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::Shr, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::BitOr(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::BitOr, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::BitAnd(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::BitAnd, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::BitXor(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::BitXor, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::BitNot(lhs) => {
            make_in_fn_expr_impl! {InFnExprKind::BitNot, lhs => ctx, scope, expr}
        }
        ExprKind::LogOr(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::LogOr, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::LogAnd(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::LogAnd, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::LogNot(lhs) => {
            make_in_fn_expr_impl! {InFnExprKind::LogNot, lhs => ctx, scope, expr}
        }
        ExprKind::Cast(lhs, rhs) => {
            let lhs = make_in_fn_expr(ctx, scope, *lhs);
            ctx.push_expr(InFnExpr {
                scope,
                kind: InFnExprKind::Cast(
                    lhs,
                    TyRef {
                        scope: ScopeRef {
                            module: ctx.module,
                            scope: Some(scope),
                        },
                        ty: rhs,
                    },
                ),
                span: expr.span,
            })
        }
        ExprKind::Object(lhs) => {
            let lhs = make_in_fn_object(ctx, scope, lhs);
            ctx.push_expr(InFnExpr {
                scope,
                kind: InFnExprKind::Object(lhs),
                span: expr.span,
            })
        }
        ExprKind::Call(lhs, rhs) => {
            let lhs = make_in_fn_expr(ctx, scope, *lhs);
            let rhs = rhs
                .into_iter()
                .map(|expr| make_in_fn_expr(ctx, scope, expr))
                .collect();
            ctx.push_expr(InFnExpr {
                scope,
                kind: InFnExprKind::Call(lhs, rhs),
                span: expr.span,
            })
        }
        ExprKind::Index(lhs, rhs) => {
            make_in_fn_expr_impl! {InFnExprKind::Index, lhs, rhs => ctx, scope, expr}
        }
        ExprKind::Member(lhs, rhs) => {
            let lhs = make_in_fn_expr(ctx, scope, *lhs);
            ctx.push_expr(InFnExpr {
                scope,
                kind: InFnExprKind::Member(lhs, rhs),
                span: expr.span,
            })
        }
        ExprKind::SizeOf(lhs) => ctx.push_expr(InFnExpr {
            scope,
            kind: InFnExprKind::SizeOf(TyRef {
                scope: ScopeRef {
                    module: ctx.module,
                    scope: Some(scope),
                },
                ty: lhs,
            }),
            span: expr.span,
        }),
        ExprKind::AddrOf(lhs) => {
            make_in_fn_expr_impl! {InFnExprKind::AddrOf, lhs => ctx, scope, expr}
        }
        ExprKind::Deref(lhs) => {
            make_in_fn_expr_impl! {InFnExprKind::Deref, lhs => ctx, scope, expr}
        }
        ExprKind::Id(lhs) => ctx.push_expr(InFnExpr {
            scope,
            kind: InFnExprKind::Id(lhs),
            span: expr.span,
        }),
        ExprKind::Literal(lhs) => {
            let lit = ctx.push_lit(lhs);
            ctx.push_expr(InFnExpr {
                scope,
                kind: InFnExprKind::Literal(lit),
                span: expr.span,
            })
        }
    }
}
