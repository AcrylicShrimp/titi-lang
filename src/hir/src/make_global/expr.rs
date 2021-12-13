use crate::make_global::{make_global_object, GlobalExpr, GlobalExprKind};
use crate::{ExprDef, ScopeDef, ScopeRef, TyRef};
use ast::{Expr, ExprKind};

macro_rules! make_global_expr_impl {
    ($expr_ty:expr, $lhs:ident => $global_exprs:ident, $scope:ident, $expr:expr) => {
        let lhs = make_global_expr($global_exprs, $scope, *$lhs);
        let def = $global_exprs.len();
        $global_exprs.push(GlobalExpr {
            $scope,
            kind: $expr_ty(lhs),
            span: $expr.span,
        });
        def
    };
    ($expr_ty:expr, $lhs:ident, $rhs:ident => $global_exprs:ident, $scope:ident, $expr:expr) => {
        let lhs = make_global_expr($global_exprs, $scope, *$lhs);
        let rhs = make_global_expr($global_exprs, $scope, *$rhs);
        let def = $global_exprs.len();
        $global_exprs.push(GlobalExpr {
            $scope,
            kind: $expr_ty(lhs, rhs),
            span: $expr.span,
        });
        def
    };
}

pub fn make_global_expr(
    global_exprs: &mut Vec<GlobalExpr>,
    scope: ScopeDef,
    expr: Expr,
) -> ExprDef {
    let def = match expr.kind {
        ExprKind::Assign(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::Assign, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::AssignAdd(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::AssignAdd, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::AssignSub(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::AssignSub, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::AssignMul(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::AssignMul, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::AssignDiv(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::AssignDiv, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::AssignMod(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::AssignMod, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::AssignShl(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::AssignShl, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::AssignShr(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::AssignShr, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::AssignBitOr(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::AssignBitOr, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::AssignBitAnd(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::AssignBitAnd, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::AssignBitXor(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::AssignBitXor, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::AssignBitNot(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::AssignBitNot, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::Rng(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::Rng, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::RngInclusive(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::RngInclusive, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::Eq(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::Eq, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::Ne(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::Ne, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::Lt(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::Lt, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::Gt(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::Gt, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::Le(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::Le, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::Ge(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::Ge, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::Neg(lhs) => {
            make_global_expr_impl! {GlobalExprKind::Neg, lhs => global_exprs, scope, expr}
        }
        ExprKind::Add(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::Add, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::Sub(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::Sub, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::Mul(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::Mul, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::Div(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::Div, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::Mod(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::Mod, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::Shl(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::Shl, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::Shr(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::Shr, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::BitOr(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::BitOr, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::BitAnd(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::BitAnd, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::BitXor(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::BitXor, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::BitNot(lhs) => {
            make_global_expr_impl! {GlobalExprKind::BitNot, lhs => global_exprs, scope, expr}
        }
        ExprKind::LogOr(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::LogOr, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::LogAnd(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::LogAnd, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::LogNot(lhs) => {
            make_global_expr_impl! {GlobalExprKind::LogNot, lhs => global_exprs, scope, expr}
        }
        ExprKind::Cast(lhs, rhs) => {
            let lhs = make_global_expr(global_exprs, scope, *lhs);
            let def = global_exprs.len();
            global_exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Cast(
                    lhs,
                    TyRef {
                        scope: ScopeRef::Scope(scope),
                        ty: rhs,
                    },
                ),
                span: expr.span,
            });
            def
        }
        ExprKind::Object(object) => {
            let lhs = make_global_object(global_exprs, scope, object);
            let def = global_exprs.len();
            global_exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Object(lhs),
                span: expr.span,
            });
            def
        }
        ExprKind::Call(lhs, rhs) => {
            let lhs = make_global_expr(global_exprs, scope, *lhs);
            let rhs = rhs
                .into_iter()
                .map(|expr| make_global_expr(global_exprs, scope, expr))
                .collect();
            let def = global_exprs.len();
            global_exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Call(lhs, rhs),
                span: expr.span,
            });
            def
        }
        ExprKind::Index(lhs, rhs) => {
            make_global_expr_impl! {GlobalExprKind::Index, lhs, rhs => global_exprs, scope, expr}
        }
        ExprKind::Member(lhs, rhs) => {
            let lhs = make_global_expr(global_exprs, scope, *lhs);
            let def = global_exprs.len();
            global_exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Member(lhs, rhs),
                span: expr.span,
            });
            def
        }
        ExprKind::Deref(lhs) => {
            make_global_expr_impl! {GlobalExprKind::Deref, lhs => global_exprs, scope, expr}
        }
        ExprKind::Id(lhs) => {
            let def = global_exprs.len();
            global_exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Id(lhs),
                span: expr.span,
            });
            def
        }
        ExprKind::Literal(lhs) => {
            let def = global_exprs.len();
            global_exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Literal(lhs),
                span: expr.span,
            });
            def
        }
    };

    ExprDef(def)
}
