use crate::{
    ty::{lookup_full, lookup_let, resolve_ty, Lookup, Ty},
    MirTy, MirTyKind, MirTySourceKind,
};
use hir::{
    GlobalContext, GlobalStructFieldKind, InFnContext, InFnExprDef, InFnExprKind, InFnLetKind,
    InFnScopeDef, ScopeRef,
};
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct BinaryOpTyMap {
    map: HashMap<BinaryOpKind, Vec<BinaryOpTy>>,
}

impl BinaryOpTyMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn search(&self, op: BinaryOpKind, lhs: Ty, rhs: Ty) -> Option<&BinaryOpTy> {
        let tys = if let Some(tys) = self.map.get(&op) {
            tys
        } else {
            return None;
        };

        for ty in tys {
            if ty.lhs == lhs && ty.rhs == rhs {
                return Some(ty);
            }
        }

        for ty in tys {
            if ty.lhs == lhs && ty.rhs == rhs {
                return Some(ty);
            }
        }

        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BinaryOpTy {
    pub kind: BinaryOpKind,
    pub lhs: Ty,
    pub rhs: Ty,
    pub return_ty: Ty,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOpKind {
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
}

pub fn deduce_expr(
    global_ctx: &GlobalContext,
    ctx: &InFnContext,
    scope: InFnScopeDef,
    binary_op_ty_map: &BinaryOpTyMap,
    expr: InFnExprDef,
) -> Ty {
    let ty = match &ctx.exprs[expr.0].kind {
        &InFnExprKind::Rng(lhs, rhs) => MirTy {
            kind: MirTyKind::Range(
                deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, lhs),
                deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, rhs),
            ),
            source_kind: None,
            is_ref: false,
        },
        &InFnExprKind::RngInclusive(lhs, rhs) => MirTy {
            kind: MirTyKind::RangeInclusive(
                deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, lhs),
                deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, rhs),
            ),
            source_kind: None,
            is_ref: false,
        },
        InFnExprKind::Eq(_, _)
        | InFnExprKind::Ne(_, _)
        | InFnExprKind::Lt(_, _)
        | InFnExprKind::Gt(_, _)
        | InFnExprKind::Le(_, _)
        | InFnExprKind::Ge(_, _) => MirTy {
            kind: MirTyKind::Bool,
            source_kind: None,
            is_ref: false,
        },
        &InFnExprKind::Neg(lhs) => {
            let ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, lhs);
            MirTy {
                kind: match &ty.as_ty().kind {
                    MirTyKind::None
                    | MirTyKind::Bool
                    | MirTyKind::Byte
                    | MirTyKind::Char
                    | MirTyKind::U64
                    | MirTyKind::Usize
                    | MirTyKind::Str
                    | MirTyKind::Range(..)
                    | MirTyKind::RangeInclusive(..)
                    | MirTyKind::Struct(..)
                    | MirTyKind::InnerStruct(..)
                    | MirTyKind::Fn(..) => MirTyKind::None,
                    kind @ _ => kind.clone(),
                },
                source_kind: None,
                is_ref: false,
            }
        }
        &InFnExprKind::Add(lhs, rhs) => {
            let lhs_ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, lhs);
            let rhs_ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, rhs);
            return binary_op_ty_map
                .search(BinaryOpKind::Add, lhs_ty, rhs_ty)
                .unwrap()
                .return_ty;
        }
        &InFnExprKind::Sub(lhs, rhs) => {
            let lhs_ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, lhs);
            let rhs_ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, rhs);
            return binary_op_ty_map
                .search(BinaryOpKind::Sub, lhs_ty, rhs_ty)
                .unwrap()
                .return_ty;
        }
        &InFnExprKind::Mul(lhs, rhs) => {
            let lhs_ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, lhs);
            let rhs_ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, rhs);
            return binary_op_ty_map
                .search(BinaryOpKind::Mul, lhs_ty, rhs_ty)
                .unwrap()
                .return_ty;
        }
        &InFnExprKind::Div(lhs, rhs) => {
            let lhs_ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, lhs);
            let rhs_ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, rhs);
            return binary_op_ty_map
                .search(BinaryOpKind::Div, lhs_ty, rhs_ty)
                .unwrap()
                .return_ty;
        }
        &InFnExprKind::Mod(lhs, rhs) => {
            let lhs_ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, lhs);
            let rhs_ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, rhs);
            return binary_op_ty_map
                .search(BinaryOpKind::Mod, lhs_ty, rhs_ty)
                .unwrap()
                .return_ty;
        }
        &InFnExprKind::Shl(lhs, rhs) => {
            let lhs_ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, lhs);
            let rhs_ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, rhs);
            return binary_op_ty_map
                .search(BinaryOpKind::Shl, lhs_ty, rhs_ty)
                .unwrap()
                .return_ty;
        }
        &InFnExprKind::Shr(lhs, rhs) => {
            let lhs_ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, lhs);
            let rhs_ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, rhs);
            return binary_op_ty_map
                .search(BinaryOpKind::Shr, lhs_ty, rhs_ty)
                .unwrap()
                .return_ty;
        }
        &InFnExprKind::BitOr(lhs, rhs) => {
            let lhs_ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, lhs);
            let rhs_ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, rhs);
            return binary_op_ty_map
                .search(BinaryOpKind::BitOr, lhs_ty, rhs_ty)
                .unwrap()
                .return_ty;
        }
        &InFnExprKind::BitAnd(lhs, rhs) => {
            let lhs_ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, lhs);
            let rhs_ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, rhs);
            return binary_op_ty_map
                .search(BinaryOpKind::BitAnd, lhs_ty, rhs_ty)
                .unwrap()
                .return_ty;
        }
        &InFnExprKind::BitXor(lhs, rhs) => {
            let lhs_ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, lhs);
            let rhs_ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, rhs);
            return binary_op_ty_map
                .search(BinaryOpKind::BitXor, lhs_ty, rhs_ty)
                .unwrap()
                .return_ty;
        }
        &InFnExprKind::BitNot(lhs) => {
            let ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, lhs);
            MirTy {
                kind: match &ty.as_ty().kind {
                    MirTyKind::None
                    | MirTyKind::F64
                    | MirTyKind::Str
                    | MirTyKind::Range(..)
                    | MirTyKind::RangeInclusive(..)
                    | MirTyKind::Struct(..)
                    | MirTyKind::InnerStruct(..)
                    | MirTyKind::Fn(..) => MirTyKind::None,
                    kind @ _ => kind.clone(),
                },
                source_kind: None,
                is_ref: false,
            }
        }
        InFnExprKind::LogOr(_, _) | InFnExprKind::LogAnd(_, _) | InFnExprKind::LogNot(_) => MirTy {
            kind: MirTyKind::Bool,
            source_kind: None,
            is_ref: false,
        },
        InFnExprKind::Cast(_, rhs) => {
            return resolve_ty(global_ctx, Some(ctx), binary_op_ty_map, rhs, false).unwrap();
        }
        InFnExprKind::Object(lhs) => {
            match lookup_full(
                global_ctx,
                Some(ctx),
                ScopeRef {
                    module: ctx.module,
                    scope: Some(scope),
                },
                &lhs.ty.ty,
                true,
            )
            .unwrap()
            {
                Lookup::Struct(_) => todo!(),
                _ => panic!("no struct found"),
            }
        }
        InFnExprKind::Call(lhs, _) => {
            let ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, *lhs);

            if let MirTyKind::Fn(r#fn) = &ty.as_ty().kind {
                if let Some(ty) = r#fn.return_ty {
                    return ty;
                }
            }

            MirTy {
                kind: MirTyKind::None,
                source_kind: None,
                is_ref: false,
            }
        }
        InFnExprKind::Index(_, _) => todo!(),
        &InFnExprKind::Member(lhs, rhs) => {
            let ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, lhs);

            match ty.as_ty().kind {
                MirTyKind::Struct(def) => {
                    let fields = &global_ctx.structs[def.0].fields;
                    if let Some(position) = fields.iter().position(|field| field.name == rhs) {
                        match &fields[position].kind {
                            GlobalStructFieldKind::Plain(ty) => {
                                return resolve_ty(
                                    global_ctx,
                                    Some(ctx),
                                    binary_op_ty_map,
                                    ty,
                                    false,
                                )
                                .unwrap();
                            }
                            &GlobalStructFieldKind::Struct(def) => MirTy {
                                kind: MirTyKind::InnerStruct(def),
                                source_kind: None,
                                is_ref: ty.as_ty().is_ref,
                            },
                        }
                    } else {
                        MirTy {
                            kind: MirTyKind::None,
                            source_kind: None,
                            is_ref: ty.as_ty().is_ref,
                        }
                    }
                }
                MirTyKind::InnerStruct(def) => {
                    let fields = &global_ctx.structs[def.0].fields;
                    if let Some(position) = fields.iter().position(|field| field.name == rhs) {
                        match &fields[position].kind {
                            GlobalStructFieldKind::Plain(ty) => {
                                return resolve_ty(
                                    global_ctx,
                                    Some(ctx),
                                    binary_op_ty_map,
                                    ty,
                                    false,
                                )
                                .unwrap();
                            }
                            &GlobalStructFieldKind::Struct(def) => MirTy {
                                kind: MirTyKind::InnerStruct(def),
                                source_kind: None,
                                is_ref: ty.as_ty().is_ref,
                            },
                        }
                    } else {
                        MirTy {
                            kind: MirTyKind::None,
                            source_kind: None,
                            is_ref: ty.as_ty().is_ref,
                        }
                    }
                }
                _ => MirTy {
                    kind: MirTyKind::None,
                    source_kind: None,
                    is_ref: false,
                },
            }
        }
        InFnExprKind::SizeOf(_) => MirTy {
            kind: MirTyKind::Usize,
            source_kind: None,
            is_ref: false,
        },
        InFnExprKind::AddrOf(lhs) => {
            let ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, *lhs);
            MirTy {
                kind: if ty.as_ty().source_kind.is_none() {
                    MirTyKind::None
                } else {
                    MirTyKind::Ptr(ty)
                },
                source_kind: None,
                is_ref: false,
            }
        }
        InFnExprKind::TakeRef(lhs) => {
            let ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, *lhs);
            MirTy {
                kind: ty.as_ty().kind.clone(),
                source_kind: None,
                is_ref: true,
            }
        }
        InFnExprKind::Deref(lhs) => {
            let ty = deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, *lhs);
            MirTy {
                kind: if let MirTyKind::Ptr(inner_ty) = &ty.as_ty().kind {
                    inner_ty.as_ty().kind.clone()
                } else {
                    MirTyKind::None
                },
                source_kind: Some(MirTySourceKind::Deref),
                is_ref: false,
            }
        }
        InFnExprKind::Id(id) => {
            return match lookup_let(global_ctx, ctx, scope, id.symbol).unwrap() {
                Lookup::Local(def) => match &ctx.lets[def.0].kind {
                    InFnLetKind::Ty(ty) => {
                        resolve_ty(global_ctx, Some(ctx), binary_op_ty_map, ty, false).unwrap()
                    }
                    &InFnLetKind::Expr(def) => {
                        deduce_expr(global_ctx, ctx, scope, binary_op_ty_map, def)
                    }
                    InFnLetKind::TyExpr(ty, _) => {
                        resolve_ty(global_ctx, Some(ctx), binary_op_ty_map, ty, false).unwrap()
                    }
                },
                _ => unreachable!(),
            }
        }
        InFnExprKind::Literal(_) => todo!(),
    };

    new_ty! { ty }
}
