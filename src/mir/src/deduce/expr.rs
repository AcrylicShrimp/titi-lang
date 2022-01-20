use crate::{MirContext, MirStructFieldKind, MirTy, MirTyDef, MirTyKind, MirTyRefKind};
use hir::{GlobalContext, InFnContext, InFnExprDef, InFnExprKind, InFnScopeDef, ScopeRef};
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct BinaryOpTyMap {
    pub map: HashMap<BinaryOpKind, Vec<BinaryOpTy>>,
}

impl BinaryOpTyMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn search(&self, op: BinaryOpKind, lhs: &MirTy, rhs: &MirTy) -> Option<&BinaryOpTy> {
        let tys = if let Some(tys) = self.map.get(&op) {
            tys
        } else {
            return None;
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BinaryOpTy {
    pub kind: BinaryOpKind,
    pub lhs: MirTyDef,
    pub rhs: MirTyDef,
    pub return_ty: MirTyDef,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

pub fn deduce_expr(
    global_ctx: &GlobalContext,
    ctx: &InFnContext,
    scope: InFnScopeDef,
    mir_ctx: &mut MirContext,
    expr_ty_cache: &mut HashMap<InFnExprDef, MirTyDef>,
    expr: InFnExprDef,
) -> MirTyDef {
    if let Some(ty) = expr_ty_cache.get(&expr) {
        return *ty;
    }

    let ty = match &ctx.exprs[expr.0].kind {
        &InFnExprKind::Rng(lhs, rhs) => MirTy {
            temporary: true,
            kind: MirTyKind::Range(
                deduce_expr(global_ctx, ctx, scope, mir_ctx, expr_ty_cache, lhs),
                deduce_expr(global_ctx, ctx, scope, mir_ctx, expr_ty_cache, rhs),
            ),
            ref_kind: None,
        },
        &InFnExprKind::RngInclusive(lhs, rhs) => MirTy {
            temporary: true,
            kind: MirTyKind::RangeInclusive(
                deduce_expr(global_ctx, ctx, scope, mir_ctx, expr_ty_cache, lhs),
                deduce_expr(global_ctx, ctx, scope, mir_ctx, expr_ty_cache, rhs),
            ),
            ref_kind: None,
        },
        InFnExprKind::Eq(lhs, rhs) => todo!(),
        InFnExprKind::Ne(_, _) => todo!(),
        InFnExprKind::Lt(_, _) => todo!(),
        InFnExprKind::Gt(_, _) => todo!(),
        InFnExprKind::Le(_, _) => todo!(),
        InFnExprKind::Ge(_, _) => todo!(),
        InFnExprKind::Neg(_) => todo!(),
        InFnExprKind::Add(_, _) => todo!(),
        InFnExprKind::Sub(_, _) => todo!(),
        InFnExprKind::Mul(_, _) => todo!(),
        InFnExprKind::Div(_, _) => todo!(),
        InFnExprKind::Mod(_, _) => todo!(),
        InFnExprKind::Shl(_, _) => todo!(),
        InFnExprKind::Shr(_, _) => todo!(),
        InFnExprKind::BitOr(_, _) => todo!(),
        InFnExprKind::BitAnd(_, _) => todo!(),
        InFnExprKind::BitXor(_, _) => todo!(),
        InFnExprKind::BitNot(_) => todo!(),
        InFnExprKind::LogOr(_, _) => todo!(),
        InFnExprKind::LogAnd(_, _) => todo!(),
        InFnExprKind::LogNot(_) => todo!(),
        InFnExprKind::Cast(_, _) => todo!(),
        InFnExprKind::Object(_) => todo!(),
        InFnExprKind::Call(_, _) => todo!(),
        InFnExprKind::Index(_, _) => todo!(),
        InFnExprKind::Member(_, _) => todo!(),
        InFnExprKind::SizeOf(_) => todo!(),
        InFnExprKind::AddrOf(_) => todo!(),
        InFnExprKind::Deref(_) => todo!(),
        InFnExprKind::Id(_) => todo!(),
        InFnExprKind::Literal(_) => todo!(),
    };

    todo!()

    // match &ctx.exprs[expr.0].kind {
    //     &InFnExprKind::Assign(lhs, rhs) => {
    //         let lhs_ty = deduce_expr(global_ctx, ctx, scope, mir_ctx, expr_ty_cache, lhs);
    //         let rhs_ty = deduce_expr(global_ctx, ctx, scope, mir_ctx, expr_ty_cache, rhs);
    //         let lhs = &ctx.exprs[lhs.0];
    //         let lhs_ty = match lhs.kind {
    //             InFnExprKind::Index(_, _) => todo!(),
    //             InFnExprKind::Member(expr, id) => {
    //                 let expr_ty = deduce_expr(global_ctx, ctx, scope, mir_ctx, expr_ty_cache, expr);

    //                 match mir_ctx.tys[expr_ty.0].kind {
    //                     MirTyKind::Struct(def) => {
    //                         let r#struct = &mir_ctx.structs[def.0];
    //                         let index = match r#struct
    //                             .fields
    //                             .iter()
    //                             .position(|field| field.name.symbol == id.symbol)
    //                         {
    //                             Some(index) => index,
    //                             None => panic!(
    //                                 "{:?} has no member {}",
    //                                 mir_ctx.tys[expr_ty.0].kind, id.symbol
    //                             ),
    //                         };

    //                         // let mut accessible = false;

    //                         // match r#struct.fields[index].vis {
    //                         //     Some(vis) => match vis.kind {
    //                         //         VisKind::Pub => {
    //                         //             accessible = true;
    //                         //         }
    //                         //     },
    //                         //     None => {}
    //                         // };

    //                         // if !accessible {
    //                         //     panic!(
    //                         //         "the member {} of the type {:?} is private",
    //                         //         id.symbol, mir_ctx.tys[expr_ty.0].kind
    //                         //     );
    //                         // }

    //                         MirTy {
    //                             kind: match r#struct.fields[index].kind {
    //                                 MirStructFieldKind::Plain(def) => mir_ctx.tys[def.0].kind,
    //                                 MirStructFieldKind::Struct(def) => MirTyKind::InnerStruct(def),
    //                             },
    //                             ref_kind: mir_ctx.tys[expr_ty.0].ref_kind,
    //                         }
    //                     }
    //                     MirTyKind::InnerStruct(def) => {
    //                         let inner_struct = &mir_ctx.inner_structs[def.0];
    //                         let index = match inner_struct
    //                             .fields
    //                             .iter()
    //                             .position(|field| field.name.symbol == id.symbol)
    //                         {
    //                             Some(index) => index,
    //                             None => panic!(
    //                                 "{:?} has no member {}",
    //                                 mir_ctx.tys[expr_ty.0].kind, id.symbol
    //                             ),
    //                         };

    //                         MirTy {
    //                             kind: match inner_struct.fields[index].kind {
    //                                 MirStructFieldKind::Plain(def) => mir_ctx.tys[def.0].kind,
    //                                 MirStructFieldKind::Struct(def) => MirTyKind::InnerStruct(def),
    //                             },
    //                             ref_kind: mir_ctx.tys[expr_ty.0].ref_kind,
    //                         }
    //                     }
    //                     _ => panic!(
    //                         "{:?} has no member {}",
    //                         mir_ctx.tys[expr_ty.0].kind, id.symbol
    //                     ),
    //                 }
    //             }
    //             InFnExprKind::Id(id) => {
    //                 let ty = lookup_id(
    //                     global_ctx,
    //                     Some(ctx),
    //                     ScopeRef {
    //                         module: ctx.module,
    //                         scope: Some(scope),
    //                     },
    //                     mir_ctx,
    //                     id.symbol,
    //                 )
    //                 .unwrap();

    //                 mir_ctx.tys[ty.0].clone()
    //             }
    //             _ => panic!("it cannot be assigned"),
    //         };
    //         let rhs_ty = deduce_expr(global_ctx, ctx, scope, mir_ctx, expr_ty_cache, rhs);

    //         todo!()
    //     }
    //     InFnExprKind::AssignAdd(_, _) => todo!(),
    //     InFnExprKind::AssignSub(_, _) => todo!(),
    //     InFnExprKind::AssignMul(_, _) => todo!(),
    //     InFnExprKind::AssignDiv(_, _) => todo!(),
    //     InFnExprKind::AssignMod(_, _) => todo!(),
    //     InFnExprKind::AssignShl(_, _) => todo!(),
    //     InFnExprKind::AssignShr(_, _) => todo!(),
    //     InFnExprKind::AssignBitOr(_, _) => todo!(),
    //     InFnExprKind::AssignBitAnd(_, _) => todo!(),
    //     InFnExprKind::AssignBitXor(_, _) => todo!(),
    //     InFnExprKind::AssignBitNot(_, _) => todo!(),
    //     InFnExprKind::Rng(_, _) => todo!(),
    //     InFnExprKind::RngInclusive(_, _) => todo!(),
    //     InFnExprKind::Eq(_, _) => todo!(),
    //     InFnExprKind::Ne(_, _) => todo!(),
    //     InFnExprKind::Lt(_, _) => todo!(),
    //     InFnExprKind::Gt(_, _) => todo!(),
    //     InFnExprKind::Le(_, _) => todo!(),
    //     InFnExprKind::Ge(_, _) => todo!(),
    //     InFnExprKind::Neg(_) => todo!(),
    //     InFnExprKind::Add(_, _) => todo!(),
    //     InFnExprKind::Sub(_, _) => todo!(),
    //     InFnExprKind::Mul(_, _) => todo!(),
    //     InFnExprKind::Div(_, _) => todo!(),
    //     InFnExprKind::Mod(_, _) => todo!(),
    //     InFnExprKind::Shl(_, _) => todo!(),
    //     InFnExprKind::Shr(_, _) => todo!(),
    //     InFnExprKind::BitOr(_, _) => todo!(),
    //     InFnExprKind::BitAnd(_, _) => todo!(),
    //     InFnExprKind::BitXor(_, _) => todo!(),
    //     InFnExprKind::BitNot(_) => todo!(),
    //     InFnExprKind::LogOr(_, _) => todo!(),
    //     InFnExprKind::LogAnd(_, _) => todo!(),
    //     InFnExprKind::LogNot(_) => todo!(),
    //     InFnExprKind::Cast(_, _) => todo!(),
    //     InFnExprKind::Object(_) => todo!(),
    //     InFnExprKind::Call(_, _) => todo!(),
    //     InFnExprKind::Index(_, _) => todo!(),
    //     InFnExprKind::Member(_, _) => todo!(),
    //     InFnExprKind::Deref(_) => todo!(),
    //     InFnExprKind::Id(_) => todo!(),
    //     InFnExprKind::Literal(_) => todo!(),
    // }
}
