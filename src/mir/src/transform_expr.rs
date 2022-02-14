use crate::{
    MirExpr, MirExprCall, MirExprCallTargetKind, MirExprDef, MirExprKind, MirIntrinsic, MirStmt,
    MirStmtKind, MirTy, MirTyKind,
};
// use hir::{ExprDef, GlobalExpr};

// pub fn transform_expr(
//     mir_tys: &mut Vec<MirTy>,
//     mir_stmts: &mut Vec<MirStmt>,
//     mir_exprs: &mut Vec<MirExpr>,
//     exprs: &[GlobalExpr],
//     expr: ExprDef,
// ) -> MirExprDef {
//     let expr = &exprs[expr.0];

//     match &expr.kind {
//         hir::GlobalExprKind::AssignAdd(_, _) => todo!(),
//         hir::GlobalExprKind::AssignSub(_, _) => todo!(),
//         hir::GlobalExprKind::AssignMul(_, _) => todo!(),
//         hir::GlobalExprKind::AssignDiv(_, _) => todo!(),
//         hir::GlobalExprKind::AssignMod(_, _) => todo!(),
//         hir::GlobalExprKind::AssignShl(_, _) => todo!(),
//         hir::GlobalExprKind::AssignShr(_, _) => todo!(),
//         hir::GlobalExprKind::AssignBitOr(_, _) => todo!(),
//         hir::GlobalExprKind::AssignBitAnd(_, _) => todo!(),
//         hir::GlobalExprKind::AssignBitXor(_, _) => todo!(),
//         hir::GlobalExprKind::AssignBitNot(_, _) => todo!(),
//         hir::GlobalExprKind::Rng(_, _) => todo!(),
//         hir::GlobalExprKind::RngInclusive(_, _) => todo!(),
//         hir::GlobalExprKind::Eq(_, _) => todo!(),
//         hir::GlobalExprKind::Ne(_, _) => todo!(),
//         hir::GlobalExprKind::Lt(_, _) => todo!(),
//         hir::GlobalExprKind::Gt(_, _) => todo!(),
//         hir::GlobalExprKind::Le(_, _) => todo!(),
//         hir::GlobalExprKind::Ge(_, _) => todo!(),
//         hir::GlobalExprKind::Neg(_) => todo!(),
//         hir::GlobalExprKind::Add(_, _) => todo!(),
//         hir::GlobalExprKind::Sub(_, _) => todo!(),
//         hir::GlobalExprKind::Mul(_, _) => todo!(),
//         hir::GlobalExprKind::Div(_, _) => todo!(),
//         hir::GlobalExprKind::Mod(_, _) => todo!(),
//         hir::GlobalExprKind::Shl(_, _) => todo!(),
//         hir::GlobalExprKind::Shr(_, _) => todo!(),
//         hir::GlobalExprKind::BitOr(_, _) => todo!(),
//         hir::GlobalExprKind::BitAnd(_, _) => todo!(),
//         hir::GlobalExprKind::BitXor(_, _) => todo!(),
//         hir::GlobalExprKind::BitNot(_) => todo!(),
//         hir::GlobalExprKind::LogOr(_, _) => todo!(),
//         hir::GlobalExprKind::LogAnd(_, _) => todo!(),
//         hir::GlobalExprKind::LogNot(_) => todo!(),
//         hir::GlobalExprKind::Cast(_, _) => todo!(),
//         hir::GlobalExprKind::Object(_) => todo!(),
//         hir::GlobalExprKind::Call(_, _) => todo!(),
//         hir::GlobalExprKind::Index(_, _) => todo!(),
//         hir::GlobalExprKind::Member(_, _) => todo!(),
//         hir::GlobalExprKind::Deref(_) => todo!(),
//         hir::GlobalExprKind::Id(_) => todo!(),
//         hir::GlobalExprKind::Literal(_) => todo!(),
//     };

//     let stmt = MirStmt {
//         kind: MirStmtKind::Assign(),
//         span: expr.span,
//     };
//     let def = mir_stmts.len();
//     mir_stmts.push(stmt);
//     MirExprDef(def)
// }
