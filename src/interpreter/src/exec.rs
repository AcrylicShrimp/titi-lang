use crate::parsed::*;

pub struct Runtime<'ctx> {
    ctx: &'ctx Context,
}

impl<'ctx> Runtime<'ctx> {
    pub fn new(ctx: &'ctx Context) -> Self {
        Self { ctx }
    }

    pub fn call(
        &self,
        module: &'ctx ParsedModule,
        func: &ParsedFunction,
        args: Vec<Value>,
    ) -> Option<Value> {
        for stmt in &func.body.stmts {
            println!("{:?}", stmt);
        }

        None
    }
}

fn eval(ctx: &Context, module: &ParsedModule, scope: &ParsedScope, expr: &ParsedExpr) -> Value {
    match expr.kind {
        ParsedExprKind::Assign(_, _) => todo!(),
        ParsedExprKind::AssignAdd(_, _) => todo!(),
        ParsedExprKind::AssignSub(_, _) => todo!(),
        ParsedExprKind::AssignMul(_, _) => todo!(),
        ParsedExprKind::AssignDiv(_, _) => todo!(),
        ParsedExprKind::AssignMod(_, _) => todo!(),
        ParsedExprKind::AssignShl(_, _) => todo!(),
        ParsedExprKind::AssignShr(_, _) => todo!(),
        ParsedExprKind::AssignBitOr(_, _) => todo!(),
        ParsedExprKind::AssignBitAnd(_, _) => todo!(),
        ParsedExprKind::AssignBitXor(_, _) => todo!(),
        ParsedExprKind::AssignBitNot(_, _) => todo!(),
        ParsedExprKind::Rng(_, _) => todo!(),
        ParsedExprKind::RngInclusive(_, _) => todo!(),
        ParsedExprKind::Eq(_, _) => todo!(),
        ParsedExprKind::Ne(_, _) => todo!(),
        ParsedExprKind::Lt(_, _) => todo!(),
        ParsedExprKind::Gt(_, _) => todo!(),
        ParsedExprKind::Le(_, _) => todo!(),
        ParsedExprKind::Ge(_, _) => todo!(),
        ParsedExprKind::Neg(_) => todo!(),
        ParsedExprKind::Add(_, _) => todo!(),
        ParsedExprKind::Sub(_, _) => todo!(),
        ParsedExprKind::Mul(_, _) => todo!(),
        ParsedExprKind::Div(_, _) => todo!(),
        ParsedExprKind::Mod(_, _) => todo!(),
        ParsedExprKind::Shl(_, _) => todo!(),
        ParsedExprKind::Shr(_, _) => todo!(),
        ParsedExprKind::BitOr(_, _) => todo!(),
        ParsedExprKind::BitAnd(_, _) => todo!(),
        ParsedExprKind::BitXor(_, _) => todo!(),
        ParsedExprKind::BitNot(_) => todo!(),
        ParsedExprKind::LogOr(_, _) => todo!(),
        ParsedExprKind::LogAnd(_, _) => todo!(),
        ParsedExprKind::LogNot(_) => todo!(),
        ParsedExprKind::Cast(_, _) => todo!(),
        ParsedExprKind::Call(_, _) => todo!(),
        ParsedExprKind::Index(_, _) => todo!(),
        ParsedExprKind::Member(_, _) => todo!(),
        ParsedExprKind::Object(_) => todo!(),
        ParsedExprKind::Id(_) => todo!(),
        ParsedExprKind::Literal(_) => todo!(),
    }
}

pub enum Value {}
