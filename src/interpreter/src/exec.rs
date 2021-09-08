use crate::parsed::*;
use high_lexer::{Symbol, TokenLiteralKind};

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
            match &stmt.kind {
                ParsedStmtKind::Struct(..) => {}
                ParsedStmtKind::Fn(..) => {}
                ParsedStmtKind::Let(_) => todo!(),
                ParsedStmtKind::If(_) => todo!(),
                ParsedStmtKind::For(_) => todo!(),
                ParsedStmtKind::Block(_) => todo!(),
                ParsedStmtKind::Break(_) => todo!(),
                ParsedStmtKind::Continue(_) => todo!(),
                ParsedStmtKind::Return(_) => todo!(),
                ParsedStmtKind::Expr(expr) => {
                    eval(self.ctx, module, self.ctx.scope(stmt.scope), expr);
                }
            }
        }

        None
    }
}

fn eval(
    ctx: &Context,
    module: &ParsedModule,
    scope: &ParsedScope,
    expr: &ParsedExpr,
) -> Option<Value> {
    match &expr.kind {
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
        ParsedExprKind::Call(lhs, rhs) => {
            match eval_for_find_callable(ctx, module, scope, lhs) {
                Value::Id(id) => {
                    if id == "print" {
                        print!(
                            "{:?}",
                            rhs.iter()
                                .map(|rhs| eval(ctx, module, scope, rhs))
                                .collect::<Vec<_>>()
                        );
                    } else if id == "println" {
                        println!(
                            "{:?}",
                            rhs.iter()
                                .map(|rhs| eval(ctx, module, scope, rhs))
                                .collect::<Vec<_>>()
                        );
                    }

                    return None;
                }
                value => panic!("{:?} is not callable", value),
            }

            todo!()
        }
        ParsedExprKind::Index(_, _) => todo!(),
        ParsedExprKind::Member(_, _) => todo!(),
        ParsedExprKind::Object(_) => todo!(),
        ParsedExprKind::Id(_) => todo!(),
        ParsedExprKind::Literal(literal) => Some(match literal.literal.kind() {
            TokenLiteralKind::Bool => Value::Bool(literal.literal.str() == "true"),
            TokenLiteralKind::IntegerBinary => todo!(),
            TokenLiteralKind::IntegerOctal => todo!(),
            TokenLiteralKind::IntegerHexadecimal => todo!(),
            TokenLiteralKind::IntegerDecimal => todo!(),
            TokenLiteralKind::Float => todo!(),
            TokenLiteralKind::SingleQuotedStr => todo!(),
            TokenLiteralKind::DoubleQuotedStr => {
                Value::Str(literal.literal.str().as_str().to_owned())
            }
        }),
    }
}

fn eval_for_find_callable(
    ctx: &Context,
    module: &ParsedModule,
    scope: &ParsedScope,
    expr: &ParsedExpr,
) -> Value {
    match &expr.kind {
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
        ParsedExprKind::Id(id) => Value::Id(id.symbol),
        ParsedExprKind::Literal(_) => todo!(),
    }
}

#[derive(Debug)]
pub enum Value {
    Bool(bool),
    Str(String),
    Id(Symbol),
}
