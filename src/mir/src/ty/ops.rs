use crate::ty::Ty;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOpKind {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
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
    LogOr,
    LogAnd,
}

#[derive(Debug, Clone, Hash)]
pub struct BinaryOp {
    pub kind: BinaryOpKind,
    pub lhs: Ty,
    pub rhs: Ty,
    pub ty: Ty,
}

#[derive(Default, Debug)]
pub struct BinaryOpMap {
    ops: HashMap<BinaryOpKind, Vec<BinaryOp>>,
}

impl BinaryOpMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, op: BinaryOp) {
        self.ops.entry(op.kind).or_default().push(op);
    }

    pub fn get(&self, kind: BinaryOpKind) -> Option<&Vec<BinaryOp>> {
        self.ops.get(&kind)
    }

    pub fn try_match_op(&self, kind: BinaryOpKind, lhs: Ty, rhs: Ty) -> Option<&BinaryOp> {
        let ops = if let Some(ops) = self.ops.get(&kind) {
            ops
        } else {
            return None;
        };

        for op in ops {
            if op.lhs.as_ty().is_assignable_from(lhs.as_ty())
                && op.rhs.as_ty().is_assignable_from(rhs.as_ty())
            {
                return Some(op);
            }
        }

        None
    }
}
