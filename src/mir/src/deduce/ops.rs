use crate::ty_interner::Ty;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtendedTy {
    pub ty: Ty,
    pub addressable: bool,
}

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

        // Best fit
        for op in ops {
            if op.lhs == lhs && op.rhs == rhs {
                return Some(op);
            }
        }

        // First fit
        for op in ops {
            if op.lhs == lhs && op.rhs.as_ty().kind == rhs.as_ty().kind {
                return Some(op);
            }
        }

        None
    }
}
