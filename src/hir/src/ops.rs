use crate::*;

pub enum Ops {
    Unary(UnaryOps),
    Binary(BinaryOps),
}

pub struct UnaryOps {
    pub ty: UnaryOpsType,
    pub lhs: Type,
}

pub struct BinaryOps {
    pub ty: BinaryOpsType,
    pub lhs: Type,
    pub rhs: Type,
}

pub enum UnaryOpsType {
    Neg,
}

pub enum BinaryOpsType {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

pub struct Ops {}
