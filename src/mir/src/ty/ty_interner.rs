use hir::{FunctionDef, InFnLetDef, InnerStructDef, StructDef};
use lazy_static::lazy_static;
use parking_lot::Mutex;
use rustc_hash::FxHashMap;
use std::fmt::{Debug, Display};

#[derive(Debug)]
pub struct TyInterner {
    tys: Vec<MirTy>,
    reversed: FxHashMap<MirTy, TyIdx>,
}

impl TyInterner {
    pub fn with_prefilled(mut tys: Vec<MirTy>) -> Self {
        tys.insert(
            0,
            MirTy {
                kind: MirTyKind::None,
                source_kind: None,
                is_ref: false,
            },
        );

        let reversed = tys
            .into_iter()
            .zip((0..).map(TyIdx::new))
            .collect::<FxHashMap<_, _>>();
        let tys = {
            let mut tys = reversed.iter().collect::<Vec<_>>();
            tys.sort_unstable_by_key(|(_, &idx)| idx);
            tys.into_iter().map(|(ty, _)| ty.clone()).collect()
        };

        Self { reversed, tys }
    }

    pub fn get_ty(&self, idx: TyIdx) -> &'static MirTy {
        unsafe { &*(&self.tys[u32::from(idx) as usize] as *const _) }
    }

    pub fn intern(&mut self, ty: MirTy) -> TyIdx {
        if let Some(&reverse) = self.reversed.get(&ty) {
            return reverse;
        }

        let idx = TyIdx::new(self.tys.len() as _);
        self.reversed.insert(ty.clone(), idx);
        self.tys.push(ty);
        idx
    }
}

unsafe impl Send for TyInterner {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TyIdx(u32);

impl TyIdx {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub const fn new_const(index: u32) -> Self {
        Self(index)
    }
}

impl From<TyIdx> for u32 {
    fn from(idx: TyIdx) -> Self {
        u32::from(idx.0)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ty(pub(crate) TyIdx);

impl Ty {
    pub const fn from_idx_const(index: u32) -> Self {
        Self(TyIdx::new_const(index))
    }

    pub fn as_ty(&self) -> &'static MirTy {
        TY_INTERNER.lock().get_ty(self.0)
    }
}

impl From<Ty> for u32 {
    fn from(ty: Ty) -> Self {
        u32::from(ty.0)
    }
}

impl Debug for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{:?}\"", self.as_ty())
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.as_ty())
    }
}

pub const TY_NONE: Ty = Ty::from_idx_const(0);
pub const TY_BOOL: Ty = Ty::from_idx_const(1);
pub const TY_BYTE: Ty = Ty::from_idx_const(2);
pub const TY_CHAR: Ty = Ty::from_idx_const(3);
pub const TY_I64: Ty = Ty::from_idx_const(4);
pub const TY_U64: Ty = Ty::from_idx_const(5);
pub const TY_ISIZE: Ty = Ty::from_idx_const(6);
pub const TY_USIZE: Ty = Ty::from_idx_const(7);
pub const TY_F64: Ty = Ty::from_idx_const(8);
pub const TY_STR: Ty = Ty::from_idx_const(9);
pub const TY_PTRSTR: Ty = Ty::from_idx_const(10);

lazy_static! {
    pub(crate) static ref TY_INTERNER: Mutex<TyInterner> = TyInterner::with_prefilled(vec![
        MirTy {
            kind: MirTyKind::Bool,
            source_kind: None,
            is_ref: false,
        },
        MirTy {
            kind: MirTyKind::Byte,
            source_kind: None,
            is_ref: false,
        },
        MirTy {
            kind: MirTyKind::Char,
            source_kind: None,
            is_ref: false,
        },
        MirTy {
            kind: MirTyKind::I64,
            source_kind: None,
            is_ref: false,
        },
        MirTy {
            kind: MirTyKind::U64,
            source_kind: None,
            is_ref: false,
        },
        MirTy {
            kind: MirTyKind::Isize,
            source_kind: None,
            is_ref: false,
        },
        MirTy {
            kind: MirTyKind::Usize,
            source_kind: None,
            is_ref: false,
        },
        MirTy {
            kind: MirTyKind::F64,
            source_kind: None,
            is_ref: false,
        },
        MirTy {
            kind: MirTyKind::Str,
            source_kind: None,
            is_ref: false,
        },
        MirTy {
            kind: MirTyKind::Ptr(TY_STR),
            source_kind: None,
            is_ref: false,
        }
    ])
    .into();
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MirTy {
    pub kind: MirTyKind,
    pub source_kind: Option<MirTySourceKind>,
    pub is_ref: bool,
}

impl MirTy {
    pub fn is_none(&self) -> bool {
        self.kind == MirTyKind::None
    }

    pub fn is_addressable(&self) -> bool {
        self.source_kind.is_some() || self.is_ref
    }

    pub fn is_same_with(&self, other: &MirTy) -> bool {
        self.kind == other.kind && self.is_ref == other.is_ref
    }

    pub fn is_assignable_from(&self, other: &MirTy) -> bool {
        if self.is_none() || other.is_none() {
            return false;
        }

        if self.is_ref {
            if !other.is_ref {
                return false;
            }

            return self.kind == other.kind;
        }

        if other.is_ref {
            return false;
        }

        match &self.kind {
            MirTyKind::None => false,
            MirTyKind::Bool => match other.kind {
                MirTyKind::Bool
                | MirTyKind::Byte
                | MirTyKind::Char
                | MirTyKind::I64
                | MirTyKind::U64
                | MirTyKind::Isize
                | MirTyKind::Usize => true,
                _ => false,
            },
            MirTyKind::Byte => match other.kind {
                MirTyKind::Bool | MirTyKind::Byte => true,
                _ => false,
            },
            MirTyKind::Char => match other.kind {
                MirTyKind::Bool | MirTyKind::Byte | MirTyKind::Char => true,
                _ => false,
            },
            MirTyKind::I64 => match other.kind {
                MirTyKind::Bool | MirTyKind::Byte | MirTyKind::Char | MirTyKind::I64 => true,
                _ => false,
            },
            MirTyKind::U64 => match other.kind {
                MirTyKind::Bool | MirTyKind::Byte | MirTyKind::Char | MirTyKind::U64 => true,
                _ => false,
            },
            MirTyKind::Isize => match other.kind {
                MirTyKind::Bool | MirTyKind::Byte | MirTyKind::Isize => true,
                _ => false,
            },
            MirTyKind::Usize => match other.kind {
                MirTyKind::Bool | MirTyKind::Byte | MirTyKind::Usize => true,
                _ => false,
            },
            MirTyKind::F64 => match other.kind {
                MirTyKind::F64 => true,
                _ => false,
            },
            MirTyKind::Str => match other.kind {
                MirTyKind::Str => true,
                _ => false,
            },
            MirTyKind::Ptr(lhs_ty) => match other.kind {
                MirTyKind::Bool | MirTyKind::Byte | MirTyKind::Usize => true,
                MirTyKind::Ptr(rhs_ty) => lhs_ty.as_ty().is_same_with(rhs_ty.as_ty()),
                _ => false,
            },
            MirTyKind::Range(lhs_ty0, lhs_ty1) => match other.kind {
                MirTyKind::Range(rhs_ty0, rhs_ty1) => {
                    lhs_ty0.as_ty().is_same_with(rhs_ty0.as_ty())
                        && lhs_ty1.as_ty().is_same_with(rhs_ty1.as_ty())
                }
                _ => false,
            },
            MirTyKind::RangeInclusive(lhs_ty0, lhs_ty1) => match other.kind {
                MirTyKind::RangeInclusive(rhs_ty0, rhs_ty1) => {
                    lhs_ty0.as_ty().is_same_with(rhs_ty0.as_ty())
                        && lhs_ty1.as_ty().is_same_with(rhs_ty1.as_ty())
                }
                _ => false,
            },
            &MirTyKind::Struct(lhs_def) => match other.kind {
                MirTyKind::Struct(rhs_def) => lhs_def == rhs_def,
                _ => false,
            },
            &MirTyKind::InnerStruct(lhs_def) => match other.kind {
                MirTyKind::InnerStruct(rhs_def) => lhs_def == rhs_def,
                _ => false,
            },
            MirTyKind::Fn(lhs_fn) => match &other.kind {
                MirTyKind::Fn(rhs_fn) => {
                    match lhs_fn.return_ty {
                        Some(lhs_return_ty) => {
                            return match rhs_fn.return_ty {
                                Some(rhs_return_ty) => {
                                    lhs_return_ty.as_ty().is_same_with(rhs_return_ty.as_ty())
                                }
                                None => false,
                            };
                        }
                        None => {
                            if !rhs_fn.return_ty.is_none() {
                                return false;
                            }
                        }
                    }

                    if lhs_fn.params.len() != rhs_fn.params.len() {
                        return false;
                    }

                    for index in 0..lhs_fn.params.len() {
                        if !lhs_fn.params[index]
                            .as_ty()
                            .is_same_with(&rhs_fn.params[index].as_ty())
                        {
                            return false;
                        }
                    }

                    true
                }
                _ => false,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MirTyKind {
    None,
    Bool,
    Byte,
    Char,
    I64,
    U64,
    Isize,
    Usize,
    F64,
    Str,
    Ptr(Ty),
    Range(Ty, Ty),
    RangeInclusive(Ty, Ty),
    Struct(StructDef),
    InnerStruct(InnerStructDef),
    Fn(MirTyFn),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MirTySourceKind {
    Deref,
    Let(FunctionDef, InFnLetDef),
    Struct(StructDef, usize),
    InnerStruct(InnerStructDef, usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MirTyFn {
    pub params: Vec<Ty>,
    pub return_ty: Option<Ty>,
}