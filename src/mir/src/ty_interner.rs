use crate::{MirTy, MirTyKind};
use lazy_static::lazy_static;
use parking_lot::Mutex;
use rustc_hash::FxHashMap;
use std::fmt::{Debug, Display};
use std::num::NonZeroU32;

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
                ref_kind: None,
            },
        );

        Self {
            tys,
            reversed: tys.into_iter().zip((1..).map(TyIdx::new)).collect(),
        }
    }

    pub fn get_ty(&self, idx: TyIdx) -> &MirTy {
        &self.tys[u32::from(idx) as usize]
    }

    pub fn intern(&mut self, ty: MirTy) -> TyIdx {
        if let Some(&reverse) = self.reversed.get(&ty) {
            return reverse;
        }

        let idx = TyIdx::new(self.tys.len() as _);
        self.tys.push(ty);
        self.reversed.insert(ty, idx);
        idx
    }
}

unsafe impl Send for TyInterner {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TyIdx(NonZeroU32);

impl TyIdx {
    pub fn new(index: u32) -> Self {
        Self(unsafe { NonZeroU32::new_unchecked(index) })
    }

    pub const fn new_const(index: u32) -> Self {
        Self(unsafe { NonZeroU32::new_unchecked(index) })
    }
}

impl From<TyIdx> for u32 {
    fn from(idx: TyIdx) -> Self {
        u32::from(idx.0)
    }
}

impl From<TyIdx> for NonZeroU32 {
    fn from(idx: TyIdx) -> Self {
        idx.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ty(pub(crate) TyIdx);

impl Ty {
    pub const fn from_idx_const(index: u32) -> Self {
        Self(TyIdx::new_const(index))
    }

    pub fn as_ty(&self) -> &MirTy {
        TY_INTERNER.lock().get_ty(self.0)
    }
}

impl From<Ty> for u32 {
    fn from(ty: Ty) -> Self {
        u32::from(ty.0)
    }
}

impl From<Ty> for NonZeroU32 {
    fn from(ty: Ty) -> Self {
        NonZeroU32::from(ty.0)
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

pub const TY_NONE: Ty = Ty::from_idx_const(1);
pub const TY_BOOL: Ty = Ty::from_idx_const(2);
pub const TY_BYTE: Ty = Ty::from_idx_const(3);
pub const TY_CHAR: Ty = Ty::from_idx_const(4);
pub const TY_I64: Ty = Ty::from_idx_const(5);
pub const TY_U64: Ty = Ty::from_idx_const(6);
pub const TY_ISIZE: Ty = Ty::from_idx_const(7);
pub const TY_USIZE: Ty = Ty::from_idx_const(8);
pub const TY_F64: Ty = Ty::from_idx_const(9);
pub const TY_STR: Ty = Ty::from_idx_const(10);
pub const TY_CPTRSTR: Ty = Ty::from_idx_const(11);
pub const TY_MPTRSTR: Ty = Ty::from_idx_const(12);

lazy_static! {
    pub(crate) static ref TY_INTERNER: Mutex<TyInterner> = TyInterner::with_prefilled(vec![
        MirTy {
            kind: MirTyKind::None,
            ref_kind: None,
        },
        MirTy {
            kind: MirTyKind::Bool,
            ref_kind: None,
        },
        MirTy {
            kind: MirTyKind::Byte,
            ref_kind: None,
        },
        MirTy {
            kind: MirTyKind::Char,
            ref_kind: None,
        },
        MirTy {
            kind: MirTyKind::I64,
            ref_kind: None,
        },
        MirTy {
            kind: MirTyKind::U64,
            ref_kind: None,
        },
        MirTy {
            kind: MirTyKind::Isize,
            ref_kind: None,
        },
        MirTy {
            kind: MirTyKind::Usize,
            ref_kind: None,
        },
        MirTy {
            kind: MirTyKind::F64,
            ref_kind: None,
        },
        MirTy {
            kind: MirTyKind::Str,
            ref_kind: None,
        },
        MirTy {
            kind: MirTyKind::Cptr(TY_STR),
            ref_kind: None,
        },
        MirTy {
            kind: MirTyKind::Mptr(TY_STR),
            ref_kind: None,
        }
    ])
    .into();
}
