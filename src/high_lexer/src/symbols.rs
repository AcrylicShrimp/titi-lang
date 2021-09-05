use lazy_static::lazy_static;
use parking_lot::Mutex;
use std::{
    fmt::{Debug, Display},
    num::NonZeroU32,
};
use str_interner::{StrIdx, StrInterner};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(pub(crate) StrIdx);

impl Symbol {
    pub const fn from_idx_const(index: u32) -> Self {
        Self(StrIdx::new_const(index))
    }

    pub fn as_str(&self) -> &'static str {
        STR_INTERNER.lock().get_str(self.0)
    }
}

impl From<Symbol> for u32 {
    fn from(symbol: Symbol) -> Self {
        u32::from(symbol.0)
    }
}

impl From<Symbol> for NonZeroU32 {
    fn from(symbol: Symbol) -> Self {
        NonZeroU32::from(symbol.0)
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", self.as_str())
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

pub const BOOL: Symbol = Symbol::from_idx_const(1);
pub const BYTE: Symbol = Symbol::from_idx_const(2);
pub const CHAR: Symbol = Symbol::from_idx_const(3);
pub const I64: Symbol = Symbol::from_idx_const(4);
pub const U64: Symbol = Symbol::from_idx_const(5);
pub const ISIZE: Symbol = Symbol::from_idx_const(6);
pub const USIZE: Symbol = Symbol::from_idx_const(7);
pub const F64: Symbol = Symbol::from_idx_const(8);
pub const STR: Symbol = Symbol::from_idx_const(9);
pub const CPTR: Symbol = Symbol::from_idx_const(10);
pub const MPTR: Symbol = Symbol::from_idx_const(11);
pub const IF: Symbol = Symbol::from_idx_const(12);
pub const ELSE: Symbol = Symbol::from_idx_const(13);
pub const FOR: Symbol = Symbol::from_idx_const(14);
pub const IN: Symbol = Symbol::from_idx_const(15);
pub const AS: Symbol = Symbol::from_idx_const(16);
pub const FN: Symbol = Symbol::from_idx_const(17);
pub const STRUCT: Symbol = Symbol::from_idx_const(18);
pub const PUB: Symbol = Symbol::from_idx_const(19);
pub const MOD: Symbol = Symbol::from_idx_const(20);

lazy_static! {
    pub(crate) static ref STR_INTERNER: Mutex<StrInterner> = StrInterner::with_prefilled(&[
        "bool", "byte", "char", "i64", "u64", "isize", "usize", "f64", "str", "cptr", "mptr", "if",
        "else", "for", "in", "as", "fn", "struct", "pub", "mod"
    ])
    .into();
}
