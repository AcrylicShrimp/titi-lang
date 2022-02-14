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

impl<'s> PartialEq<&'s str> for Symbol {
    fn eq(&self, other: &&'s str) -> bool {
        self.as_str() == *other
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

pub const EMPTY: Symbol = Symbol::from_idx_const(1);
pub const MAIN: Symbol = Symbol::from_idx_const(2);
pub const BOOL: Symbol = Symbol::from_idx_const(3);
pub const BYTE: Symbol = Symbol::from_idx_const(4);
pub const CHAR: Symbol = Symbol::from_idx_const(5);
pub const I64: Symbol = Symbol::from_idx_const(6);
pub const U64: Symbol = Symbol::from_idx_const(7);
pub const ISIZE: Symbol = Symbol::from_idx_const(8);
pub const USIZE: Symbol = Symbol::from_idx_const(9);
pub const F64: Symbol = Symbol::from_idx_const(10);
pub const STR: Symbol = Symbol::from_idx_const(11);
pub const PTR: Symbol = Symbol::from_idx_const(12);
pub const REF: Symbol = Symbol::from_idx_const(13);
pub const SIZEOF: Symbol = Symbol::from_idx_const(14);
pub const ADDROF: Symbol = Symbol::from_idx_const(15);
pub const AS: Symbol = Symbol::from_idx_const(16);
pub const USE: Symbol = Symbol::from_idx_const(17);
pub const EXTERN: Symbol = Symbol::from_idx_const(18);
pub const PUB: Symbol = Symbol::from_idx_const(19);
pub const FN: Symbol = Symbol::from_idx_const(20);
pub const BREAK: Symbol = Symbol::from_idx_const(21);
pub const CONTINUE: Symbol = Symbol::from_idx_const(22);
pub const RETURN: Symbol = Symbol::from_idx_const(23);
pub const IF: Symbol = Symbol::from_idx_const(24);
pub const ELSE: Symbol = Symbol::from_idx_const(25);
pub const FOR: Symbol = Symbol::from_idx_const(26);
pub const IN: Symbol = Symbol::from_idx_const(27);
pub const LET: Symbol = Symbol::from_idx_const(28);
pub const STRUCT: Symbol = Symbol::from_idx_const(29);

lazy_static! {
    pub(crate) static ref STR_INTERNER: Mutex<StrInterner> = StrInterner::with_prefilled(&[
        "", "main", "bool", "byte", "char", "i64", "u64", "isize", "usize", "f64", "str", "ptr",
        "ref", "sizeof", "addrof", "as", "use", "extern", "pub", "fn", "break", "continue",
        "return", "if", "else", "for", "in", "let", "struct"
    ])
    .into();
}
