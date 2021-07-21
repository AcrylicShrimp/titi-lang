#[cfg(feature = "global_instance")]
use crate::global_instance::STR_INTERNER;
#[cfg(feature = "global_instance")]
use std::fmt::{Debug, Display};

#[cfg(feature = "global_instance")]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StrIdx(u32);

#[cfg(not(feature = "global_instance"))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StrIdx(u32);

impl StrIdx {
    #[cfg(feature = "global_instance")]
    pub fn intern<S: AsRef<str>>(str: S) -> Self {
        STR_INTERNER.lock().intern(str.as_ref())
    }

    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub const fn new_const(index: u32) -> StrIdx {
        Self(index)
    }

    #[cfg(feature = "global_instance")]
    pub fn as_str(&self) -> &'static str {
        STR_INTERNER.lock().get_str(self.clone())
    }
}

impl From<StrIdx> for u32 {
    fn from(idx: StrIdx) -> Self {
        idx.0
    }
}

#[cfg(feature = "global_instance")]
impl Debug for StrIdx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[cfg(feature = "global_instance")]
impl Display for StrIdx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
