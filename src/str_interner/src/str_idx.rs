use std::num::NonZeroU32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StrIdx(NonZeroU32);

impl StrIdx {
    pub fn new(index: u32) -> Self {
        Self(unsafe { NonZeroU32::new_unchecked(index) })
    }

    pub const fn new_const(index: u32) -> Self {
        Self(unsafe { NonZeroU32::new_unchecked(index) })
    }
}

impl From<StrIdx> for u32 {
    fn from(idx: StrIdx) -> Self {
        u32::from(idx.0)
    }
}

impl From<StrIdx> for NonZeroU32 {
    fn from(idx: StrIdx) -> Self {
        idx.0
    }
}
