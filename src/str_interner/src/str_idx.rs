#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StrIdx(u32);

impl StrIdx {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub const fn new_const(index: u32) -> StrIdx {
        Self(index)
    }
}

impl From<StrIdx> for u32 {
    fn from(idx: StrIdx) -> Self {
        idx.0
    }
}
