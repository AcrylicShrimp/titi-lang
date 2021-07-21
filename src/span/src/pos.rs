use std::ops::{Add, Sub};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pos(u32);

impl Pos {
    pub fn new(pos: u32) -> Self {
        Self(pos)
    }

    pub fn offset(&self, offset: u32) -> Self {
        Self(self.0 + offset)
    }
}

impl Add<Pos> for Pos {
    type Output = u32;

    fn add(self, other: Pos) -> u32 {
        self.0 + other.0
    }
}

impl Sub<Pos> for Pos {
    type Output = u32;

    fn sub(self, other: Pos) -> u32 {
        self.0 - other.0
    }
}
