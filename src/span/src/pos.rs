use std::ops::{Add, Sub};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pos(u32);

impl Pos {
    pub fn new(pos: u32) -> Self {
        Self(pos)
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
