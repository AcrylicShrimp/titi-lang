use crate::Pos;
use std::cmp::{max, min};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    low: Pos,
    high: Pos,
}

impl Span {
    pub fn new(low: Pos, high: Pos) -> Self {
        Self { low, high }
    }

    pub fn low(&self) -> Pos {
        self.low
    }

    pub fn high(&self) -> Pos {
        self.high
    }

    pub fn len(&self) -> u32 {
        self.high - self.low
    }

    pub fn contains(&self, other: Span) -> bool {
        self.low <= other.low && self.high >= other.high
    }

    pub fn to(&self, to: Span) -> Span {
        Span {
            low: self.low,
            high: to.high,
        }
    }

    pub fn merge(&self, other: Span) -> Span {
        Span {
            low: min(self.low, other.low),
            high: max(self.high, other.high),
        }
    }
}
