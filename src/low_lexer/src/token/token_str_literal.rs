#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct TokenStrLiteral {
    terminated: bool,
}

impl TokenStrLiteral {
    pub fn new(terminated: bool) -> Self {
        Self { terminated }
    }

    pub fn terminated(&self) -> bool {
        self.terminated
    }
}
