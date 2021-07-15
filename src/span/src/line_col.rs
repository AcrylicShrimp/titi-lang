#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LineCol {
    line: u32,
    column: u32,
}

impl LineCol {
    pub fn new(line: u32, column: u32) -> Self {
        Self { line, column }
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn column(&self) -> u32 {
        self.column
    }
}
