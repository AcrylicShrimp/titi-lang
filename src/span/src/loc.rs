use crate::{LineCol, Source};
use std::fmt::Display;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Loc {
    source: Arc<Source>,
    line_col: LineCol,
}

impl Loc {
    pub fn new(source: Arc<Source>, line_col: LineCol) -> Self {
        Self { source, line_col }
    }

    pub fn source(&self) -> &Arc<Source> {
        &self.source
    }

    pub fn line_col(&self) -> LineCol {
        self.line_col
    }
}

impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.source.name(),
            self.line_col.line() + 1,
            self.line_col.column() + 1
        )
    }
}
