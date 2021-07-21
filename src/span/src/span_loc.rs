use crate::{LineCol, Source};
use std::fmt::Display;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SpanLoc {
    source: Arc<Source>,
    begin: LineCol,
    end: LineCol,
}

impl SpanLoc {
    pub fn new(source: Arc<Source>, begin: LineCol, end: LineCol) -> Self {
        Self { source, begin, end }
    }

    pub fn source(&self) -> &Arc<Source> {
        &self.source
    }

    pub fn begin(&self) -> LineCol {
        self.begin
    }

    pub fn end(&self) -> LineCol {
        self.end
    }
}

impl Display for SpanLoc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}: {}:{}",
            self.source.name(),
            self.begin.line() + 1,
            self.begin.column() + 1,
            self.end.line() + 1,
            self.end.column() + 1
        )
    }
}
