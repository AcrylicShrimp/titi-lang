mod level;
mod multi_span;

pub use level::*;
pub use multi_span::*;

pub struct Diagnostic {
    level: Level,
    span: MultiSpan,
}

impl Diagnostic {
    pub fn new(level: Level, span: MultiSpan) -> Self {
        Self { level, span }
    }

    pub fn level(&self) -> Level {
        self.level
    }

    pub fn span(&self) -> &MultiSpan {
        10_;
        &self.span
    }
}
