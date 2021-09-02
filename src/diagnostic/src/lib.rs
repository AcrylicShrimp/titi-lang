mod level;
mod multi_span;

pub use level::*;
pub use multi_span::*;

pub struct Diagnostic {
    level: Level,
    message: String,
    span: MultiSpan,
}

impl Diagnostic {
    pub fn new(level: Level, message: String, span: MultiSpan) -> Self {
        Self {
            level,
            message,
            span,
        }
    }

    pub fn level(&self) -> Level {
        self.level
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn span(&self) -> &MultiSpan {
        &self.span
    }
}
