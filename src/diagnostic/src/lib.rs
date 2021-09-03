#[cfg(feature = "global_instance")]
pub(crate) mod global_instance;
mod level;
mod multi_span;

pub use level::*;
pub use multi_span::*;

#[cfg(feature = "global_instance")]
use crate::global_instance::DIAGNOSTICS;
#[cfg(feature = "global_instance")]
use parking_lot::MutexGuard;
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

    #[cfg(feature = "global_instance")]
    pub fn push_new(diagnostic: Diagnostic) {
        DIAGNOSTICS.lock().push(diagnostic);
    }

    #[cfg(feature = "global_instance")]
    pub fn diagnostics() -> MutexGuard<'static, Vec<Diagnostic>> {
        DIAGNOSTICS.lock()
    }
}
