mod level;

use level::*;

pub struct Diagnostic {
    level: Level,
}

impl Diagnostic {
    pub fn new(level: Level) -> Self {
        Self { level }
    }
}
