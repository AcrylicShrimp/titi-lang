use span::Span;

#[derive(Default, Debug, Clone, Hash)]
pub struct MultiSpan {
    spans: Vec<(Span, Option<String>)>,
}

impl MultiSpan {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn spans(&self) -> &Vec<(Span, Option<String>)> {
        &self.spans
    }

    pub fn spans_mut(&mut self) -> &mut Vec<(Span, Option<String>)> {
        &mut self.spans
    }
}
