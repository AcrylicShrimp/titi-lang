use span::Span;

// TODO: Consider add a level for each span items.
#[derive(Default, Debug, Clone, Hash)]
pub struct MultiSpan {
    spans: Vec<(String, Option<Span>)>,
}

impl MultiSpan {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_spans(spans: Vec<(String, Option<Span>)>) -> Self {
        Self { spans }
    }

    pub fn spans(&self) -> &Vec<(String, Option<Span>)> {
        &self.spans
    }

    pub fn spans_mut(&mut self) -> &mut Vec<(String, Option<Span>)> {
        &mut self.spans
    }
}
