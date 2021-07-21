use crate::{Loc, Pos, Source, SourcePath, Span, SpanLoc};
use std::{cmp::Ordering, sync::Arc};

#[derive(Default, Debug)]
pub struct SourceMap {
    sources: Vec<Arc<Source>>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn end_pos(&self) -> Pos {
        self.sources
            .last()
            .map_or(Pos::new(0), |source| source.span().high())
    }

    pub fn find_source(&self, pos: Pos) -> &Arc<Source> {
        self.sources
            .binary_search_by(|source| {
                if pos > source.span().high() {
                    Ordering::Less
                } else if pos < source.span().low() {
                    Ordering::Greater
                } else {
                    Ordering::Equal
                }
            })
            .ok()
            .map(|index| &self.sources[index])
            .expect("wrong position")
    }

    pub fn lookup_pos(&self, pos: Pos) -> Loc {
        let source = self.find_source(pos);
        Loc::new(source.clone(), source.find_line_col(pos))
    }

    pub fn lookup_span(&self, span: Span) -> SpanLoc {
        let source = self.find_source(span.low());
        SpanLoc::new(
            source.clone(),
            source.find_line_col(span.low()),
            source.find_line_col(span.high()),
        )
    }

    pub fn add_source(&mut self, name: String, path: SourcePath, content: String) -> Arc<Source> {
        let low = self.end_pos();
        let high = low.offset(content.len() as _);
        let source = Arc::new(Source::new(Span::new(low, high), name, path, content));
        self.sources.push(source.clone());
        source
    }
}
