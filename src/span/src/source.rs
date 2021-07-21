use crate::{LineCol, Pos, SourcePath, Span};
use std::fmt::Debug;
use std::hash::{Hash, Hasher};

pub struct Source {
    span: Span,
    name: String,
    path: SourcePath,
    content: String,
    lines: Vec<Pos>,
}

impl Source {
    pub fn new(span: Span, name: String, path: SourcePath, content: String) -> Self {
        let mut lines = vec![span.low()];
        lines.extend(
            content
                .match_indices('\n')
                .map(|(pos, _)| Pos::new(span.low() + Pos::new(pos as u32 + 1))),
        );

        Self {
            span,
            name,
            path,
            content,
            lines,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn path(&self) -> &SourcePath {
        &self.path
    }

    pub fn content(&self) -> &str {
        &self.content
    }

    pub fn lines(&self) -> &Vec<Pos> {
        &self.lines
    }

    pub fn line_num(&self) -> u32 {
        self.lines.len() as _
    }

    pub fn line_span(&self, line: u32) -> Span {
        Span::new(self.lines[line as usize], self.lines[line as usize + 1])
    }

    pub fn find_line(&self, pos: Pos) -> u32 {
        match self.lines.binary_search(&pos) {
            Ok(line) => line as u32,
            Err(line) => line as u32 - 1,
        }
    }

    pub fn find_line_col(&self, pos: Pos) -> LineCol {
        let line = self.find_line(pos);
        let line_span = self.line_span(line);
        LineCol::new(
            line,
            self.slice(line_span)[..(pos - line_span.low()) as usize]
                .chars()
                .count() as _,
        )
    }

    pub fn slice(&self, span: Span) -> &str {
        &self.content
            [((span.low() - self.span.low()) as usize)..((span.high() - self.span.low()) as usize)]
    }

    pub fn slice_line(&self, line: u32) -> &str {
        let chars: &[_] = &['\n', '\r'];
        self.slice(self.line_span(line)).trim_end_matches(chars)
    }
}

impl Debug for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Source({:?})", self.name)
    }
}

impl PartialEq for Source {
    fn eq(&self, other: &Self) -> bool {
        self as *const _ == other as *const _
    }
}

impl Eq for Source {}

impl Hash for Source {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.span.hash(state);
    }
}
