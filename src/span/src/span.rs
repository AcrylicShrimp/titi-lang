use std::fmt::{Display, Formatter, Result as FmtResult};

pub struct Span<'s> {
    line: usize,
    line_offset: usize,
    source: &'s str,
}

impl<'s> Span<'s> {
    pub fn new(line: usize, line_offset: usize, source: &'s str) -> Self {
        Self {
            line,
            line_offset,
            source,
        }
    }
}

impl<'s> Display for Span<'s> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let lines = self.source.split('\n');

        let mut line_number = 0;
        let max_line_number = lines.clone().count();
        let mut count = 0;
        let max_count = self.source.len();

        let line_number_width = ((self.line + max_line_number) as f64).log10().floor() as usize;

        for line in self.source.split('\n') {
            writeln!(
                f,
                "{:>line_number_width$} : ",
                self.line + line_number,
                line_number_width = line_number_width
            )?;

            let (offset, length) = if line_number == 0 {
                (0, 0)
            } else if line_number + 1 == max_line_number {
                (0, 0)
            } else {
                (0, line.len())
            };

            writeln!(
                f,
                "{:offset$}{}",
                "^".repeat(length),
                offset = line_number_width + 3 + offset
            )?;

            line_number += 1;
            count += line.len() + 1;
        }

        Ok(())
    }
}
