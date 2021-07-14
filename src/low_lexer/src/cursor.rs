use std::str::Chars;

pub struct Cursor<'s> {
    chars: Chars<'s>,
    initial_length: usize,
}

impl<'s> Cursor<'s> {
    pub fn new(src: &'s str) -> Self {
        Self {
            chars: src.chars(),
            initial_length: src.len(),
        }
    }

    pub fn len_consumed(&self) -> usize {
        self.initial_length - self.chars.as_str().len()
    }

    pub fn first(&self) -> char {
        self.lookup(0)
    }

    pub fn second(&self) -> char {
        self.lookup(1)
    }

    pub fn lookup(&self, offset: usize) -> char {
        self.chars.clone().nth(offset).unwrap_or('\0')
    }

    pub fn consume(&mut self) -> Option<char> {
        self.chars.next()
    }
}
