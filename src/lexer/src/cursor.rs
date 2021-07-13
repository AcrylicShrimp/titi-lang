use std::collections::VecDeque;
use std::str::Chars;

pub struct Cursor<'s> {
    chars: Chars<'s>,
    buffers: VecDeque<char>,
}

impl<'s> Cursor<'s> {
    pub fn new(src: &'s str) -> Self {
        Self {
            chars: src.chars(),
            buffers: VecDeque::new(),
        }
    }

    pub fn lookup(&mut self, offset: usize) -> Option<char> {
        while self.buffers.len() <= offset {
            self.buffers.push_back(match self.chars.next() {
                Some(char) => char,
                None => return None,
            });
        }

        Some(self.buffers[offset])
    }

    pub fn consume(&mut self, mut count: usize) {
        loop {
            match self.buffers.pop_front() {
                Some(..) => {
                    count -= 1;
                }
                None => break,
            }
        }

        while count != 0 {
            match self.chars.next() {
                Some(..) => {
                    count -= 1;
                }
                None => break,
            }
        }
    }
}
