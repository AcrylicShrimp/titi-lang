use crate::{Cursor, Token};
use std::iter::Iterator;

pub struct Lexer<'s> {
    cursor: Cursor<'s>,
}

impl<'s> Lexer<'s> {
    pub fn new(cursor: Cursor<'s>) -> Self {
        Lexer { cursor }
    }
}

impl<'s> Iterator for Lexer<'s> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let char = match self.cursor.lookup(0) {
            Some(char) => char,
            None => return None,
        };

        if char.is_whitespace() {
            self.cursor.consume(1);
            return self.next();
        }

        match char {
            '#' => while let Some(char) = self.cursor.lookup(0) {},
            _ => {}
        }

        None
    }
}
