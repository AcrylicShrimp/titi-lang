use crate::compiler::{Token, TokenType};

pub struct TokenBuilder {
    index: usize,
    line: usize,
    line_offset: usize,
}

impl TokenBuilder {
    pub fn new(index: usize, line: usize, line_offset: usize) -> TokenBuilder {
        TokenBuilder {
            index,
            line,
            line_offset,
        }
    }

    pub fn build(self, ty: TokenType, content: String) -> Token {
        Token::new(self.index, self.line, self.line_offset, ty, content)
    }
}
