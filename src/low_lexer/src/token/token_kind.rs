use crate::TokenLiteralKind;

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
    Unknown,
    Whitespace,
    Comment,      // "#"
    OpenParen,    // "("
    CloseParen,   // ")"
    OpenBrace,    // "{"
    CloseBrace,   // "}"
    OpenBracket,  // "["
    CloseBracket, // "]"
    Dot,          // "."
    Comma,        // ","
    Colon,        // ":"
    Semicolon,    // ";"
    Eq,           // "="
    Bang,         // "!"
    Lt,           // "<"
    Gt,           // ">"
    Plus,         // "+"
    Minus,        // "-"
    Star,         // "*"
    Slash,        // "/"
    Percent,      // "%"
    Or,           // "|"
    And,          // "&"
    Caret,        // "^"
    Tilde,        // "~"
    Id,           // identifier or keyword
    Literal(TokenLiteralKind),
}
