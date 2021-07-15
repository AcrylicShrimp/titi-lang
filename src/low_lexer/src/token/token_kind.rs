use crate::TokenErrorKind;
use crate::TokenLiteralKind;

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
    Error(TokenErrorKind),
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
    Semicolon,    // ";"
    Eq,           // "="
    Bang,         // "!"
    Ls,           // "<"
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