use crate::{TokenBinaryOpKind, TokenUnaryOpKind};

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
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
    BinaryOp(TokenBinaryOpKind),
    UnaryOp(TokenUnaryOpKind),
}
