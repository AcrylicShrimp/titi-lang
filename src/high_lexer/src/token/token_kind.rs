use crate::{TokenBinaryOpKind, TokenLiteral, TokenUnaryOpKind};
use str_interner::StrIdx;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    Literal(TokenLiteral),
    Id(StrIdx),
}

impl TokenKind {
    pub fn binary_op(op: TokenBinaryOpKind) -> Self {
        Self::BinaryOp(op)
    }

    pub fn unary_op(op: TokenUnaryOpKind) -> Self {
        Self::UnaryOp(op)
    }

    pub fn literal(literal: TokenLiteral) -> Self {
        Self::Literal(literal)
    }

    pub fn id<S: AsRef<str>>(id: S) -> Self {
        Self::Id(StrIdx::intern(id))
    }
}
