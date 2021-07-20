use crate::symbols::str as symbol_strs;
use crate::{TokenBinaryOpKind, TokenLiteral, TokenUnaryOpKind};
use lazy_static::lazy_static;
use parking_lot::Mutex;
use str_interner::{StrIdx, StrInterner};

lazy_static! {
    static ref STR_INTERNER: Mutex<StrInterner> = StrInterner::with_prefilled(&[
        symbol_strs::EMPTY,
        symbol_strs::BOOL,
        symbol_strs::BYTE,
        symbol_strs::CHAR,
        symbol_strs::I64,
        symbol_strs::U64,
        symbol_strs::ISIZE,
        symbol_strs::USIZE,
        symbol_strs::F64,
        symbol_strs::STR,
        symbol_strs::CPTR,
        symbol_strs::MPTR,
        symbol_strs::IF,
        symbol_strs::ELSE,
        symbol_strs::FOR,
        symbol_strs::IN,
        symbol_strs::AS,
    ])
    .into();
}

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
        Self::Id(STR_INTERNER.lock().intern(id.as_ref()))
    }
}
