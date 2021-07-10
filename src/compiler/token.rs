#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    index: usize,
    line: usize,
    line_offset: usize,
    ty: TokenType,
    content: String,
}

impl Token {
    pub fn new(
        index: usize,
        line: usize,
        line_offset: usize,
        ty: TokenType,
        content: String,
    ) -> Token {
        Token {
            index,
            line,
            line_offset,
            ty,
            content,
        }
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn line_offset(&self) -> usize {
        self.line_offset
    }

    pub fn ty(&self) -> &TokenType {
        &self.ty
    }

    pub fn content(&self) -> &String {
        &self.content
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    Error(TokenError),
    Eof,               // EOF
    Comment,           // #
    PuncParenL,        // (
    PuncParenR,        // )
    PuncBraceL,        // {
    PuncBraceR,        // }
    PuncBracketL,      // [
    PuncBracketR,      // ]
    PuncDot,           // .
    PuncComma,         // ,
    PuncSemicolon,     // ;
    OpAssign,          // =
    OpAssignAdd,       // +=
    OpAssignSub,       // -=
    OpAssignMul,       // *=
    OpAssignDiv,       // /=
    OpAssignMod,       // %=
    OpAssignBitShiftL, // <<=
    OpAssignBitShiftR, // >>=
    OpAssignBitOr,     // |=
    OpAssignBitAnd,    // &=
    OpAssignBitXor,    // ^=
    OpAssignBitNot,    // ~=
    OpLogOr,           // or
    OpLogAnd,          // and
    OpLogNot,          // not
    OpCmpEq,           // ==
    OpCmpNeq,          // !=
    OpCmpLs,           // <
    OpCmpLsEq,         // <=
    OpCmpGt,           // >
    OpCmpGtEq,         // >=
    OpAdd,             // +
    OpSub,             // -
    OpMul,             // *
    OpDiv,             // /
    OpMod,             // %
    OpBitShiftL,       // <<
    OpBitShiftR,       // >>
    OpBitOr,           // |
    OpBitAnd,          // &
    OpBitXor,          // ^
    OpBitNot,          // ~
    KeywordBool,       // bool
    KeywordByte,       // byte
    KeywordChar,       // char
    KeywordI64,        // i64
    KeywordU64,        // u64
    KeywordIsize,      // isize
    KeywordUsize,      // usize
    KeywordF32,        // f32
    KeywordF64,        // f64
    KeywordCptr,       // cptr
    KeywordMptr,       // mptr
    KeywordUse,        // use
    KeywordLet,        // let
    KeywordFn,         // fn
    KeywordIf,         // if
    KeywordElse,       // else
    KeywordFor,        // for
    KeywordIn,         // in
    KeywordAs,         // as
    LiteralBool(bool),
    LiteralByte(String),
    LiteralChar(char),
    LiteralI64(String),
    LiteralU64(String),
    LiteralIsize(String),
    LiteralUsize(String),
    LiteralF64(String),
    LiteralInteger(String),
    LiteralStr(String),
    Id,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenError {
    Unknown,
    CharLiteralNotTerminated,
    CharLiteralEmpty,
    CharLiteralTooLong,
    StrLiteralNotTerminated,
    F64LiteralTerminatedWithDot,
}
