use crate::{Symbol, TokenLiteral, STR_INTERNER};

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
    // Assignment operators
    Assign,       // "="
    AssignAdd,    // "+="
    AssignSub,    // "-="
    AssignMul,    // "*="
    AssignDiv,    // "/="
    AssignMod,    // "%="
    AssignShl,    // "<<="
    AssignShr,    // ">>="
    AssignBitOr,  // "|="
    AssignBitAnd, // "&="
    AssignBitXor, // "^="
    AssignBitNot, // "~="
    // Range operators
    Rng,          // ".."
    RngInclusive, // "..="
    // Cmp operators
    Eq, // "=="
    Ne, // "!="
    Lt, // "<"
    Gt, // ">"
    Le, // "<="
    Ge, // ">="
    // Binary operators
    Add,    // "+"
    Sub,    // "-"
    Mul,    // "*"
    Div,    // "/"
    Mod,    // "%"
    Shl,    // "<<"
    Shr,    // ">>"
    BitOr,  // "|"
    BitAnd, // "&"
    BitXor, // "^"
    LogOr,  // "||"
    LogAnd, // "&&"
    // Unary operators
    BitNot, // "~"
    LogNot, // "!"
    Id(Symbol),
    Literal(TokenLiteral),
}

impl TokenKind {
    pub fn id<S: AsRef<str>>(id: S) -> Self {
        Self::Id(Symbol(STR_INTERNER.lock().intern(id)))
    }

    // TODO: Make the output more readable
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Comment => "comment",
            Self::OpenParen => "'('",
            Self::CloseParen => "')'",
            Self::OpenBrace => "'{'",
            Self::CloseBrace => "'}'",
            Self::OpenBracket => "'['",
            Self::CloseBracket => "']'",
            Self::Dot => "'.'",
            Self::Comma => "','",
            Self::Semicolon => "';'",
            Self::Assign => "'='",
            Self::AssignAdd => "'+='",
            Self::AssignSub => "'-='",
            Self::AssignMul => "'*='",
            Self::AssignDiv => "'/='",
            Self::AssignMod => "'%='",
            Self::AssignShl => "'<<='",
            Self::AssignShr => "'>>='",
            Self::AssignBitOr => "'|='",
            Self::AssignBitAnd => "'&='",
            Self::AssignBitXor => "'^='",
            Self::AssignBitNot => "'~='",
            Self::Rng => "'..'",
            Self::RngInclusive => "'..='",
            Self::Eq => "'=='",
            Self::Ne => "'!='",
            Self::Lt => "'<'",
            Self::Gt => "'>'",
            Self::Le => "'<='",
            Self::Ge => "'>='",
            Self::Add => "'+'",
            Self::Sub => "'-'",
            Self::Mul => "'*'",
            Self::Div => "'/'",
            Self::Mod => "'%'",
            Self::Shl => "'<<'",
            Self::Shr => "'>>'",
            Self::BitOr => "'|'",
            Self::BitAnd => "'&'",
            Self::BitXor => "'^'",
            Self::LogOr => "'||'",
            Self::LogAnd => "'&&'",
            Self::BitNot => "'~'",
            Self::LogNot => "'!'",
            Self::Id(..) => "identifier",
            Self::Literal(..) => "literal",
        }
    }
}
