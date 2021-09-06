use crate::TokenKind;
use span::Span;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    kind: TokenKind,
    span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn glue(&self, next: &Token) -> Option<Token> {
        let kind = match self.kind {
            TokenKind::Dot => match next.kind {
                TokenKind::Dot => TokenKind::Rng,
                _ => return None,
            },
            TokenKind::Rng => match next.kind {
                TokenKind::Assign => TokenKind::RngInclusive,
                _ => return None,
            },
            TokenKind::Assign => match next.kind {
                TokenKind::Assign => TokenKind::Eq,
                _ => return None,
            },
            TokenKind::Lt => match next.kind {
                TokenKind::Assign => TokenKind::Le,
                TokenKind::Lt => TokenKind::Shl,
                _ => return None,
            },
            TokenKind::Gt => match next.kind {
                TokenKind::Assign => TokenKind::Ge,
                TokenKind::Gt => TokenKind::Shr,
                _ => return None,
            },
            TokenKind::Add => match &next.kind {
                TokenKind::Assign => TokenKind::AssignAdd,
                _ => return None,
            },
            TokenKind::Sub => match &next.kind {
                TokenKind::Assign => TokenKind::AssignSub,
                _ => return None,
            },
            TokenKind::Mul => match next.kind {
                TokenKind::Assign => TokenKind::AssignMul,
                _ => return None,
            },
            TokenKind::Div => match next.kind {
                TokenKind::Assign => TokenKind::AssignDiv,
                _ => return None,
            },
            TokenKind::Mod => match next.kind {
                TokenKind::Assign => TokenKind::AssignMod,
                _ => return None,
            },
            TokenKind::Shl => match next.kind {
                TokenKind::Assign => TokenKind::AssignShl,
                _ => return None,
            },
            TokenKind::Shr => match next.kind {
                TokenKind::Assign => TokenKind::AssignShr,
                _ => return None,
            },
            TokenKind::BitOr => match next.kind {
                TokenKind::Assign => TokenKind::AssignBitOr,
                TokenKind::BitOr => TokenKind::LogOr,
                _ => return None,
            },
            TokenKind::BitAnd => match next.kind {
                TokenKind::Assign => TokenKind::AssignBitAnd,
                TokenKind::BitAnd => TokenKind::LogAnd,
                _ => return None,
            },
            TokenKind::BitXor => match next.kind {
                TokenKind::Assign => TokenKind::AssignBitXor,
                _ => return None,
            },
            TokenKind::BitNot => match next.kind {
                TokenKind::Assign => TokenKind::AssignBitNot,
                _ => return None,
            },
            TokenKind::LogNot => match next.kind {
                TokenKind::Assign => TokenKind::Ne,
                _ => return None,
            },
            TokenKind::Comment
            | TokenKind::OpenParen
            | TokenKind::CloseParen
            | TokenKind::OpenBrace
            | TokenKind::CloseBrace
            | TokenKind::OpenBracket
            | TokenKind::CloseBracket
            | TokenKind::Comma
            | TokenKind::Semicolon
            | TokenKind::AssignAdd
            | TokenKind::AssignSub
            | TokenKind::AssignMul
            | TokenKind::AssignDiv
            | TokenKind::AssignMod
            | TokenKind::AssignShl
            | TokenKind::AssignShr
            | TokenKind::AssignBitOr
            | TokenKind::AssignBitAnd
            | TokenKind::AssignBitXor
            | TokenKind::AssignBitNot
            | TokenKind::RngInclusive
            | TokenKind::Eq
            | TokenKind::Ne
            | TokenKind::Le
            | TokenKind::Ge
            | TokenKind::LogOr
            | TokenKind::LogAnd
            | TokenKind::Literal(..)
            | TokenKind::Id(..) => return None,
        };

        Some(Token::new(kind, self.span.to(next.span)))
    }
}
