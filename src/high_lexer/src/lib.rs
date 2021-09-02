mod symbols;
mod token;

pub use token::*;

use low_lexer::{
    token_iter as low_token_iter, Token as LowToken,
    TokenIntegerLiteralKind as LowTokenIntegerLiteralKind, TokenKind as LowTokenKind,
    TokenLiteralKind as LowTokenLiteralKind, TokenNumberLiteralKind as LowTokenNumberLiteralKind,
};
use span::{Pos, Source, Span};
use std::iter::from_fn as iter_from_fn;
use str_interner::StrIdx;

pub fn token_iter(source: &Source) -> impl Iterator<Item = Token> + '_ {
    let mut iter = unglued_token_iter(source);
    let mut current = iter.next();
    let mut next = iter.next();

    iter_from_fn(move || {
        let mut token = match current.take() {
            Some(token) => token,
            None => return None,
        };

        while let Some(next_token) = next.take() {
            if let Some(glued) = token.glue(&next_token) {
                next = iter.next();
                token = glued;
            } else {
                next = Some(next_token);
                break;
            }
        }

        current = next.take();
        next = iter.next();
        Some(token)
    })
}

fn unglued_token_iter(source: &Source) -> impl Iterator<Item = Token> + '_ {
    let mut low = source.span().low();
    let mut iter = low_token_iter(source.content());

    iter_from_fn(move || loop {
        let token = match iter.next() {
            Some(token) => token,
            None => return None,
        };
        let length = token.len();
        let token = convert(token, low, source);

        low = low.offset(length as _);

        match token {
            Some(token) => {
                return Some(token);
            }
            None => {}
        }
    })
}

macro_rules! literal {
    ($kind:expr, $str:expr, $suffix:expr) => {
        TokenKind::Literal(TokenLiteral::new(
            $kind,
            StrIdx::intern($str),
            $suffix.map(|suffix: &str| StrIdx::intern(suffix)),
        ))
    };
}

// TODO: Add diagnostics for error reporting.
fn convert(token: LowToken, low: Pos, source: &Source) -> Option<Token> {
    let span = Span::new(low, low.offset(token.len() as _));

    Some(Token::new(
        match token.kind() {
            LowTokenKind::Unknown | LowTokenKind::Whitespace => {
                return None;
            }
            LowTokenKind::Comment => TokenKind::Comment,
            LowTokenKind::OpenParen => TokenKind::OpenParen,
            LowTokenKind::CloseParen => TokenKind::CloseParen,
            LowTokenKind::OpenBrace => TokenKind::OpenBrace,
            LowTokenKind::CloseBrace => TokenKind::CloseBrace,
            LowTokenKind::OpenBracket => TokenKind::OpenBracket,
            LowTokenKind::CloseBracket => TokenKind::CloseBracket,
            LowTokenKind::Dot => TokenKind::Dot,
            LowTokenKind::Comma => TokenKind::Comma,
            LowTokenKind::Semicolon => TokenKind::Semicolon,
            LowTokenKind::Eq => TokenKind::Assign,
            LowTokenKind::Bang => TokenKind::LogNot,
            LowTokenKind::Lt => TokenKind::Lt,
            LowTokenKind::Gt => TokenKind::Gt,
            LowTokenKind::Plus => TokenKind::Add,
            LowTokenKind::Minus => TokenKind::Sub,
            LowTokenKind::Star => TokenKind::Mul,
            LowTokenKind::Slash => TokenKind::Div,
            LowTokenKind::Percent => TokenKind::Mod,
            LowTokenKind::Or => TokenKind::BitOr,
            LowTokenKind::And => TokenKind::BitAnd,
            LowTokenKind::Caret => TokenKind::BitXor,
            LowTokenKind::Tilde => TokenKind::BitNot,
            LowTokenKind::Id => match source.slice(span) {
                "true" => literal!(TokenLiteralKind::Bool, "true", None),
                "false" => literal!(TokenLiteralKind::Bool, "false", None),
                id => TokenKind::id(id),
            },
            LowTokenKind::Literal(literal) => {
                let str = source.slice(span);

                match literal {
                    LowTokenLiteralKind::Number(number) => literal!(
                        match number.kind() {
                            LowTokenNumberLiteralKind::Integer(integer) => match integer {
                                LowTokenIntegerLiteralKind::Binary =>
                                    TokenLiteralKind::IntegerBinary,
                                LowTokenIntegerLiteralKind::Octal => TokenLiteralKind::IntegerOctal,
                                LowTokenIntegerLiteralKind::Hexadecimal =>
                                    TokenLiteralKind::IntegerHexadecimal,
                                LowTokenIntegerLiteralKind::Decimal =>
                                    TokenLiteralKind::IntegerDecimal,
                            },
                            LowTokenNumberLiteralKind::Float => TokenLiteralKind::Float,
                        },
                        &str[..number.suffix_start()],
                        if number.suffix_start() == str.len() {
                            None
                        } else {
                            Some(&str[number.suffix_start()..])
                        }
                    ),
                    LowTokenLiteralKind::SingleQuotedStr(..) => {
                        literal!(TokenLiteralKind::SingleQuotedStr, str, None)
                    }
                    LowTokenLiteralKind::DoubleQuotedStr(..) => {
                        literal!(TokenLiteralKind::DoubleQuotedStr, str, None)
                    }
                }
            }
        },
        span,
    ))
}
