mod symbols;
mod token;

pub use symbols::*;
pub use token::*;

use diagnostic::{Diagnostic, Level, MultiSpan};
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

    check_low_token(&token, span, source);

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

fn check_low_token(token: &LowToken, span: Span, source: &Source) {
    match token.kind() {
        LowTokenKind::Unknown => Diagnostic::push_new(Diagnostic::new(
            Level::Error,
            format!("unknown token '{}'", source.slice(span)),
            MultiSpan::with_spans(vec![(
                format!("'{}' is not allowed", source.slice(span)),
                Some(span),
            )]),
        )),
        LowTokenKind::Whitespace => {}
        LowTokenKind::Comment => {}
        LowTokenKind::OpenParen => {}
        LowTokenKind::CloseParen => {}
        LowTokenKind::OpenBrace => {}
        LowTokenKind::CloseBrace => {}
        LowTokenKind::OpenBracket => {}
        LowTokenKind::CloseBracket => {}
        LowTokenKind::Dot => {}
        LowTokenKind::Comma => {}
        LowTokenKind::Semicolon => {}
        LowTokenKind::Eq => {}
        LowTokenKind::Bang => {}
        LowTokenKind::Lt => {}
        LowTokenKind::Gt => {}
        LowTokenKind::Plus => {}
        LowTokenKind::Minus => {}
        LowTokenKind::Star => {}
        LowTokenKind::Slash => {}
        LowTokenKind::Percent => {}
        LowTokenKind::Or => {}
        LowTokenKind::And => {}
        LowTokenKind::Caret => {}
        LowTokenKind::Tilde => {}
        LowTokenKind::Id => {}
        LowTokenKind::Literal(literal) => match literal {
            LowTokenLiteralKind::Number(number) => {
                if number.suffix_start() != token.len() {
                    let suffix = &source.slice(span)[number.suffix_start()..];

                    // TODO: Perform value overflow check.

                    match number.kind() {
                        LowTokenNumberLiteralKind::Integer(integer) => match suffix {
                            suffix if is_integer_suffix(suffix) => {}
                            suffix if is_float_suffix(suffix) => {
                                if !integer.is_decimal() {
                                    Diagnostic::push_new(Diagnostic::new(
                                        Level::Error,
                                        format!("invalid use of float suffix '{}'", suffix),
                                        MultiSpan::with_spans(vec![
                                            (
                                                format!("float suffix '{}' is not allowed for non-decimal integer literals", suffix),
                                                Some(Span::new(
                                                    span.low().offset(number.suffix_start() as _),
                                                    span.high(),
                                                )),
                                            ),
                                            (format!("consider use integer suffix or remove it"), None),
                                        ]),
                                    ));
                                }
                            }
                            suffix => {
                                Diagnostic::push_new(Diagnostic::new(
                                    Level::Error,
                                    format!("invalid suffix '{}'", suffix),
                                    MultiSpan::with_spans(vec![
                                        (
                                            format!("'{}' is not valid suffix", suffix),
                                            Some(Span::new(
                                                span.low().offset(number.suffix_start() as _),
                                                span.high(),
                                            )),
                                        ),
                                        (format!("consider use integer suffix or remove it"), None),
                                    ]),
                                ));
                            }
                        },
                        LowTokenNumberLiteralKind::Float => match suffix {
                            suffix if is_float_suffix(suffix) => {}
                            suffix if is_integer_suffix(suffix) => {
                                Diagnostic::push_new(Diagnostic::new(
                                    Level::Error,
                                    format!("invalid use of integer suffix '{}'", suffix),
                                    MultiSpan::with_spans(vec![
                                        (
                                            format!("integer suffix '{}' is not allowed for float literals", suffix),
                                            Some(Span::new(
                                                span.low().offset(number.suffix_start() as _),
                                                span.high(),
                                            )),
                                        ),
                                        (format!("consider use 'f64' or remove it"), None),
                                    ]),
                                ));
                            }
                            suffix => {
                                Diagnostic::push_new(Diagnostic::new(
                                    Level::Error,
                                    format!("invalid suffix '{}'", suffix),
                                    MultiSpan::with_spans(vec![
                                        (
                                            format!("'{}' is not valid suffix", suffix),
                                            Some(Span::new(
                                                span.low().offset(number.suffix_start() as _),
                                                span.high(),
                                            )),
                                        ),
                                        (format!("consider use 'f64' or remove it"), None),
                                    ]),
                                ));
                            }
                        },
                    }
                }
            }
            LowTokenLiteralKind::SingleQuotedStr(str) => {
                // TODO: Detect long-length literals and emit diagnostics for it.
                if !str.terminated() {
                    Diagnostic::push_new(Diagnostic::new(
                        Level::Error,
                        format!("single quoted literal is not closed"),
                        MultiSpan::with_spans(vec![
                            (format!("' is missing"), Some(span)),
                            (format!("add ' at the end of the literal"), None),
                        ]),
                    ));
                }
            }
            LowTokenLiteralKind::DoubleQuotedStr(str) => {
                if !str.terminated() {
                    Diagnostic::push_new(Diagnostic::new(
                        Level::Error,
                        format!("double quoted literal is not closed"),
                        MultiSpan::with_spans(vec![
                            (format!("\" is missing"), Some(span)),
                            (format!("add \" at the end of the literal"), None),
                        ]),
                    ));
                }
            }
        },
    }
}

fn is_integer_suffix(suffix: &str) -> bool {
    match suffix {
        "byte" | "char" | "i64" | "u64" | "isize" | "usize" => true,
        _ => false,
    }
}

fn is_float_suffix(suffix: &str) -> bool {
    match suffix {
        "f64" => true,
        _ => false,
    }
}
