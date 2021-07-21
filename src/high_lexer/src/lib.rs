mod symbols;
mod token;

pub use token::*;

use low_lexer::{token_iter as low_token_iter, Token as LowToken, TokenKind as LowTokenKind};
use std::iter::from_fn as iter_from_fn;

// pub fn token_iter(mut input: &str) -> impl Iterator<Item = Token> + '_ {
//     let mut iter = low_token_iter(input);
//     let mut token = iter.next();
//     let mut next = iter.next();

//     iter_from_fn(move || {
//         let mut token = match token {
//             Some(token) => token,
//             None => return None,
//         };

// 		if let Some(next) = next {
// 			match token.glue()
// 		}

//         token = next;
//         next = iter.next();
//     })
// }

fn convert(token: LowToken) -> Token {
    match token.kind() {
        LowTokenKind::Error(_) => todo!(),
        LowTokenKind::Whitespace => todo!(),
        LowTokenKind::Comment => todo!(),
        LowTokenKind::OpenParen => todo!(),
        LowTokenKind::CloseParen => todo!(),
        LowTokenKind::OpenBrace => todo!(),
        LowTokenKind::CloseBrace => todo!(),
        LowTokenKind::OpenBracket => todo!(),
        LowTokenKind::CloseBracket => todo!(),
        LowTokenKind::Dot => todo!(),
        LowTokenKind::Comma => todo!(),
        LowTokenKind::Semicolon => todo!(),
        LowTokenKind::Eq => todo!(),
        LowTokenKind::Bang => todo!(),
        LowTokenKind::Ls => todo!(),
        LowTokenKind::Gt => todo!(),
        LowTokenKind::Plus => todo!(),
        LowTokenKind::Minus => todo!(),
        LowTokenKind::Star => todo!(),
        LowTokenKind::Slash => todo!(),
        LowTokenKind::Percent => todo!(),
        LowTokenKind::Or => todo!(),
        LowTokenKind::And => todo!(),
        LowTokenKind::Caret => todo!(),
        LowTokenKind::Tilde => todo!(),
        LowTokenKind::Id => todo!(),
        LowTokenKind::Literal(_) => todo!(),
    }
}
