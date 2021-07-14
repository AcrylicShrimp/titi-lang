mod cursor;
mod token;

pub use token::*;

use cursor::*;
use std::iter::from_fn as iter_from_fn;
use unicode_xid::UnicodeXID;

pub fn token_iter(mut input: &str) -> impl Iterator<Item = Token> + '_ {
    iter_from_fn(move || {
        if input.is_empty() {
            return None;
        }

        let token = next(input);
        input = &input[token.len()..];
        Some(token)
    })
}

fn next(input: &str) -> Token {
    let mut cursor = Cursor::new(input);
    let kind = match cursor.consume().unwrap() {
        char if char.is_whitespace() => {
            consume_while(&mut cursor, |char| char.is_whitespace());
            TokenKind::Whitespace
        }
        char if is_id_start(char) => {
            consume_while(&mut cursor, |char| is_id_continue(char));
            TokenKind::Id
        }
        char @ '0'..='9' => {
            let kind = consume_number(&mut cursor, char);
            let suffix_start = cursor.len_consumed();

            if is_id_start(cursor.first()) {
                cursor.consume();
                consume_while(&mut cursor, |char| is_id_continue(char));
            }

            TokenKind::Literal(TokenLiteralKind::Number(TokenNumberLiteral::new(
                kind,
                suffix_start,
            )))
        }
        '#' => {
            consume_while(&mut cursor, |char| char != '\n');
            TokenKind::Comment
        }
        '(' => TokenKind::OpenParen,
        ')' => TokenKind::CloseParen,
        '{' => TokenKind::OpenBrace,
        '}' => TokenKind::CloseBrace,
        '[' => TokenKind::OpenBracket,
        ']' => TokenKind::CloseBracket,
        '.' => TokenKind::Dot,
        ',' => TokenKind::Comma,
        ';' => TokenKind::Semicolon,
        '=' => TokenKind::Eq,
        '!' => TokenKind::Bang,
        '<' => TokenKind::Ls,
        '>' => TokenKind::Gt,
        '+' => TokenKind::Plus,
        '-' => TokenKind::Minus,
        '*' => TokenKind::Star,
        '/' => TokenKind::Slash,
        '%' => TokenKind::Percent,
        '|' => TokenKind::Or,
        '&' => TokenKind::And,
        '^' => TokenKind::Caret,
        '~' => TokenKind::Tilde,
        '\'' => TokenKind::Literal(TokenLiteralKind::SingleQuotedStr(TokenStrLiteral::new(
            consume_single_quoted(&mut cursor),
        ))),
        '"' => TokenKind::Literal(TokenLiteralKind::DoubleQuotedStr(TokenStrLiteral::new(
            consume_double_quoted(&mut cursor),
        ))),
        _ => TokenKind::Error(TokenErrorKind::InvalidCharacter),
    };

    Token::new(kind, cursor.len_consumed())
}

fn consume_while(cursor: &mut Cursor, mut pred: impl FnMut(char) -> bool) {
    while let Some(c) = cursor.consume() {
        if !pred(c) {
            break;
        }
    }
}

fn is_id_start(char: char) -> bool {
    ('a'..'z').contains(&char)
        || ('A'..'Z').contains(&char)
        || (char == '_')
        || (char > '\x7f' && UnicodeXID::is_xid_start(char))
}

fn is_id_continue(char: char) -> bool {
    ('a'..'z').contains(&char)
        || ('A'..'Z').contains(&char)
        || ('0'..'9').contains(&char)
        || (char == '_')
        || (char > '\x7f' && UnicodeXID::is_xid_continue(char))
}

fn consume_number(cursor: &mut Cursor, first_char: char) -> TokenNumberLiteralKind {
	if first_char== '0' {
		
    TokenNumberLiteralKind::Float
}

fn consume_single_quoted(cursor: &mut Cursor) -> bool {
    if cursor.first() != '\\' && cursor.second() == '\'' {
        cursor.consume();
        cursor.consume();
        return true;
    }

    loop {
        match cursor.first() {
            '\'' => {
                cursor.consume();
                return true;
            }
            '\\' => {
                cursor.consume();
                cursor.consume();
            }
            '#' => break,
            '\0' => break,
            '\n' if cursor.second() != '\'' => break,
            _ => {
                cursor.consume();
            }
        }
    }

    false
}

fn consume_double_quoted(cursor: &mut Cursor) -> bool {
    while let Some(char) = cursor.consume() {
        match char {
            '"' => return true,
            '\\' => {
                cursor.consume();
            }
            _ => {}
        }
    }

    false
}
