use crate::*;

#[test]
fn simple_test_punctuations() {
    let mut iter =
        token_iter("@@@   ((())){{{}}}[[[]]]...,,,:::;;;===!!!<<<>>>+++---***///%%%|||&&&^^^~~~");

    assert_eq!(iter.next(), Some(Token::new(TokenKind::Unknown, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Unknown, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Unknown, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Whitespace, 3)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::OpenParen, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::OpenParen, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::OpenParen, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::CloseParen, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::CloseParen, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::CloseParen, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::OpenBrace, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::OpenBrace, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::OpenBrace, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::CloseBrace, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::CloseBrace, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::CloseBrace, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::OpenBracket, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::OpenBracket, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::OpenBracket, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::CloseBracket, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::CloseBracket, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::CloseBracket, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Dot, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Dot, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Dot, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Comma, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Comma, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Comma, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Colon, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Colon, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Colon, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Semicolon, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Semicolon, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Semicolon, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Eq, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Eq, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Eq, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Bang, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Bang, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Bang, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Lt, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Lt, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Lt, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Gt, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Gt, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Gt, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Plus, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Plus, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Plus, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Minus, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Minus, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Minus, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Star, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Star, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Star, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Slash, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Slash, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Slash, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Percent, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Percent, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Percent, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Or, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Or, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Or, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::And, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::And, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::And, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Caret, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Caret, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Caret, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Tilde, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Tilde, 1)));
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Tilde, 1)));
    assert_eq!(iter.next(), None);
}

#[test]
fn simple_test_literals() {
    let mut iter = token_iter("3 3 3 3 3");

    assert_eq!(
        iter.next(),
        Some(Token::new(
            TokenKind::Literal(TokenLiteralKind::Number(TokenNumberLiteral::new(
                TokenNumberLiteralKind::Integer(TokenIntegerLiteralKind::Decimal),
                1
            ))),
            1
        ))
    );
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Whitespace, 1)));
    assert_eq!(
        iter.next(),
        Some(Token::new(
            TokenKind::Literal(TokenLiteralKind::Number(TokenNumberLiteral::new(
                TokenNumberLiteralKind::Integer(TokenIntegerLiteralKind::Decimal),
                1
            ))),
            1
        ))
    );
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Whitespace, 1)));
    assert_eq!(
        iter.next(),
        Some(Token::new(
            TokenKind::Literal(TokenLiteralKind::Number(TokenNumberLiteral::new(
                TokenNumberLiteralKind::Integer(TokenIntegerLiteralKind::Decimal),
                1
            ))),
            1
        ))
    );
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Whitespace, 1)));
    assert_eq!(
        iter.next(),
        Some(Token::new(
            TokenKind::Literal(TokenLiteralKind::Number(TokenNumberLiteral::new(
                TokenNumberLiteralKind::Integer(TokenIntegerLiteralKind::Decimal),
                1
            ))),
            1
        ))
    );
    assert_eq!(iter.next(), Some(Token::new(TokenKind::Whitespace, 1)));
    assert_eq!(
        iter.next(),
        Some(Token::new(
            TokenKind::Literal(TokenLiteralKind::Number(TokenNumberLiteral::new(
                TokenNumberLiteralKind::Integer(TokenIntegerLiteralKind::Decimal),
                1
            ))),
            1
        ))
    );
    assert_eq!(iter.next(), None);
}
