use crate::Cursor;
use high_lexer::{Symbol, Token, TokenKind, TokenLiteral};
use span::{Pos, Span};

// TODO: Make the expects a bit more readable and informative.

pub struct Parser<T>
where
    T: Iterator<Item = Token>,
{
    cursor: Cursor<T>,
    expects: Vec<String>,
    span: Span,
}

impl<T> Parser<T>
where
    T: Iterator<Item = Token>,
{
    pub fn new(cursor: Cursor<T>) -> Self {
        Self {
            cursor,
            expects: vec![],
            span: Span::new(Pos::new(0), Pos::new(0)),
        }
    }

    pub fn cursor(&self) -> &Cursor<T> {
        &self.cursor
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn exists(&self) -> bool {
        self.cursor.first().is_some()
    }

    pub fn expect_begin(&mut self) {
        self.expects.clear();
    }

    pub fn expect_kind(&mut self, kind: TokenKind) -> bool {
        self.expects.push(kind.to_str().to_owned());

        if self.cursor.is_kind(kind) {
            self.span = self.cursor.first().as_ref().unwrap().span();
            self.cursor.consume();
            return true;
        }

        false
    }

    pub fn expect_id(&mut self) -> Option<Symbol> {
        self.expects.push("identifier".to_owned());

        if let Some(id) = self.cursor.id() {
            self.span = self.cursor.first().as_ref().unwrap().span();
            self.cursor.consume();
            return Some(id);
        }

        None
    }

    pub fn expect_keyword(&mut self, keyword: Symbol) -> bool {
        self.expects.push(format!("keyword '{}'", keyword));

        if let Some(id) = self.cursor.id() {
            if id == keyword {
                self.span = self.cursor.first().as_ref().unwrap().span();
                self.cursor.consume();
                return true;
            }
        }

        false
    }

    pub fn expect_literal(&mut self) -> Option<TokenLiteral> {
        self.expects.push("literal".to_owned());

        if let Some(literal) = self.cursor.literal().cloned() {
            self.span = self.cursor.first().as_ref().unwrap().span();
            self.cursor.consume();
            return Some(literal);
        }

        None
    }

    pub fn expect_else(&mut self) -> (String, Span) {
        let err = if let Some(token) = self.cursor.first() {
            if self.expects.len() == 1 {
                format!(
                    "unexpected token {}; {} expected",
                    token.kind().to_str(),
                    self.expects.join(", "),
                )
            } else {
                format!(
                    "unexpected token {}; one of {} expected",
                    token.kind().to_str(),
                    self.expects.join(", "),
                )
            }
        } else {
            if self.expects.len() == 1 {
                format!("unexpected end of file; {} expected", self.expects[0])
            } else {
                format!(
                    "unexpected end of file; one of {} expected",
                    self.expects.join(", "),
                )
            }
        };
        self.expects.clear();
        (err, self.span)
    }

    pub fn consume(&mut self) {
        self.cursor.consume();
    }
}
