use high_lexer::{Symbol, Token, TokenKind, TokenLiteral};

pub struct Cursor<T>
where
    T: Iterator<Item = Token>,
{
    tok: T,
    first: Option<Token>,
    second: Option<Token>,
}

impl<T> Cursor<T>
where
    T: Iterator<Item = Token>,
{
    pub fn new(mut tok: T) -> Self {
        Self {
            first: tok.next(),
            second: tok.next(),
            tok,
        }
    }

    pub fn first(&self) -> &Option<Token> {
        &self.first
    }

    pub fn second(&self) -> &Option<Token> {
        &self.second
    }

    pub fn consume(&mut self) {
        self.first = self.second.take();
        self.second = self.tok.next();
    }

    pub fn id(&self) -> Option<Symbol> {
        self.first.as_ref().and_then(|token| {
            if let TokenKind::Id(id) = token.kind() {
                Some(*id)
            } else {
                None
            }
        })
    }

    pub fn literal(&self) -> Option<&TokenLiteral> {
        self.first.as_ref().and_then(|token| {
            if let TokenKind::Literal(literal) = token.kind() {
                Some(literal)
            } else {
                None
            }
        })
    }

    pub fn is_kind(&self, kind: TokenKind) -> bool {
        self.first
            .as_ref()
            .map_or(false, |token| token.kind() == &kind)
    }

    // pub fn is_id(&self) -> bool {
    //     self.first.as_ref().map_or(false, |token| {
    //         if let TokenKind::Id(..) = token.kind() {
    //             true
    //         } else {
    //             false
    //         }
    //     })
    // }

    // pub fn is_keyword(&self, keyword: Symbol) -> bool {
    //     self.id().map_or(false, |id| id == keyword)
    // }

    // pub fn is_literal(&self) -> bool {
    //     self.first.as_ref().map_or(false, |token| {
    //         if let TokenKind::Literal(..) = token.kind() {
    //             true
    //         } else {
    //             false
    //         }
    //     })
    // }
}
