use crate::compiler::{Token, TokenBuilder, TokenError, TokenType};

macro_rules! token {
    ($self:ident, $ty:expr, $content:expr) => {{
        let token = Token::new(
            $self.index,
            $self.line,
            $self.line_offset,
            $ty,
            $content.to_owned(),
        );

        $self.index += $content.len();
        $self.line_offset += $content.len();

        token
    }};
}

macro_rules! token_builder {
    ($self:ident, $builder:ident, $content:ident) => {
        let $builder = TokenBuilder::new($self.index, $self.line, $self.line_offset);
        let mut $content = String::with_capacity(64);
    };
}

macro_rules! try_character {
    ($self:ident, { $($char:pat => $body:block)* }) => {
        {
            let index = $self.index + 1;

            match if index < $self.max_index {
                $self.characters[index]
            } else {
                '\0'
            } {
                $($char => $body)*
            }
        }
    };
}

pub struct Lexer {
    index: usize,
    max_index: usize,
    line: usize,
    line_offset: usize,
    characters: Vec<char>,
}

impl Lexer {
    pub fn new(content: &str) -> Lexer {
        let characters = content.chars().collect::<Vec<_>>();

        Lexer {
            index: 0,
            max_index: characters.len(),
            line: 1,
            line_offset: 1,
            characters,
        }
    }

    pub fn remain(&self) -> bool {
        self.index < self.max_index
    }

    pub unsafe fn current(&self) -> char {
        self.characters[self.index]
    }

    pub fn parse(&mut self) -> Token {
        loop {
            if !self.remain() {
                return Token::new(
                    self.index,
                    self.line,
                    self.line_offset,
                    TokenType::Eof,
                    "".to_owned(),
                );
            }

            let current = unsafe { self.current() };

            if current.is_whitespace() {
                self.index += 1;
                self.line_offset += 1;

                if current == '\n' {
                    self.line += 1;
                    self.line_offset = 1;
                }

                continue;
            }

            match current {
                '#' => {
                    token_builder!(self, builder, content);

                    while self.remain() {
                        let current = unsafe { self.current() };
                        content.push(current);
                        self.index += 1;
                        self.line_offset += 1;

                        if current == '\n' {
                            self.line += 1;
                            self.line_offset = 0;
                            break;
                        }
                    }

                    return builder.build(TokenType::Comment, content);
                }
                '(' => return token!(self, TokenType::PuncParenL, "("),
                ')' => return token!(self, TokenType::PuncParenR, ")"),
                '{' => return token!(self, TokenType::PuncBraceL, "{"),
                '}' => return token!(self, TokenType::PuncBraceR, "}"),
                '[' => return token!(self, TokenType::PuncBracketL, "["),
                ']' => return token!(self, TokenType::PuncBracketR, "]"),
                '.' => return token!(self, TokenType::PuncDot, "."),
                ',' => return token!(self, TokenType::PuncComma, ","),
                ';' => return token!(self, TokenType::PuncSemicolon, ";"),
                '=' => try_character!(self, {
                    '=' => { return token!(self, TokenType::OpCmpEq, "=="); }
                    _ => { return token!(self, TokenType::OpAssign, "="); }
                }),
                '!' => try_character!(self, {
                    '=' => { return token!(self, TokenType::OpCmpNeq, "!="); }
                    _ => { return token!(self, TokenType::Error(TokenError::Unknown), "!"); }
                }),
                '+' => try_character!(self, {
                    '=' => { return token!(self, TokenType::OpAssignAdd, "+="); }
                    _ => { return token!(self, TokenType::OpAdd, "+"); }
                }),
                '-' => try_character!(self, {
                    '=' => { return token!(self, TokenType::OpAssignSub, "-="); }
                    _ => { return token!(self, TokenType::OpSub, "-"); }
                }),
                '*' => try_character!(self, {
                    '=' => { return token!(self, TokenType::OpAssignMul, "*="); }
                    _ => { return token!(self, TokenType::OpMul, "*"); }
                }),
                '/' => try_character!(self, {
                    '=' => { return token!(self, TokenType::OpAssignDiv, "/="); }
                    _ => { return token!(self, TokenType::OpDiv, "/"); }
                }),
                '%' => try_character!(self, {
                    '=' => { return token!(self, TokenType::OpAssignMod, "%="); }
                    _ => { return token!(self, TokenType::OpMod, "%"); }
                }),
                '<' => try_character!(self, {
                    '<' => { try_character!(self, {
                        '=' => { return token!(self, TokenType::OpAssignBitShiftL, "<<="); }
                        _ => { return token!(self, TokenType::OpBitShiftL, "<<"); }
                    }); }
                    '=' => { return token!(self, TokenType::OpCmpLsEq, "<="); }
                    _ => { return token!(self, TokenType::OpCmpLs, "<"); }
                }),
                '>' => try_character!(self, {
                    '>' => { try_character!(self, {
                        '=' => { return token!(self, TokenType::OpAssignBitShiftR, ">="); }
                        _ => { return token!(self, TokenType::OpBitShiftR, ">"); }
                    }); }
                    '=' => { return token!(self, TokenType::OpCmpGtEq, ">="); }
                    _ => { return token!(self, TokenType::OpCmpGt, ">"); }
                }),
                '|' => try_character!(self, {
                    '=' => { return token!(self, TokenType::OpAssignBitOr, "|="); }
                    _ => { return token!(self, TokenType::OpBitOr, "|"); }
                }),
                '&' => try_character!(self, {
                    '=' => { return token!(self, TokenType::OpAssignBitAnd, "&="); }
                    _ => { return token!(self, TokenType::OpBitAnd, "&"); }
                }),
                '^' => try_character!(self, {
                    '=' => { return token!(self, TokenType::OpAssignBitXor, "^="); }
                    _ => { return token!(self, TokenType::OpBitXor, "^"); }
                }),
                '~' => try_character!(self, {
                    '=' => { return token!(self, TokenType::OpAssignBitNot, "~="); }
                    _ => { return token!(self, TokenType::OpBitNot, "~"); }
                }),
                '0' => try_character!(self, {
                    'b' => {

                    }
                    'x' => {

                    }
                    _ => {}
                }),
                '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {}
                '\'' => {
                    token_builder!(self, builder, content);
                    content.push('\'');
                    self.index += 1;
                    self.line_offset += 1;

                    let mut error = None;
                    let mut literal = Vec::with_capacity(2);

                    loop {
                        if !self.remain() {
                            error = Some(TokenError::CharLiteralNotTerminated);
                            break;
                        }

                        let mut current = unsafe { self.current() };

                        match current {
                            '\\' => {
                                content.push(current);
                                self.index += 1;
                                self.line_offset += 1;

                                if !self.remain() {
                                    error = Some(TokenError::CharLiteralNotTerminated);
                                    break;
                                }

                                current = unsafe { self.current() };

                                if current == '\n' {
                                    error = Some(TokenError::CharLiteralNotTerminated);
                                    break;
                                }

                                content.push(current);
                                literal.push(match current {
                                    'n' => '\n',
                                    'r' => '\r',
                                    't' => '\t',
                                    '\\' => '\\',
                                    '0' => '\0',
                                    '\'' => '\'',
                                    '"' => '"',
                                    _ => current,
                                });
                                self.index += 1;
                                self.line_offset += 1;
                            }
                            '\'' => {
                                content.push(current);
                                self.index += 1;
                                self.line_offset += 1;
                                break;
                            }
                            '\n' => {
                                error = Some(TokenError::CharLiteralNotTerminated);
                                break;
                            }
                            _ => {
                                content.push(current);
                                literal.push(current);
                                self.index += 1;
                                self.line_offset += 1;
                            }
                        }
                    }

                    if let Some(error) = error {
                        return builder.build(TokenType::Error(error), content);
                    }

                    match literal.len() {
                        0 => {
                            return builder
                                .build(TokenType::Error(TokenError::CharLiteralEmpty), content)
                        }
                        1 => return builder.build(TokenType::LiteralChar(literal[0]), content),
                        _ => {
                            return builder
                                .build(TokenType::Error(TokenError::CharLiteralTooLong), content)
                        }
                    }
                }
                '"' => {
                    token_builder!(self, builder, content);
                    content.push('"');
                    self.index += 1;
                    self.line_offset += 1;

                    let mut error = None;
                    let mut literal = Vec::with_capacity(2);

                    loop {
                        if !self.remain() {
                            error = Some(TokenError::CharLiteralNotTerminated);
                            break;
                        }

                        let mut current = unsafe { self.current() };

                        match current {
                            '\\' => {
                                content.push(current);
                                self.index += 1;
                                self.line_offset += 1;

                                if !self.remain() {
                                    error = Some(TokenError::StrLiteralNotTerminated);
                                    break;
                                }

                                current = unsafe { self.current() };

                                if current == '\n' {
                                    error = Some(TokenError::StrLiteralNotTerminated);
                                    break;
                                }

                                content.push(current);
                                literal.push(match current {
                                    'n' => '\n',
                                    'r' => '\r',
                                    't' => '\t',
                                    '\\' => '\\',
                                    '0' => '\0',
                                    '\'' => '\'',
                                    '"' => '"',
                                    _ => current,
                                });
                                self.index += 1;
                                self.line_offset += 1;
                            }
                            '"' => {
                                content.push(current);
                                self.index += 1;
                                self.line_offset += 1;
                                break;
                            }
                            '\n' => {
                                error = Some(TokenError::StrLiteralNotTerminated);
                                break;
                            }
                            _ => {
                                content.push(current);
                                literal.push(current);
                                self.index += 1;
                                self.line_offset += 1;
                            }
                        }
                    }

                    if let Some(error) = error {
                        return builder.build(TokenType::Error(error), content);
                    }

                    return builder.build(
                        TokenType::LiteralStr(literal.into_iter().collect::<String>()),
                        content,
                    );
                }
                '@' => try_character!(self, {
                    '"' => {

                    }
                    _ => { return token!(self, TokenType::Error(TokenError::Unknown), "@"); }
                }),
                _ => {}
            }

            token_builder!(self, builder, content);

            while self.remain() {
                let current = unsafe { self.current() };

                if current.is_whitespace() || current.is_ascii_punctuation() && current != '_' {
                    break;
                }

                content.push(current);
                self.index += 1;
                self.line_offset += 1;
            }

            match content.as_str() {
                "" => {
                    return token!(
                        self,
                        TokenType::Error(TokenError::Unknown),
                        current.to_string()
                    )
                }
                "or" => return builder.build(TokenType::OpLogOr, content),
                "and" => return builder.build(TokenType::OpLogAnd, content),
                "not" => return builder.build(TokenType::OpLogNot, content),
                "bool" => return builder.build(TokenType::KeywordBool, content),
                "byte" => return builder.build(TokenType::KeywordByte, content),
                "char" => return builder.build(TokenType::KeywordChar, content),
                "i64" => return builder.build(TokenType::KeywordI64, content),
                "u64" => return builder.build(TokenType::KeywordU64, content),
                "isize" => return builder.build(TokenType::KeywordIsize, content),
                "usize" => return builder.build(TokenType::KeywordUsize, content),
                "f32" => return builder.build(TokenType::KeywordF32, content),
                "f64" => return builder.build(TokenType::KeywordF64, content),
                "cptr" => return builder.build(TokenType::KeywordCptr, content),
                "mptr" => return builder.build(TokenType::KeywordMptr, content),
                "let" => return builder.build(TokenType::KeywordLet, content),
                "fn" => return builder.build(TokenType::KeywordFn, content),
                "if" => return builder.build(TokenType::KeywordIf, content),
                "else" => return builder.build(TokenType::KeywordElse, content),
                "for" => return builder.build(TokenType::KeywordFor, content),
                "in" => return builder.build(TokenType::KeywordIn, content),
                "as" => return builder.build(TokenType::KeywordAs, content),
                "true" => return builder.build(TokenType::LiteralBool(true), content),
                "false" => return builder.build(TokenType::LiteralBool(false), content),
                _ => return builder.build(TokenType::Id, content),
            }
        }
    }
}
