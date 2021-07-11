use crate::compiler::*;

macro_rules! lit {
    ($self:ident, $ty:expr, $content:literal) => {{
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

macro_rules! dy {
    ($self:ident, $ty:expr, $content:expr) => {{
        let len = $content.len();
        let token = Token::new($self.index, $self.line, $self.line_offset, $ty, $content);

        $self.index += len;
        $self.line_offset += len;

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
    ($self:ident, $index:ident, $($char:pat => $body:block)+) => {
        {
            let $index = $index + 1;

            match if $index < $self.max_index {
                $self.characters[$index]
            } else {
                '\0'
            } {
                $($char => $body)+
            }
        }
    };
    ($self:ident, $index:ident, $($char:pat => $body:expr),+) => {
        {
            let $index = $index + 1;

            match if $index < $self.max_index {
                $self.characters[$index]
            } else {
                '\0'
            } {
                $($char => $body),+
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

    pub fn char(&self) -> char {
        self.characters[self.index]
    }

    fn advance(&mut self) {
        self.index += 1;
        self.line_offset += 1;
    }

    fn advance_by(&mut self, len: usize) {
        self.index += len;
        self.line_offset += len;
    }

    fn advance_line(&mut self) {
        self.index += 1;
        self.line += 1;
        self.line_offset = 1;
    }

    fn advance_possible_newline(&mut self) {
        if self.char() == '\n' {
            self.index += 1;
            self.line += 1;
            self.line_offset = 1;
            return;
        }

        self.index += 1;
        self.line_offset += 1;
    }

    pub fn next(&mut self) -> Token {
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

            let char = self.char();

            if char.is_whitespace() {
                self.advance_possible_newline();
                continue;
            }

            let index = self.index;

            match char {
                '#' => {
                    token_builder!(self, builder, content);

                    while self.remain() {
                        let char = self.char();
                        self.advance_possible_newline();

                        if char == '\n' {
                            break;
                        }

                        content.push(char);
                    }

                    return builder.build(TokenType::Comment, content);
                }
                '(' => return lit! { self, TokenType::PuncParenL, "(" },
                ')' => return lit! { self, TokenType::PuncParenR, ")" },
                '{' => return lit! { self, TokenType::PuncBraceL, "{" },
                '}' => return lit! { self, TokenType::PuncBraceR, "}" },
                '[' => return lit! { self, TokenType::PuncBracketL, "[" },
                ']' => return lit! { self, TokenType::PuncBracketR, "]" },
                '.' => return lit! { self, TokenType::PuncDot, "." },
                ',' => return lit! { self, TokenType::PuncComma, "," },
                ';' => return lit! { self, TokenType::PuncSemicolon, ";" },
                '=' => try_character! {
                    self,
                    index,
                    '=' => return lit! { self, TokenType::OpCmpEq, "==" },
                    _ => return lit! { self, TokenType::OpAssign, "=" }
                },
                '!' => try_character! {
                    self,
                    index,
                    '=' => return lit! { self, TokenType::OpCmpNeq, "!=" },
                    _ => return lit! { self, TokenType::Error(TokenError::Unknown), "!" }
                },
                '+' => try_character! {
                    self,
                    index,
                    '=' => return lit! { self, TokenType::OpAssignAdd, "+=" },
                    '0' => return self.next_number(),
                    '1' => return self.next_number(),
                    '2' => return self.next_number(),
                    '3' => return self.next_number(),
                    '4' => return self.next_number(),
                    '5' => return self.next_number(),
                    '6' => return self.next_number(),
                    '7' => return self.next_number(),
                    '8' => return self.next_number(),
                    '9' => return self.next_number(),
                    _ => return lit! { self, TokenType::OpAdd, "+" }
                },
                '-' => try_character! {
                    self,
                    index,
                    '=' => return lit! { self, TokenType::OpAssignSub, "-=" },
                    '0' => return self.next_number(),
                    '1' => return self.next_number(),
                    '2' => return self.next_number(),
                    '3' => return self.next_number(),
                    '4' => return self.next_number(),
                    '5' => return self.next_number(),
                    '6' => return self.next_number(),
                    '7' => return self.next_number(),
                    '8' => return self.next_number(),
                    '9' => return self.next_number(),
                    _ => return lit! { self, TokenType::OpSub, "-" }
                },
                '*' => try_character! {
                    self,
                    index,
                    '=' => return lit! { self, TokenType::OpAssignMul, "*=" },
                    _ => return lit! { self, TokenType::OpMul, "*" }
                },
                '/' => try_character! {
                    self,
                    index,
                    '=' => return lit! { self, TokenType::OpAssignDiv, "/=" },
                    _ => return lit! { self, TokenType::OpDiv, "/" }
                },
                '%' => try_character! {
                    self,
                    index,
                    '=' => return lit! { self, TokenType::OpAssignMod, "%=" },
                    _ => return lit! { self, TokenType::OpMod, "%" }
                },
                '<' => try_character! {
                    self,
                    index,
                    '<' => try_character! {
                        self,
                        index,
                        '=' => return lit! { self, TokenType::OpAssignBitShiftL, "<<=" },
                        _ => return lit! { self, TokenType::OpBitShiftL, "<<" }
                    },
                    '=' => return lit! { self, TokenType::OpCmpLsEq, "<=" },
                    _ => return lit! { self, TokenType::OpCmpLs, "<" }
                },
                '>' => try_character! {
                    self,
                    index,
                    '>' => try_character! {
                        self,
                        index,
                        '=' => return lit! { self, TokenType::OpAssignBitShiftR, ">>=" },
                        _ => return lit! { self, TokenType::OpBitShiftR, ">>" }
                    },
                    '=' => return lit! { self, TokenType::OpCmpGtEq, ">=" },
                    _ => return lit! { self, TokenType::OpCmpGt, ">" }
                },
                '|' => try_character! {
                    self,
                    index,
                    '=' => return lit! { self, TokenType::OpAssignBitOr, "|=" },
                    _ => return lit! { self, TokenType::OpBitOr, "|" }
                },
                '&' => try_character! {
                    self,
                    index,
                    '=' => return lit! { self, TokenType::OpAssignBitAnd, "&=" },
                    _ => return lit! { self, TokenType::OpBitAnd, "&" }
                },
                '^' => try_character! {
                    self,
                    index,
                    '=' => return lit! { self, TokenType::OpAssignBitXor, "^=" },
                    _ => return lit! { self, TokenType::OpBitXor, "^" }
                },
                '~' => try_character! {
                    self,
                    index,
                    '=' => return lit! { self, TokenType::OpAssignBitNot, "~=" },
                    _ => return lit! { self, TokenType::OpBitNot, "~" }
                },
                '0' => try_character! {
                    self,
                    index,
                    'b' => {
                        unimplemented!()
                    }
                    'B' => {
                        unimplemented!()
                    }
                    'x' => {
                        unimplemented!()
                    }
                    'X' => {
                        unimplemented!()
                    }
                    _ => {
                        return self.next_number();
                    }
                },
                '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                    return self.next_number();
                }
                '\'' => {
                    token_builder!(self, builder, content);
                    content.push('\'');
                    self.advance();

                    let mut error = None;
                    let mut literal = Vec::with_capacity(2);

                    loop {
                        if !self.remain() {
                            error = Some(TokenError::CharLiteralNotTerminated);
                            break;
                        }

                        let mut char = self.char();

                        match char {
                            '\\' => {
                                content.push(char);
                                self.advance();

                                if !self.remain() {
                                    error = Some(TokenError::CharLiteralNotTerminated);
                                    break;
                                }

                                char = self.char();

                                if char == '\n' {
                                    error = Some(TokenError::CharLiteralNotTerminated);
                                    break;
                                }

                                content.push(char);
                                literal.push(match char {
                                    'n' => '\n',
                                    'r' => '\r',
                                    't' => '\t',
                                    '\\' => '\\',
                                    '0' => '\0',
                                    '\'' => '\'',
                                    '"' => '"',
                                    _ => char,
                                });
                                self.advance();
                            }
                            '\'' => {
                                content.push(char);
                                self.advance();
                                break;
                            }
                            '\n' => {
                                error = Some(TokenError::CharLiteralNotTerminated);
                                break;
                            }
                            _ => {
                                content.push(char);
                                literal.push(char);
                                self.advance();
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
                    self.advance();

                    let mut error = None;
                    let mut literal = Vec::with_capacity(2);

                    loop {
                        if !self.remain() {
                            error = Some(TokenError::StrLiteralNotTerminated);
                            break;
                        }

                        let mut char = self.char();

                        match char {
                            '\\' => {
                                content.push(char);
                                self.advance();

                                if !self.remain() {
                                    error = Some(TokenError::StrLiteralNotTerminated);
                                    break;
                                }

                                char = self.char();

                                if char == '\n' {
                                    error = Some(TokenError::StrLiteralNotTerminated);
                                    break;
                                }

                                content.push(char);
                                literal.push(match char {
                                    'n' => '\n',
                                    'r' => '\r',
                                    't' => '\t',
                                    '\\' => '\\',
                                    '0' => '\0',
                                    '\'' => '\'',
                                    '"' => '"',
                                    _ => char,
                                });
                                self.advance();
                            }
                            '"' => {
                                content.push(char);
                                self.advance();
                                break;
                            }
                            '\n' => {
                                error = Some(TokenError::StrLiteralNotTerminated);
                                break;
                            }
                            _ => {
                                content.push(char);
                                literal.push(char);
                                self.advance();
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
                '@' => try_character! {
                    self,
                    index,
                    '"' => {
                        token_builder!(self, builder, content);
                        content.push('@');
                        content.push('"');
                        self.advance_by(2);

                        let mut error = None;
                        let mut literal = Vec::with_capacity(2);

                        loop {
                            if !self.remain() {
                                error = Some(TokenError::StrLiteralNotTerminated);
                                break;
                            }

                            let mut char = self.char();

                            match char {
                                '\\' => {
                                    content.push(char);
                                    self.advance();

                                    if !self.remain() {
                                        error = Some(TokenError::StrLiteralNotTerminated);
                                        break;
                                    }

                                    char = self.char();
                                    content.push(char);
                                    literal.push(match char {
                                        'n' => '\n',
                                        'r' => '\r',
                                        't' => '\t',
                                        '\\' => '\\',
                                        '0' => '\0',
                                        '\'' => '\'',
                                        '"' => '"',
                                        _ => char,
                                    });

                                    if char == '\n' {
                                        self.advance_line();
                                    } else {
                                        self.advance();
                                    }
                                }
                                '"' => {
                                    content.push(char);
                                    self.advance();

                                    if !self.remain() {
                                        error = Some(TokenError::StrLiteralNotTerminated);
                                        break;
                                    }

                                    char = self.char();
                                    content.push(char);

                                    if char == '@' {
                                        self.advance();
                                        break;
                                    }

                                    literal.push('"');
                                }
                                '\n' => {
                                    content.push(char);
                                    literal.push(char);
                                    self.advance_line();
                                }
                                _ => {
                                    content.push(char);
                                    literal.push(char);
                                    self.advance();
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
                    _ => { return lit! { self, TokenType::Error(TokenError::Unknown), "@" }; }
                },
                _ => {}
            }

            token_builder!(self, builder, content);

            while self.remain() {
                let char = self.char();

                if char.is_whitespace() || char.is_ascii_punctuation() && char != '_' {
                    break;
                }

                content.push(char);
                self.advance();
            }

            match content.as_str() {
                "" => {
                    return dy! {
                        self,
                        TokenType::Error(TokenError::Unknown),
                        char.to_string()
                    }
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
                "use" => return builder.build(TokenType::KeywordUse, content),
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

    fn next_number(&mut self) -> Token {
        token_builder!(self, builder, content);

        let char = self.char();

        if char == '+' || char == '-' {
            content.push(char);
            self.advance();
        }

        let decimal = self.next_integer(self.index, 10);
        content.push_str(&decimal);
        self.advance_by(decimal.len());

        if !self.remain() {
            return builder.build(TokenType::LiteralInteger(content.clone()), content);
        }

        match self.char() {
            '.' => {
                let frac = self.next_frac(self.index);

                if frac.is_empty() {
                    return builder.build(TokenType::LiteralInteger(content.clone()), content);
                }

                content.push_str(&frac);
                self.advance_by(frac.len());

                let exp = self.next_exp(self.index);
                content.push_str(&exp);
                self.advance_by(exp.len());

                let suffix = self.next_literal_suffix(self.index);

                if suffix.is_empty() {
                    return builder.build(TokenType::LiteralF64(content.clone()), content);
                }

                content.push_str(&suffix);
                self.advance_by(suffix.len());

                return builder.build(
                    match suffix.as_str() {
                        "byte" => TokenType::Error(TokenError::F64LiteralFollowedBySuffixByte),
                        "char" => TokenType::Error(TokenError::F64LiteralFollowedBySuffixChar),
                        "i64" => TokenType::Error(TokenError::F64LiteralFollowedBySuffixI64),
                        "u64" => TokenType::Error(TokenError::F64LiteralFollowedBySuffixU64),
                        "isize" => TokenType::Error(TokenError::F64LiteralFollowedBySuffixISize),
                        "usize" => TokenType::Error(TokenError::F64LiteralFollowedBySuffixUSize),
                        "f64" => TokenType::LiteralF64(content.clone()),
                        _ => unreachable!(),
                    },
                    content,
                );
            }
            'e' | 'E' => {
                let exp = self.next_exp(self.index);

                if exp.is_empty() {
                    // We can skip the suffix parsing, because there's no suffix that starts with a 'e'.
                    return builder.build(TokenType::LiteralInteger(content.clone()), content);
                }

                content.push_str(&exp);
                self.advance_by(exp.len());

                let suffix = self.next_literal_suffix(self.index);

                if suffix.is_empty() {
                    return builder.build(TokenType::LiteralF64(content.clone()), content);
                }

                content.push_str(&suffix);
                self.advance_by(suffix.len());

                return builder.build(
                    match suffix.as_str() {
                        "byte" => TokenType::Error(TokenError::F64LiteralFollowedBySuffixByte),
                        "char" => TokenType::Error(TokenError::F64LiteralFollowedBySuffixChar),
                        "i64" => TokenType::Error(TokenError::F64LiteralFollowedBySuffixI64),
                        "u64" => TokenType::Error(TokenError::F64LiteralFollowedBySuffixU64),
                        "isize" => TokenType::Error(TokenError::F64LiteralFollowedBySuffixISize),
                        "usize" => TokenType::Error(TokenError::F64LiteralFollowedBySuffixUSize),
                        "f64" => TokenType::LiteralF64(content.clone()),
                        _ => unreachable!(),
                    },
                    content,
                );
            }
            _ => {
                let suffix = self.next_literal_suffix(self.index);

                if suffix.is_empty() {
                    return builder.build(TokenType::LiteralInteger(content.clone()), content);
                }

                content.push_str(&suffix);
                self.advance_by(suffix.len());

                return builder.build(
                    match suffix.as_str() {
                        "byte" => TokenType::LiteralByte(content.clone()),
                        "char" => TokenType::LiteralCharInteger(content.clone()),
                        "i64" => TokenType::LiteralI64(content.clone()),
                        "u64" => TokenType::LiteralU64(content.clone()),
                        "isize" => TokenType::LiteralIsize(content.clone()),
                        "usize" => TokenType::LiteralUsize(content.clone()),
                        "f64" => TokenType::LiteralF64(content.clone()),
                        _ => unreachable!(),
                    },
                    content,
                );
            }
        }
    }

    fn next_integer(&mut self, mut index: usize, radix: u32) -> String {
        let mut integer = String::with_capacity(8);

        while index < self.max_index {
            let char = self.characters[index];

            if !char.is_digit(radix) {
                break;
            }

            integer.push(char);
            index += 1;
        }

        integer
    }

    fn next_frac(&mut self, mut index: usize) -> String {
        if !self.remain() {
            return "".to_owned();
        }

        let mut char = self.characters[index];

        if char != '.' {
            return "".to_owned();
        }

        let mut frac = String::with_capacity(8);
        frac.push(char);
        index += 1;

        if self.max_index <= index {
            return "".to_owned();
        }

        char = self.characters[index];

        if !char.is_digit(10) {
            return "".to_owned();
        }

        frac.push(char);
        index += 1;

        frac.push_str(&self.next_integer(index, 10));
        frac
    }

    fn next_exp(&mut self, mut index: usize) -> String {
        if !self.remain() {
            return "".to_owned();
        }

        let mut char = self.characters[index];

        if char != 'e' && char != 'E' {
            return "".to_owned();
        }

        let mut exp = String::with_capacity(8);
        exp.push(char);
        index += 1;

        if self.max_index <= index {
            return "".to_owned();
        }

        char = self.characters[index];

        if char == '+' || char == '-' {
            exp.push(char);
            index += 1;

            if self.max_index <= index {
                return "".to_owned();
            }

            char = self.characters[index];
        }

        if !char.is_digit(10) {
            return "".to_owned();
        }

        exp.push(char);
        index += 1;

        exp.push_str(&self.next_integer(index, 10));
        exp
    }

    fn next_literal_suffix(&self, index: usize) -> String {
        if !self.remain() {
            return "".to_owned();
        }

        match self.char() {
            'b' => try_character! {
                self,
                index,
                'y' => try_character! {
                    self,
                    index,
                    't' => try_character! {
                        self,
                        index,
                        'e' => return "byte".to_owned(),
                        _ => return "".to_owned()
                    },
                    _ => return "".to_owned()
                },
                _ => return "".to_owned()
            },
            'c' => try_character! {
                self,
                index,
                'h' => try_character! {
                    self,
                    index,
                    'a' => try_character! {
                        self,
                        index,
                        'r' => return "char".to_owned(),
                        _ => return "".to_owned()
                    },
                    _ => return "".to_owned()
                },
                _ => return "".to_owned()
            },
            'f' => try_character! {
                self,
                index,
                '6' => try_character! {
                    self,
                    index,
                    '4' => return "f64".to_owned(),
                    _ => return "".to_owned()
                },
                _ => return "".to_owned()
            },
            'i' => try_character! {
                self,
                index,
                '6' => try_character! {
                    self,
                    index,
                    '4' => return "i64".to_owned(),
                    _ => return "".to_owned()
                },
                's' => try_character! {
                    self,
                    index,
                    'i' => try_character! {
                        self,
                        index,
                        'z' => try_character! {
                            self,
                            index,
                            'e' => return "isize".to_owned(),
                            _ => return "".to_owned()
                        },
                        _ => return "".to_owned()
                    },
                    _ => return "".to_owned()
                },
                _ => return "".to_owned()
            },
            'u' => try_character! {
                self,
                index,
                '6' => try_character! {
                    self,
                    index,
                    '4' => return "u64".to_owned(),
                    _ => return "".to_owned()
                },
                's' => try_character! {
                    self,
                    index,
                    'i' => try_character! {
                        self,
                        index,
                        'z' => try_character! {
                            self,
                            index,
                            'e' => return "usize".to_owned(),
                            _ => return "".to_owned()
                        },
                        _ => return "".to_owned()
                    },
                    _ => return "".to_owned()
                },
                _ => return "".to_owned()
            },
            _ => return "".to_owned(),
        }
    }
}

#[cfg(test)]
mod utils {
    use super::*;

    pub fn collect_all_tokens(input: &str) -> Vec<Token> {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();

        loop {
            let token = lexer.next();

            match token.ty() {
                TokenType::Eof => {
                    break;
                }
                _ => tokens.push(token),
            }
        }

        tokens
    }

    pub fn extract_token_ty(tokens: &Vec<Token>) -> Vec<TokenType> {
        let mut token_ty = Vec::new();

        for token in tokens {
            token_ty.push(token.ty().clone());
        }

        token_ty
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_comments() {
        let input = "# comment";
        let tokens = utils::collect_all_tokens(input);
        let token_ty = utils::extract_token_ty(&tokens);
        assert_eq!(token_ty, [TokenType::Comment,]);

        let input = "#\n##\n###\n####\n#####\n######\n#######\n########\n#########\n##########";
        let tokens = utils::collect_all_tokens(input);
        let token_ty = utils::extract_token_ty(&tokens);
        assert_eq!(
            token_ty,
            [
                TokenType::Comment,
                TokenType::Comment,
                TokenType::Comment,
                TokenType::Comment,
                TokenType::Comment,
                TokenType::Comment,
                TokenType::Comment,
                TokenType::Comment,
                TokenType::Comment,
                TokenType::Comment,
            ]
        );
    }

    #[test]
    fn test_punctuations() {
        let input = "(){}[].,;";
        let tokens = utils::collect_all_tokens(input);
        let token_ty = utils::extract_token_ty(&tokens);
        assert_eq!(
            token_ty,
            [
                TokenType::PuncParenL,
                TokenType::PuncParenR,
                TokenType::PuncBraceL,
                TokenType::PuncBraceR,
                TokenType::PuncBracketL,
                TokenType::PuncBracketR,
                TokenType::PuncDot,
                TokenType::PuncComma,
                TokenType::PuncSemicolon,
            ]
        );

        let input = input.chars().rev().collect::<String>();
        let tokens = utils::collect_all_tokens(&input);
        let token_ty = utils::extract_token_ty(&tokens);
        assert_eq!(
            token_ty,
            [
                TokenType::PuncParenL,
                TokenType::PuncParenR,
                TokenType::PuncBraceL,
                TokenType::PuncBraceR,
                TokenType::PuncBracketL,
                TokenType::PuncBracketR,
                TokenType::PuncDot,
                TokenType::PuncComma,
                TokenType::PuncSemicolon,
            ]
            .iter()
            .rev()
            .cloned()
            .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_ops() {
        let input = "=+=-=*=/=%=<<=>>=|=&=^=~=or and not==!=< <=> >=+-*/%<<>>|&^~";
        let tokens = utils::collect_all_tokens(input);
        let token_ty = utils::extract_token_ty(&tokens);
        assert_eq!(
            token_ty,
            [
                TokenType::OpAssign,
                TokenType::OpAssignAdd,
                TokenType::OpAssignSub,
                TokenType::OpAssignMul,
                TokenType::OpAssignDiv,
                TokenType::OpAssignMod,
                TokenType::OpAssignBitShiftL,
                TokenType::OpAssignBitShiftR,
                TokenType::OpAssignBitOr,
                TokenType::OpAssignBitAnd,
                TokenType::OpAssignBitXor,
                TokenType::OpAssignBitNot,
                TokenType::OpLogOr,
                TokenType::OpLogAnd,
                TokenType::OpLogNot,
                TokenType::OpCmpEq,
                TokenType::OpCmpNeq,
                TokenType::OpCmpLs,
                TokenType::OpCmpLsEq,
                TokenType::OpCmpGt,
                TokenType::OpCmpGtEq,
                TokenType::OpAdd,
                TokenType::OpSub,
                TokenType::OpMul,
                TokenType::OpDiv,
                TokenType::OpMod,
                TokenType::OpBitShiftL,
                TokenType::OpBitShiftR,
                TokenType::OpBitOr,
                TokenType::OpBitAnd,
                TokenType::OpBitXor,
                TokenType::OpBitNot,
            ]
        );

        let input = "~^&|>><<%/*-+>=><=<!===not and or~=^=&=|=>>=<<=%=/=*=-=+==";
        let tokens = utils::collect_all_tokens(input);
        let token_ty = utils::extract_token_ty(&tokens);
        assert_eq!(
            token_ty,
            [
                TokenType::OpAssign,
                TokenType::OpAssignAdd,
                TokenType::OpAssignSub,
                TokenType::OpAssignMul,
                TokenType::OpAssignDiv,
                TokenType::OpAssignMod,
                TokenType::OpAssignBitShiftL,
                TokenType::OpAssignBitShiftR,
                TokenType::OpAssignBitOr,
                TokenType::OpAssignBitAnd,
                TokenType::OpAssignBitXor,
                TokenType::OpAssignBitNot,
                TokenType::OpLogOr,
                TokenType::OpLogAnd,
                TokenType::OpLogNot,
                TokenType::OpCmpEq,
                TokenType::OpCmpNeq,
                TokenType::OpCmpLs,
                TokenType::OpCmpLsEq,
                TokenType::OpCmpGt,
                TokenType::OpCmpGtEq,
                TokenType::OpAdd,
                TokenType::OpSub,
                TokenType::OpMul,
                TokenType::OpDiv,
                TokenType::OpMod,
                TokenType::OpBitShiftL,
                TokenType::OpBitShiftR,
                TokenType::OpBitOr,
                TokenType::OpBitAnd,
                TokenType::OpBitXor,
                TokenType::OpBitNot,
            ]
            .iter()
            .rev()
            .cloned()
            .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_keywords() {
        let input =
            "bool byte char i64 u64 isize usize f32 f64 cptr mptr use let fn if else for in as";
        let tokens = utils::collect_all_tokens(input);
        let token_ty = utils::extract_token_ty(&tokens);
        assert_eq!(
            token_ty,
            [
                TokenType::KeywordBool,
                TokenType::KeywordByte,
                TokenType::KeywordChar,
                TokenType::KeywordI64,
                TokenType::KeywordU64,
                TokenType::KeywordIsize,
                TokenType::KeywordUsize,
                TokenType::KeywordF32,
                TokenType::KeywordF64,
                TokenType::KeywordCptr,
                TokenType::KeywordMptr,
                TokenType::KeywordUse,
                TokenType::KeywordLet,
                TokenType::KeywordFn,
                TokenType::KeywordIf,
                TokenType::KeywordElse,
                TokenType::KeywordFor,
                TokenType::KeywordIn,
                TokenType::KeywordAs,
            ]
        );
    }

    #[test]
    fn test_literals_bool() {
        let input = "true false";
        let tokens = utils::collect_all_tokens(input);
        let token_ty = utils::extract_token_ty(&tokens);
        assert_eq!(
            token_ty,
            [TokenType::LiteralBool(true), TokenType::LiteralBool(false)]
        );
    }

    #[test]
    fn test_literals_integer() {
        let input = "0 1 2 3 4 5 00 11 22 33 44 55 000 111 222 333 444 555";
        let tokens = utils::collect_all_tokens(input);
        let token_ty = utils::extract_token_ty(&tokens);
        assert_eq!(
            token_ty,
            [
                TokenType::LiteralInteger("0".to_owned()),
                TokenType::LiteralInteger("1".to_owned()),
                TokenType::LiteralInteger("2".to_owned()),
                TokenType::LiteralInteger("3".to_owned()),
                TokenType::LiteralInteger("4".to_owned()),
                TokenType::LiteralInteger("5".to_owned()),
                TokenType::LiteralInteger("00".to_owned()),
                TokenType::LiteralInteger("11".to_owned()),
                TokenType::LiteralInteger("22".to_owned()),
                TokenType::LiteralInteger("33".to_owned()),
                TokenType::LiteralInteger("44".to_owned()),
                TokenType::LiteralInteger("55".to_owned()),
                TokenType::LiteralInteger("000".to_owned()),
                TokenType::LiteralInteger("111".to_owned()),
                TokenType::LiteralInteger("222".to_owned()),
                TokenType::LiteralInteger("333".to_owned()),
                TokenType::LiteralInteger("444".to_owned()),
                TokenType::LiteralInteger("555".to_owned()),
            ]
        );
    }

    #[test]
    fn test_literals_signed_integer() {
        let input = "+0 -1 +2 -3 +4 -5 +00 -11 +22 -33 +44 -55 +000 -111 +222 -333 +444 -555";
        let tokens = utils::collect_all_tokens(input);
        let token_ty = utils::extract_token_ty(&tokens);
        assert_eq!(
            token_ty,
            [
                TokenType::LiteralInteger("+0".to_owned()),
                TokenType::LiteralInteger("-1".to_owned()),
                TokenType::LiteralInteger("+2".to_owned()),
                TokenType::LiteralInteger("-3".to_owned()),
                TokenType::LiteralInteger("+4".to_owned()),
                TokenType::LiteralInteger("-5".to_owned()),
                TokenType::LiteralInteger("+00".to_owned()),
                TokenType::LiteralInteger("-11".to_owned()),
                TokenType::LiteralInteger("+22".to_owned()),
                TokenType::LiteralInteger("-33".to_owned()),
                TokenType::LiteralInteger("+44".to_owned()),
                TokenType::LiteralInteger("-55".to_owned()),
                TokenType::LiteralInteger("+000".to_owned()),
                TokenType::LiteralInteger("-111".to_owned()),
                TokenType::LiteralInteger("+222".to_owned()),
                TokenType::LiteralInteger("-333".to_owned()),
                TokenType::LiteralInteger("+444".to_owned()),
                TokenType::LiteralInteger("-555".to_owned()),
            ]
        );
    }

    #[test]
    fn test_literals_f64_frac() {
        let input = "0.0 1.1 2.2 3.3 4.4 5.5 00.00 11.11 22.22 33.33 44.44 55.55 000.000 111.111 222.222 333.333 444.444 555.555";
        let tokens = utils::collect_all_tokens(input);
        let token_ty = utils::extract_token_ty(&tokens);
        assert_eq!(
            token_ty,
            [
                TokenType::LiteralF64("0.0".to_owned()),
                TokenType::LiteralF64("1.1".to_owned()),
                TokenType::LiteralF64("2.2".to_owned()),
                TokenType::LiteralF64("3.3".to_owned()),
                TokenType::LiteralF64("4.4".to_owned()),
                TokenType::LiteralF64("5.5".to_owned()),
                TokenType::LiteralF64("00.00".to_owned()),
                TokenType::LiteralF64("11.11".to_owned()),
                TokenType::LiteralF64("22.22".to_owned()),
                TokenType::LiteralF64("33.33".to_owned()),
                TokenType::LiteralF64("44.44".to_owned()),
                TokenType::LiteralF64("55.55".to_owned()),
                TokenType::LiteralF64("000.000".to_owned()),
                TokenType::LiteralF64("111.111".to_owned()),
                TokenType::LiteralF64("222.222".to_owned()),
                TokenType::LiteralF64("333.333".to_owned()),
                TokenType::LiteralF64("444.444".to_owned()),
                TokenType::LiteralF64("555.555".to_owned()),
            ]
        );
    }

    #[test]
    fn test_literals_signed_f64_frac() {
        let input = "+0.0 -1.1 +2.2 -3.3 +4.4 -5.5 +00.00 -11.11 +22.22 -33.33 +44.44 -55.55 +000.000 -111.111 +222.222 -333.333 +444.444 -555.555";
        let tokens = utils::collect_all_tokens(input);
        let token_ty = utils::extract_token_ty(&tokens);
        assert_eq!(
            token_ty,
            [
                TokenType::LiteralF64("+0.0".to_owned()),
                TokenType::LiteralF64("-1.1".to_owned()),
                TokenType::LiteralF64("+2.2".to_owned()),
                TokenType::LiteralF64("-3.3".to_owned()),
                TokenType::LiteralF64("+4.4".to_owned()),
                TokenType::LiteralF64("-5.5".to_owned()),
                TokenType::LiteralF64("+00.00".to_owned()),
                TokenType::LiteralF64("-11.11".to_owned()),
                TokenType::LiteralF64("+22.22".to_owned()),
                TokenType::LiteralF64("-33.33".to_owned()),
                TokenType::LiteralF64("+44.44".to_owned()),
                TokenType::LiteralF64("-55.55".to_owned()),
                TokenType::LiteralF64("+000.000".to_owned()),
                TokenType::LiteralF64("-111.111".to_owned()),
                TokenType::LiteralF64("+222.222".to_owned()),
                TokenType::LiteralF64("-333.333".to_owned()),
                TokenType::LiteralF64("+444.444".to_owned()),
                TokenType::LiteralF64("-555.555".to_owned()),
            ]
        );
    }

    #[test]
    fn test_literals_f64_exp() {
        let input = "0e0 1e1 2e2 3e3 4e4 5e5 00E00 11E11 22E22 33E33 44E44 55E55 000e000 111e111 222e222 333e333 444e444 555e555";
        let tokens = utils::collect_all_tokens(input);
        let token_ty = utils::extract_token_ty(&tokens);
        assert_eq!(
            token_ty,
            [
                TokenType::LiteralF64("0e0".to_owned()),
                TokenType::LiteralF64("1e1".to_owned()),
                TokenType::LiteralF64("2e2".to_owned()),
                TokenType::LiteralF64("3e3".to_owned()),
                TokenType::LiteralF64("4e4".to_owned()),
                TokenType::LiteralF64("5e5".to_owned()),
                TokenType::LiteralF64("00E00".to_owned()),
                TokenType::LiteralF64("11E11".to_owned()),
                TokenType::LiteralF64("22E22".to_owned()),
                TokenType::LiteralF64("33E33".to_owned()),
                TokenType::LiteralF64("44E44".to_owned()),
                TokenType::LiteralF64("55E55".to_owned()),
                TokenType::LiteralF64("000e000".to_owned()),
                TokenType::LiteralF64("111e111".to_owned()),
                TokenType::LiteralF64("222e222".to_owned()),
                TokenType::LiteralF64("333e333".to_owned()),
                TokenType::LiteralF64("444e444".to_owned()),
                TokenType::LiteralF64("555e555".to_owned()),
            ]
        );
    }

    #[test]
    fn test_literals_signed_f64_exp() {
        let input = "+0e0 -1e1 +2e2 -3e3 +4e4 -5e5 +00E00 -11E11 +22E22 -33E33 +44E44 -55E55 +000e000 -111e111 +222e222 -333e333 +444e444 -555e555";
        let tokens = utils::collect_all_tokens(input);
        let token_ty = utils::extract_token_ty(&tokens);
        assert_eq!(
            token_ty,
            [
                TokenType::LiteralF64("+0e0".to_owned()),
                TokenType::LiteralF64("-1e1".to_owned()),
                TokenType::LiteralF64("+2e2".to_owned()),
                TokenType::LiteralF64("-3e3".to_owned()),
                TokenType::LiteralF64("+4e4".to_owned()),
                TokenType::LiteralF64("-5e5".to_owned()),
                TokenType::LiteralF64("+00E00".to_owned()),
                TokenType::LiteralF64("-11E11".to_owned()),
                TokenType::LiteralF64("+22E22".to_owned()),
                TokenType::LiteralF64("-33E33".to_owned()),
                TokenType::LiteralF64("+44E44".to_owned()),
                TokenType::LiteralF64("-55E55".to_owned()),
                TokenType::LiteralF64("+000e000".to_owned()),
                TokenType::LiteralF64("-111e111".to_owned()),
                TokenType::LiteralF64("+222e222".to_owned()),
                TokenType::LiteralF64("-333e333".to_owned()),
                TokenType::LiteralF64("+444e444".to_owned()),
                TokenType::LiteralF64("-555e555".to_owned()),
            ]
        );
    }

    #[test]
    fn test_literals_f64_signed_exp() {
        let input = "0e+0 1e-1 2e+2 3e-3 4e+4 5e-5 00E+00 11E-11 22E+22 33E-33 44E+44 55E-55 000e+000 111e-111 222e+222 333e-333 444e+444 555e-555";
        let tokens = utils::collect_all_tokens(input);
        let token_ty = utils::extract_token_ty(&tokens);
        assert_eq!(
            token_ty,
            [
                TokenType::LiteralF64("0e+0".to_owned()),
                TokenType::LiteralF64("1e-1".to_owned()),
                TokenType::LiteralF64("2e+2".to_owned()),
                TokenType::LiteralF64("3e-3".to_owned()),
                TokenType::LiteralF64("4e+4".to_owned()),
                TokenType::LiteralF64("5e-5".to_owned()),
                TokenType::LiteralF64("00E+00".to_owned()),
                TokenType::LiteralF64("11E-11".to_owned()),
                TokenType::LiteralF64("22E+22".to_owned()),
                TokenType::LiteralF64("33E-33".to_owned()),
                TokenType::LiteralF64("44E+44".to_owned()),
                TokenType::LiteralF64("55E-55".to_owned()),
                TokenType::LiteralF64("000e+000".to_owned()),
                TokenType::LiteralF64("111e-111".to_owned()),
                TokenType::LiteralF64("222e+222".to_owned()),
                TokenType::LiteralF64("333e-333".to_owned()),
                TokenType::LiteralF64("444e+444".to_owned()),
                TokenType::LiteralF64("555e-555".to_owned()),
            ]
        );
    }
}
