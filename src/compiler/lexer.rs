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
                    _ => return lit! { self, TokenType::OpAdd, "+" }
                },
                '-' => try_character! {
                    self,
                    index,
                    '=' => return lit! { self, TokenType::OpAssignSub, "-=" },
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
                        let content = self.next_integer(10);
                        return dy!(self, TokenType::LiteralInteger(content.clone()), content)
                    }
                },
                '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                    let decimal = self.next_integer(10);

                    token_builder!(self, builder, content);
                    content.push_str(&decimal);

                    try_character! {
                        self,
                        index,
                        '.' => {
                            content.push('.');
                            self.advance();

                            let frac = self.next_integer(10);

                            if frac.is_empty() {
                                return builder.build(TokenType::Error(TokenError::F64LiteralTerminatedWithDot), content);
                            }

                            content.push_str(&frac);

                            return dy!(self, TokenType::LiteralF64(content.clone()), content);
                        }
                        _ => {
                            return dy!(self, TokenType::LiteralInteger(content.clone()), content);
                        }
                    }
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

    fn next_integer(&mut self, radix: u32) -> String {
        let mut integer = String::with_capacity(8);

        while self.remain() {
            let char = self.char();

            if !char.is_digit(radix) {
                break;
            }

            integer.push(char);
            self.index += 1;
        }

        self.line_offset += integer.len();
        integer
    }

    fn next_exp(&mut self, radix: u32) -> String {
        if self.remain() {
            return "".to_owned();
        }

        let mut char = self.char();

        if char != 'e' && char != 'E' {
            return "".to_owned();
        }

        let mut exp = String::with_capacity(8);
        exp.push(char);

        let mut index = self.index + 1;
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
        self.index += exp.len();
        self.line_offset += exp.len();

        exp.push_str(&self.next_integer(10));
        exp
    }

    // fn next_literal_suffix(&mut self) -> String {
    //     if !self.remain() {
    //         return "".to_owned();
    //     }

    //     match unsafe { self.current() } {
    //         'b' => try_character!(self, {
    //             'y' => { try_character!(self, {
    //                 't' => { try_character!(self, {
    //                     'e' => { return string!(self, "byte"); }
    //                 }) }
    //                 _ => { return "".to_owned(); }
    //             }) }
    //             _ => { return "".to_owned() }
    //         }),
    //         _ => return "".to_owned(),
    //     }
    // }
}

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
    fn test_statements() {
        let input = "let x = if true { 1 } else { 0 }";
        let tokens = utils::collect_all_tokens(input);
        let token_ty = utils::extract_token_ty(&tokens);
        assert_eq!(
            token_ty,
            [
                TokenType::KeywordLet,
                TokenType::Id,
                TokenType::OpAssign,
                TokenType::KeywordIf,
                TokenType::LiteralBool(true),
                TokenType::PuncBraceL,
                TokenType::LiteralInteger("1".to_owned()),
                TokenType::PuncBraceR,
                TokenType::KeywordElse,
                TokenType::PuncBraceL,
                TokenType::LiteralInteger("0".to_owned()),
                TokenType::PuncBraceR,
            ]
        );
    }
}
