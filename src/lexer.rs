use std::str::Chars;

use miette::NamedSource;
use token::{Kind, Span, Token, Value};

use crate::error::{JoltError, LexerError, Result};

pub mod token;

pub struct Lexer<'a> {
    chars: Chars<'a>,
    file_name: &'a str,
    source: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(file_name: &'a str, source: &'a str) -> Self {
        Self {
            chars: source.chars(),
            file_name,
            source,
        }
    }

    pub fn lex(mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();

        loop {
            let token = self.next_token()?;
            let kind = token.kind;

            tokens.push(token);

            if kind == Kind::Eof {
                break;
            }
        }

        Ok(tokens)
    }

    fn offset(&self) -> usize {
        self.source.len() - self.chars.as_str().len()
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn basic(&self, kind: Kind, start: usize) -> Result<Token> {
        Ok(Token {
            kind,
            span: Span { start, end: self.offset() },
            value: Value::None,
        })
    }

    fn next_token(&mut self) -> Result<Token> {
        let mut start = self.offset();

        while let Some(c) = self.chars.next() {
            if c.is_whitespace() {
                start = self.offset();
                continue;
            }

            if c.is_alphabetic() || c == '_' {
                return self.make_identifier(c, start);
            }

            if c.is_ascii_digit() {
                return self.make_numeric_literal(c, start);
            }

            return match c {
                '+' => self.make_plus(start),
                '-' => self.make_minus(start),
                '*' => self.make_multiply(start),
                '/' => self.make_divide(start),
                '%' => self.make_modulo(start),
                '(' => self.basic(Kind::LeftParen, start),
                ')' => self.basic(Kind::RightParen, start),
                '[' => self.basic(Kind::LeftBracket, start),
                ']' => self.basic(Kind::RightBracket, start),
                '{' => self.basic(Kind::LeftBrace, start),
                '}' => self.basic(Kind::RightBrace, start),
                '<' => self.make_less_than(start),
                '>' => self.make_greater_than(start),
                '"' => self.make_string_literal(start),
                ';' => self.basic(Kind::Semicolon, start),
                ',' => self.basic(Kind::Comma, start),
                ':' => self.make_colon(start),
                '=' => self.make_eq(start),
                '!' => self.make_not(start),
                '&' => self.make_and(start),
                '|' => self.make_or(start),
                '^' => self.make_xor(start),
                '~' => self.basic(Kind::BitwiseNot, start),
                '.' => self.make_field_access(start),
                '@' => self.basic(Kind::Attribute, start),
                _ => Err(JoltError::Lexer(LexerError::UnexpectedCharacter {
                    character: c,
                    src: NamedSource::new(self.file_name, self.source.to_string()),
                    at: (start..self.offset()).into(),
                }))?,
            }
        }

        Ok(Token {
            kind: Kind::Eof,
            span: Span { start, end: start },
            value: Value::None,
        })
    }

    fn make_identifier(&mut self, c: char, start: usize) -> Result<Token> {
        let mut identifier = String::from(c);

        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.chars.next();
                identifier.push(c);
            } else {
                break;
            }
        }

        let kind = match identifier.as_ref() {
            "fun" => Kind::FunKeyword,
            "return" => Kind::ReturnKeyword,
            "if" => Kind::IfKeyword,
            "else" => Kind::ElseKeyword,
            "for" => Kind::ForKeyword,
            "in" => Kind::InKeyword,
            "while" => Kind::WhileKeyword,
            "var" => Kind::VarKeyword,
            "mut" => Kind::MutKeyword,
            "use" => Kind::UseKeyword,
            "struct" => Kind::StructKeyword,
            "trait" => Kind::TraitKeyword,
            _ => Kind::Identifier,
        };

        Ok(Token {
            kind,
            span: Span { start, end: self.offset() },
            value: if kind == Kind::Identifier {
                Value::String(identifier)
            } else {
                Value::None
            }
        })
    }

    fn make_numeric_literal(&mut self, c: char, start: usize) -> Result<Token> {
        let mut numeric = String::from(c);
        let mut has_decimal = false;

        // TODO: 0x, 0b, 0o support

        while let Some(c) = self.peek() {
            let last_is_digit = numeric.chars().last().map(|c| c.is_ascii_digit()).unwrap_or(false);

            if c == '.' {
                if has_decimal {
                    break;
                }

                let mut chars_clone = self.chars.clone();
                chars_clone.next();

                if chars_clone.next() == Some('.') {
                    break;
                }

                has_decimal = true;
            }

            if c.is_ascii_digit() || c == '_' || (last_is_digit && c == '.') {
                self.chars.next();
                numeric.push(c);

                if c == '.' {
                    has_decimal = true;
                }
            } else {
                break;
            }
        }

        let last = numeric.chars().last().expect("should always have at least one char");
        if !last.is_ascii_digit() {
            return Err(JoltError::Lexer(LexerError::TerminatedNumeric {
                character: last,
                src: NamedSource::new(self.file_name, self.source.to_string()),
                at: ((self.offset() - 1)..self.offset()).into(),
                help: match c {
                    '.' => Some("try adding a digit after the decimal point".to_string()),
                    '_' => Some("try removing the underscore".to_string()),
                    _ => None,
                }
            }))?;
        }

        Ok(Token {
            kind: Kind::NumericLiteral,
            span: Span { start, end: self.offset() },
            value: Value::Number(numeric),
        })
    }

    fn make_plus(&mut self, start: usize) -> Result<Token> {
        if self.peek() == Some('=') {
            self.chars.next();
            self.basic(Kind::PlusEq, start)
        } else {
            self.basic(Kind::Plus, start)
        }
    }

    fn make_minus(&mut self, start: usize) -> Result<Token> {
        match self.peek() {
            Some('>') => {
                self.chars.next();
                self.basic(Kind::Arrow, start)
            },
            Some('=') => {
                self.chars.next();
                self.basic(Kind::MinusEq, start)
            },
            _ => self.basic(Kind::Minus, start),
        }
    }

    fn make_less_than(&mut self, start: usize) -> Result<Token> {
        match self.peek() {
            Some('=') => {
                self.chars.next();
                self.basic(Kind::LessThanEq, start)
            },
            Some('<') => {
                self.chars.next();

                match self.peek() {
                    Some('=') => {
                        self.chars.next();
                        self.basic(Kind::BitwiseShiftLeftEq, start)
                    },
                    _ => self.basic(Kind::BitwiseShiftLeft, start)
                }
            },
            _ => self.basic(Kind::LessThan, start),
        }
    }

    fn make_greater_than(&mut self, start: usize) -> Result<Token> {
        match self.peek() {
            Some('=') => {
                self.chars.next();
                self.basic(Kind::GreaterThanEq, start)
            },
            Some('>') => {
                self.chars.next();

                match self.peek() {
                    Some('=') => {
                        self.chars.next();
                        self.basic(Kind::BitwiseShiftRightEq, start)
                    },
                    _ => self.basic(Kind::BitwiseShiftRight, start)
                }
            },
            _ => self.basic(Kind::GreaterThan, start),
        }
    }

    fn make_multiply(&mut self, start: usize) -> Result<Token> {
        if self.peek() == Some('=') {
            self.chars.next();
            self.basic(Kind::MultiplyEq, start)
        } else {
            self.basic(Kind::Multiply, start)
        }
    }

    fn make_divide(&mut self, start: usize) -> Result<Token> {
        match self.peek() {
            Some('=') => {
                self.chars.next();
                self.basic(Kind::DivideEq, start)
            },
            Some('/') => {
                self.chars.next();

                let mut comment = String::new();

                for c in self.chars.by_ref() {
                    if c == '\n' {
                        break;
                    }

                    comment.push(c);
                }

                Ok(Token {
                    kind: Kind::Comment,
                    span: Span { start, end: self.offset() },
                    value: Value::String(comment),
                })
            },
            Some('*') => {
                self.chars.next();

                let mut comment = String::new();
                let mut can_close = false;

                for c in self.chars.by_ref() {
                    if can_close {
                        if c == '/' {
                            break;
                        }

                        comment.push('*');
                    }

                    can_close = c == '*';

                    if !can_close {
                        comment.push(c);
                    }
                }

                Ok(Token {
                    kind: Kind::Comment,
                    span: Span { start, end: self.offset() },
                    value: Value::String(comment),
                })
            },
            _ => self.basic(Kind::Divide, start),
        }
    }

    fn make_modulo(&mut self, start: usize) -> Result<Token> {
        if self.peek() == Some('=') {
            self.chars.next();
            self.basic(Kind::ModuloEq, start)
        } else {
            self.basic(Kind::Modulo, start)
        }
    }

    fn make_string_literal(&mut self, start: usize) -> Result<Token> {
        let mut literal = String::new();
        let mut escaped = false;

        for c in self.chars.by_ref() {
            if escaped {
                match c {
                    '\\' => literal.push('\\'),
                    'n' => literal.push('\n'),
                    '0' => literal.push('\0'),
                    't' => literal.push('\t'),
                    'r' => literal.push('\r'),
                    '"' => literal.push('"'),
                    _ => Err(JoltError::Lexer(LexerError::UndefinedEscapeSequence {
                        character: c,
                        src: NamedSource::new(self.file_name, self.source.to_string()),
                        at: (start + literal.len(), 1).into(),
                    }))?
                }

                escaped = false;
                continue;
            }

            match c {
                '\\' => {
                    escaped = true;
                    continue;
                },
                '"' => {
                    break;
                },
                _ => (),
            }

            literal.push(c);
        }

        Ok(Token {
            kind: Kind::StringLiteral,
            span: Span { start, end: self.offset() },
            value: Value::String(literal),
        })
    }

    fn make_colon(&mut self, start: usize) -> Result<Token> {
        match self.peek() {
            Some(':') => {
                self.chars.next();
                self.basic(Kind::PathSeparator, start)
            },
            _ => self.basic(Kind::Colon, start)
        }
    }

    fn make_eq(&mut self, start: usize) -> Result<Token> {
        match self.peek() {
            Some('=') => {
                self.chars.next();
                self.basic(Kind::Eq, start)
            },
            Some('>') => {
                self.chars.next();
                self.basic(Kind::FatArrow, start)
            },
            _ => self.basic(Kind::Assign, start)
        }
    }

    fn make_not(&mut self, start: usize) -> Result<Token> {
        if self.peek() == Some('=') {
            self.chars.next();
            self.basic(Kind::NotEq, start)
        } else {
            self.basic(Kind::LogicalNot, start)
        }
    }

    fn make_and(&mut self, start: usize) -> Result<Token> {
        match self.peek() {
            Some('&') => {
                self.chars.next();
                self.basic(Kind::LogicalAnd, start)
            },
            Some('=') => {
                self.chars.next();
                self.basic(Kind::BitwiseAndEq, start)
            },
            _ => self.basic(Kind::BitwiseAnd, start)
        }
    }

    fn make_or(&mut self, start: usize) -> Result<Token> {
        match self.peek() {
            Some('|') => {
                self.chars.next();
                self.basic(Kind::LogicalOr, start)
            },
            Some('=') => {
                self.chars.next();
                self.basic(Kind::BitwiseOrEq, start)
            },
            _ => self.basic(Kind::BitwiseOr, start)
        }
    }

    fn make_xor(&mut self, start: usize) -> Result<Token> {
        match self.peek() {
            Some('=') => {
                self.chars.next();
                self.basic(Kind::BitwiseXorEq, start)
            },
            _ => self.basic(Kind::BitwiseXor, start)
        }
    }

    fn make_field_access(&mut self, start: usize) -> Result<Token> {
        match self.peek() {
            Some('.') => {
                self.chars.next();
                match self.peek() {
                    Some('=') => {
                        self.chars.next();
                        self.basic(Kind::RangeInclusive, start)
                    },
                    _ => self.basic(Kind::RangeExclusive, start)
                }
            },
            _ => self.basic(Kind::FieldAccess, start)
        }
    }
}
