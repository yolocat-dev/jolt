use miette::NamedSource;
use node::{Node, Precedence};

use crate::{error::{JoltError, ParserError, Result}, lexer::token::{Kind, Token, Value}};

pub struct Parser<'a, I> where
    I: Iterator<Item = Token> + Clone
{
    tokens: I,
    filename: &'a str,
    source: &'a str,
}

pub mod node;

const DEBUG: bool = false;

macro_rules! d_expect {
    ($($kind:expr),*) => {
        #[cfg(debug_assertions)]
        {
            if DEBUG {
                $(eprintln!("[PARSER:{:<04}] Expecting {:#?}...", line!(), $kind);)*
            }
        }
    }
}

macro_rules! d_advance {
    ($self:expr) => {{
        let token = $self.advance();

        #[cfg(debug_assertions)]
        {
            if DEBUG {
                if let Some(token) = &token {
                    eprintln!("[PARSER:{:<04}] Advanced {:#?}\n", line!(), token.kind);
                }
            }
        }

        token
    }}
}

macro_rules! parse_path {
    ($self:expr) => {{
        let mut path = String::new();

        while let Some(token) = $self.peek() {
            d_expect!(Kind::Identifier);
            match token.kind {
                Kind::Identifier => {
                    d_advance!($self);

                    path.push_str(&match token.value {
                        Value::String(path) => path,
                        _ => panic!("Kind::Identifier should always have a String value"),
                    });

                    if let Some(token) = $self.peek() {
                        d_expect!(Kind::PathSeparator);
                        match token.kind {
                            Kind::PathSeparator => {
                                d_advance!($self);

                                path.push_str("::");
                            },
                            _ => break,
                        }
                    }
                },
                _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                    token: token.kind,
                    src: NamedSource::new($self.filename, $self.source.to_string()),
                    at: (token.span.start..token.span.end).into(),
                    label: "expected type".to_string(),
                    help: Some("fields are required to have an explicit type".to_string()),
                }))?
            }
        }

        if $self.is_at_end() {
            Err(JoltError::Parser(ParserError::TerminatedStatement {
                src: NamedSource::new($self.filename, $self.source.to_string()),
                at: ($self.source.len() - 1, 1).into(),
                label: "expected type".to_string(),
                help: Some("fields are required to have an explicit type".to_string()),
            }))
        } else {
            Ok(path)
        }
    }}
}

impl<'a, I> Parser<'a, I> where
    I: Iterator<Item = Token> + Clone
{
    pub fn new(tokens: I, filename: &'a str, source: &'a str) -> Self {
        Self {
            tokens,
            filename,
            source,
        }
    }

    fn is_at_end(&self) -> bool {
        match self.peek() {
            None => true,
            Some(token) => token.kind == Kind::Eof,
        }
    }

    fn peek(&self) -> Option<Token> {
        self.tokens.clone().next()
    }

    fn advance(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    pub fn parse(mut self) -> Result<Node> {
        let mut file = vec![];

        while !self.is_at_end() {
            file.extend(self.parse_file_segment()?);
        }

        Ok(Node::File(file))
    }

    fn expect_help(&mut self, kind: Kind, label: &str, help: Option<&str>) -> Result<Token> {
        d_expect!(kind);
        match self.peek() {
            Some(token) => if token.kind == kind {
                d_advance!(self);

                Ok(token)
            } else {
                Err(JoltError::Parser(ParserError::UnexpectedToken {
                    token: token.kind,
                    src: NamedSource::new(self.filename, self.source.to_string()),
                    at: (token.span.start..token.span.end).into(),
                    label: label.to_string(),
                    help: help.map(|s| s.to_string()),
                }))?
            },
            None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                src: NamedSource::new(self.filename, self.source.to_string()),
                at: (self.source.len() - 1, 1).into(),
                label: label.to_string(),
                help: help.map(|s| s.to_string()),
            }))?
        }
    }

    fn expect(&mut self, kind: Kind, label: &str) -> Result<Token> {
        self.expect_help(kind, label, None)
    }

    fn expect_semi(&mut self) -> Result<()> {
        self.expect_help(Kind::Semicolon, "expected semicolon (`;`)", Some("try adding a semicolon (`;`)"))?;

        Ok(())
    }

    fn parse_file_segment(&mut self) -> Result<Vec<Node>> {
        let token = self.peek().expect("parse_file_segment are not allowed to be called if is_at_end()");

        d_expect!(Kind::FunKeyword, Kind::StructKeyword, Kind::TraitKeyword, Kind::PubKeyword, Kind::Attribute, Kind::UseKeyword);
        match token.kind {
            Kind::FunKeyword => Ok(vec![self.parse_function(false)?]),
            Kind::StructKeyword => Ok(vec![self.parse_struct(false, vec![])?]),
            Kind::TraitKeyword => Ok(vec![self.parse_trait(false)?]),
            Kind::PubKeyword => {
                d_advance!(self);

                d_expect!(Kind::FunKeyword, Kind::StructKeyword, Kind::TraitKeyword);
                match self.peek() {
                    Some(token) => match token.kind {
                        Kind::FunKeyword => Ok(vec![self.parse_function(true)?]),
                        Kind::StructKeyword => Ok(vec![self.parse_struct(true, vec![])?]),
                        Kind::TraitKeyword => Ok(vec![self.parse_trait(true)?]),
                        _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                            token: token.kind,
                            src: NamedSource::new(self.filename, self.source.to_string()),
                            at: (self.source.len() - 1, 1).into(),
                            label: "expected function, struct or trait definition".to_string(),
                            help: None,
                        }))?
                    },
                    None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                        src: NamedSource::new(self.filename, self.source.to_string()),
                        at: (self.source.len() - 1, 1).into(),
                        label: "expected function, struct or trait definition".to_string(),
                        help: None,
                    }))?
                }
            },
            Kind::Attribute => Ok(vec![self.parse_attribute()?]),
            Kind::UseKeyword => self.parse_use(),
            _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                token: token.kind,
                src: NamedSource::new(self.filename, self.source.to_string()),
                at: (token.span.start..token.span.end).into(),
                label: "expected `use` keyword, function, struct or trait definition".to_string(),
                help: Some("try moving this into a `fun main()`".to_string())
            }))?
        }
    }

    fn parse_statement(&mut self) -> Result<Node> {
        let token = self.peek().expect("parse_statement are not allowed to be called if is_at_end()");

        d_expect!(Kind::ReturnKeyword, Kind::WhileKeyword, Kind::ForKeyword, Kind::VarKeyword, Kind::MutKeyword, Kind::LeftBrace);
        match token.kind {
            Kind::ReturnKeyword => self.parse_return(),
            Kind::WhileKeyword => self.parse_while(),
            Kind::ForKeyword => self.parse_for(),
            Kind::VarKeyword => self.parse_var(false),
            Kind::MutKeyword => self.parse_var(true),
            Kind::LeftBrace => self.parse_block(),
            _ => self.parse_expression(),
        }
    }

    fn parse_function(&mut self, public: bool) -> Result<Node> {
        d_advance!(self);

        let name = parse_path!(self)?;

        self.expect(Kind::LeftParen, "expected opening parentheses (`(`)")?;

        let mut params = vec![];

        while let Some(next) = self.peek() {
            d_expect!(Kind::RightParen, Kind::MutKeyword, Kind::Identifier);
            let name = match next.kind {
                Kind::RightParen => {
                    d_advance!(self);
                    break;
                },
                Kind::MutKeyword => {
                    d_advance!(self);

                    match self.expect(Kind::Identifier, "expected parameter name")?.value {
                        Value::String(name) => name,
                        _ => panic!("Kind::Identifier should always have a String value"),
                    }
                },
                Kind::Identifier => {
                    d_advance!(self);

                    match next.value {
                        Value::String(name) => name,
                        _ => panic!("Kind::Identifier should always have a String value"),
                    }
                },
                _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                    token: next.kind,
                    src: NamedSource::new(self.filename, self.source.to_string()),
                    at: (next.span.start..next.span.end).into(),
                    label: "expected parameter or closing parentheses (`)`)".to_string(),
                    help: None,
                }))?
            };

            match name.as_ref() {
                "self" => {
                    d_expect!(Kind::Colon, "none");
                    let type_ = match self.peek() {
                        Some(token) => match token.kind {
                            Kind::Colon => {
                                d_advance!(self);

                                parse_path!(self)?
                            },
                            _ => "Self".to_string(),
                        }
                        None => "Self".to_string(),
                    };

                    params.push(Node::Parameter {
                        name,
                        type_,
                    });
                },
                _ => {
                    self.expect(Kind::Colon, "expected colon (`:`)")?;

                    let type_ = parse_path!(self)?;

                    params.push(Node::Parameter {
                        name,
                        type_,
                    });
                }
            }

            d_expect!(Kind::Comma, Kind::RightParen);
            match self.peek() {
                Some(token) => match token.kind {
                    Kind::Comma => {
                        d_advance!(self);
                    },
                    Kind::RightParen => {
                        d_advance!(self);
                        break;
                    },
                    _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                        token: token.kind,
                        src: NamedSource::new(self.filename, self.source.to_string()),
                        at: (token.span.start..token.span.end).into(),
                        label: "expected comma or closing parentheses (`,` or `)`)".to_string(),
                        help: None,
                    }))?
                },
                None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                    src: NamedSource::new(self.filename, self.source.to_string()),
                    at: (self.source.len() - 1, 1).into(),
                    label: "expected comma or closing parentheses (`,` or `)`)".to_string(),
                    help: None,
                }))?,
            }
        }

        d_expect!(Kind::LeftBrace, Kind::Arrow, Kind::Semicolon);
        let return_type = match self.peek() {
            Some(token) => match token.kind {
                Kind::LeftBrace => {
                    None
                },
                Kind::Semicolon => {
                    None
                },
                Kind::Arrow => {
                    d_advance!(self);

                    Some(parse_path!(self)?)
                },
                _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                    token: token.kind,
                    src: NamedSource::new(self.filename, self.source.to_string()),
                    at: (token.span.start..token.span.end).into(),
                    label: "expected opening brace, arrow or semicolon (`{`, `->` or `;`)".to_string(),
                    help: None,
                }))?
            },
            None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                src: NamedSource::new(self.filename, self.source.to_string()),
                at: (self.source.len() - 1, 1).into(),
                label: "expected opening brace or semicolon (`{` or `;`)".to_string(),
                help: None,
            }))?
        };

        d_expect!(Kind::LeftBrace, Kind::Semicolon);
        let body = match self.peek() {
            Some(token) => match token.kind {
                Kind::LeftBrace => {
                    Some(self.parse_block()?)
                },
                Kind::Semicolon => {
                    self.advance();

                    None
                },
                _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                    token: token.kind,
                    src: NamedSource::new(self.filename, self.source.to_string()),
                    at: (token.span.start..token.span.end).into(),
                    label: "expected opening brace or semicolon (`{` or `;`)".to_string(),
                    help: None,
                }))?
            },
            None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                src: NamedSource::new(self.filename, self.source.to_string()),
                at: (self.source.len() - 1, 1).into(),
                label: "expected opening brace or semicolon (`{` or `;`)".to_string(),
                help: None,
            }))?
        };

        Ok(Node::FunctionDef {
            public,
            name,
            params,
            body: body.map(Box::new),
            return_type,
        })
    }

    fn parse_use(&mut self) -> Result<Vec<Node>> {
        // TODO: fix this... this does not error at any point and can cause many issues with syntax
        //       errors
        d_advance!(self);

        let mut uses = vec![];
        let mut path = String::new();

        while let Some(token) = self.peek() {
            d_expect!(Kind::Identifier, Kind::PathSeparator, Kind::LeftBrace, Kind::Semicolon);
            match token.kind {
                Kind::Identifier => {
                    if !path.is_empty() && !path.ends_with("::") {
                        path.push_str("::");
                    }

                    match token.value {
                        Value::String(name) => path.push_str(&name),
                        _ => panic!("Kind::Identifier should always have a String value"),
                    }

                    d_advance!(self);
                },
                Kind::PathSeparator => {
                    path.push_str("::");
                    d_advance!(self);
                },
                Kind::LeftBrace => {
                    d_advance!(self);

                    let nested_uses = self.parse_use_brace(path.clone())?;
                    uses.extend(nested_uses);

                    path.clear();
                },
                Kind::Semicolon => {
                    if !path.is_empty() {
                        uses.push(Node::Use(path));
                    }

                    d_advance!(self);
                    break;
                },
                _ => {
                    Err(JoltError::Parser(ParserError::UnexpectedToken {
                        token: token.kind,
                        src: NamedSource::new(self.filename, self.source.to_string()),
                        at: (token.span.start..token.span.end).into(),
                        label: "unexpected token in use statement".to_string(),
                        help: None,
                    }))?
                }
            }
        }

        Ok(uses)
    }

    fn parse_use_brace(&mut self, prefix: String) -> Result<Vec<Node>> {
        let mut uses = vec![];
        let mut path = prefix.clone();

        while let Some(token) = self.peek() {
            d_expect!(Kind::Identifier, Kind::PathSeparator, Kind::LeftBrace, Kind::RightBrace, Kind::Comma);
            match token.kind {
                Kind::Identifier => {
                    if !path.is_empty() && !path.ends_with("::") {
                        path.push_str("::");
                    }

                    match token.value {
                        Value::String(name) => path.push_str(&name),
                        _ => panic!("Kind::Identifier should always have a String value"),
                    }

                    d_advance!(self);
                },
                Kind::PathSeparator => {
                    path.push_str("::");
                    d_advance!(self);
                },
                Kind::LeftBrace => {
                    d_advance!(self);

                    let nested_uses = self.parse_use_brace(path.clone())?;
                    uses.extend(nested_uses);
                },
                Kind::RightBrace => {
                    if !path.is_empty() && !path.ends_with("::") {
                        if path == "self" {
                            uses.push(Node::Use(prefix.clone()));
                        } else {
                            uses.push(Node::Use(path.clone()));
                        }
                    }

                    d_advance!(self);
                    break;
                },
                Kind::Comma => {
                    if !path.is_empty() && !path.ends_with("::") {
                        if path == "self" {
                            uses.push(Node::Use(prefix.clone()));
                        } else {
                            uses.push(Node::Use(path.clone()));
                        }
                    }

                    path = prefix.clone();
                    d_advance!(self);
                },
                _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                    token: token.kind,
                    src: NamedSource::new(self.filename, self.source.to_string()),
                    at: (token.span.start..token.span.end).into(),
                    label: "unexpected token in use statement".to_string(),
                    help: None,
                }))?
            }
        }

        Ok(uses)
    }

    fn parse_block(&mut self) -> Result<Node> {
        d_advance!(self);

        let mut block = vec![];
        let mut last_stmt = None;

        while !self.is_at_end() && self.peek().expect("not allowed to be called if is_at_end()").kind != Kind::RightBrace {
            if let Some(stmt) = last_stmt {
                block.push(stmt);
                last_stmt = None;
            }

            let stmt = self.parse_statement()?;

            if matches!(stmt, Node::If { .. }) {
                last_stmt = Some(stmt);
                continue;
            }

            d_expect!(Kind::Semicolon, "none");
            if self.peek().is_some_and(|t| t.kind == Kind::Semicolon) {
                d_advance!(self);
                block.push(stmt);
            } else {
                block.push(Node::BlockReturn(Box::new(stmt)));
                break;
            }
        }

        if let Some(stmt) = last_stmt {
            block.push(Node::BlockReturn(Box::new(stmt)));
        }

        if self.is_at_end() {
            return Err(JoltError::Parser(ParserError::TerminatedStatement {
                src: NamedSource::new(self.filename, self.source.to_string()),
                at: (self.source.len() - 1, 1).into(),
                label: "expected semicolon or closing brace (`;` or `}`)".to_string(),
                help: Some("try adding a semicolon or closing brace (`;` or `}`)".to_string()),
            }))?;
        } else {
            let next = self.peek().expect("is_at_end() above should rule this out");

            d_expect!(Kind::RightBrace);
            if next.kind != Kind::RightBrace {
                return Err(JoltError::Parser(ParserError::UnexpectedToken {
                    token: next.kind,
                    src: NamedSource::new(self.filename, self.source.to_string()),
                    at: (next.span.start..next.span.end).into(),
                    label: "expected semicolon or closing brace (`;` or `}`)".to_string(),
                    help: Some("try adding a semicolon or closing brace (`;` or `}`)".to_string()),
                }))?;
            }
        }

        d_advance!(self);

        Ok(Node::Block(block))
    }

    fn parse_return(&mut self) -> Result<Node> {
        d_advance!(self);

        let expr = self.parse_expression()?;

        Ok(Node::Return(Box::new(expr)))
    }

    fn parse_if(&mut self) -> Result<Node> {
        d_advance!(self);

        let condition = self.parse_expression()?;

        d_expect!(Kind::LeftBrace);
        let body = match self.peek() {
            Some(token) => match token.kind {
                Kind::LeftBrace => {
                    self.parse_block()?
                },
                _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                    token: token.kind,
                    src: NamedSource::new(self.filename, self.source.to_string()),
                    at: (token.span.start..token.span.end).into(),
                    label: "expected opening parentheses (`(`)".to_string(),
                    help: None,
                }))?
            },
            None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                src: NamedSource::new(self.filename, self.source.to_string()),
                at: (self.source.len() - 1, 1).into(),
                label: "expected opening parentheses (`(`)".to_string(),
                help: None,
            }))?
        };

        let mut else_ifs = vec![];
        let mut else_body = None;

        while let Some(else_token) = self.peek() {
            d_expect!(Kind::ElseKeyword, "none");
            if else_token.kind == Kind::ElseKeyword {
                d_advance!(self);

                match self.peek() {
                    Some(token) => {
                        d_expect!(Kind::IfKeyword, Kind::LeftBrace);
                        match token.kind {
                            Kind::IfKeyword => {
                                d_advance!(self);

                                let condition = self.parse_expression()?;

                                d_expect!(Kind::LeftBrace);
                                let body = match self.peek() {
                                    Some(token) => match token.kind {
                                        Kind::LeftBrace => {
                                            self.parse_block()?
                                        },
                                        _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                                            token: token.kind,
                                            src: NamedSource::new(self.filename, self.source.to_string()),
                                            at: (token.span.start..token.span.end).into(),
                                            label: "expected opening parentheses (`(`)".to_string(),
                                            help: None,
                                        }))?
                                    },
                                    None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                                        src: NamedSource::new(self.filename, self.source.to_string()),
                                        at: (self.source.len() - 1, 1).into(),
                                        label: "expected opening parentheses (`(`)".to_string(),
                                        help: None,
                                    }))?
                                };

                                else_ifs.push((Box::new(condition), Box::new(body)));
                            },
                            Kind::LeftBrace => {
                                else_body = Some(self.parse_block()?);
                                break;
                            },
                            _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                                token: token.kind,
                                src: NamedSource::new(self.filename, self.source.to_string()),
                                at: (token.span.start..token.span.end).into(),
                                label: "expected if keyword or block".to_string(),
                                help: None,
                            }))?
                        }
                    },
                    None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                        src: NamedSource::new(self.filename, self.source.to_string()),
                        at: (self.source.len() - 1, 1).into(),
                        label: "expected if keyword or block".to_string(),
                        help: None,
                    }))?
                }
            } else {
                break;
            }
        }

        Ok(Node::If {
            condition: Box::new(condition),
            body: Box::new(body),
            else_ifs,
            else_body: else_body.map(Box::new),
        })
    }

    fn parse_while(&mut self) -> Result<Node> {
        d_advance!(self);

        let condition = self.parse_expression()?;

        d_expect!(Kind::LeftBrace);
        let body = match self.peek() {
            Some(token) => match token.kind {
                Kind::LeftBrace => {
                    self.parse_block()?
                },
                _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                    token: token.kind,
                    src: NamedSource::new(self.filename, self.source.to_string()),
                    at: (token.span.start..token.span.end).into(),
                    label: "expected opening parentheses (`(`)".to_string(),
                    help: None,
                }))?
            },
            None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                src: NamedSource::new(self.filename, self.source.to_string()),
                at: (self.source.len() - 1, 1).into(),
                label: "expected opening parentheses (`(`)".to_string(),
                help: None,
            }))?
        };

        Ok(Node::While {
            condition: Box::new(condition),
            body: Box::new(body),
        })
    }

    fn parse_for(&mut self) -> Result<Node> {
        d_advance!(self);

        let var_name = match self.expect(Kind::Identifier, "expected variable name (identifier)")?.value {
            Value::String(var_name) => var_name,
            _ => panic!("Kind::Identifier should always have a String value"),
        };

        self.expect(Kind::InKeyword, "expected `in` keyword")?;

        let iterable = self.parse_expression()?;

        self.expect(Kind::LeftBrace, "expected opening brace (`{`)")?;

        let body = self.parse_block()?;

        Ok(Node::For {
            var_name,
            iterable: Box::new(iterable),
            body: Box::new(body),
        })
    }

    fn parse_var(&mut self, mutable: bool) -> Result<Node> {
        d_advance!(self);

        if mutable {
            self.expect(Kind::VarKeyword, "expected `var` keyword")?;
        }

        let name = match self.expect(Kind::Identifier, "expected variable name (identifier)")?.value {
            Value::String(name) => name,
            _ => panic!("Kind::Identifier should always have a String value"),
        };

        let mut type_ = None;
        let mut value = None;

        d_expect!(Kind::Assign, Kind::Colon, Kind::Semicolon);
        match self.peek() {
            Some(token) => match token.kind {
                Kind::Assign => {
                    d_advance!(self);

                    value = Some(self.parse_expression()?);
                },
                Kind::Colon => {
                    d_advance!(self);

                    type_ = match self.expect(Kind::Identifier, "expected variable type")?.value {
                        Value::String(type_) => Some(type_),
                        _ => panic!("Kind::Identifier should always have a String value"),
                    };

                    d_expect!(Kind::Assign, Kind::Semicolon);
                    match self.peek() {
                        Some(token) => match token.kind {
                            Kind::Assign => {
                                d_advance!(self);

                                value = Some(self.parse_expression()?);
                            },
                            Kind::Semicolon => (),
                            _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                                token: token.kind,
                                src: NamedSource::new(self.filename, self.source.to_string()),
                                at: (token.span.start..token.span.end).into(),
                                label: "expected assignment operator or semicolon (`=` or `;`)".to_string(),
                                help: None,
                            }))?
                        },
                        None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                            src: NamedSource::new(self.filename, self.source.to_string()),
                            at: (self.source.len() - 1, 1).into(),
                            label: "expected assignment operator or semicolon (`=` or `;`)".to_string(),
                            help: None,
                        }))?
                    }
                },
                Kind::Semicolon => (),
                _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                    token: token.kind,
                    src: NamedSource::new(self.filename, self.source.to_string()),
                    at: (token.span.start..token.span.end).into(),
                    label: "expected colon, assignment operator or semicolon (`:`, `=` or `;`)".to_string(),
                    help: None,
                }))?

            },
            None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                src: NamedSource::new(self.filename, self.source.to_string()),
                at: (self.source.len() - 1, 1).into(),
                label: "expected colon, assignment operator or semicolon (`:`, `=` or `;`)".to_string(),
                help: None,
            }))?
        }

        Ok(Node::VariableDecl {
            mutable,
            name,
            type_,
            value: value.map(Box::new),
        })
    }

    fn parse_expression(&mut self) -> Result<Node> {
        self.parse_expression_with_precedence(Precedence::Lowest)
    }

    fn parse_expression_with_precedence(&mut self, precedence: Precedence) -> Result<Node> {
        let mut left = if let Some(token) = self.peek() {
            d_expect!(Kind::Minus, Kind::LogicalNot, "none");
            match token.kind {
                Kind::Minus | Kind::LogicalNot => {
                    d_advance!(self);

                    let operand = self.parse_expression_with_precedence(Precedence::Unary)?;

                    Node::UnaryExpr {
                        operator: token.kind,
                        operand: Box::new(operand)
                    }
                },
                Kind::IfKeyword => self.parse_if()?,
                _ => self.parse_primary()?,
            }
        } else {
            Err(JoltError::Parser(ParserError::TerminatedStatement {
                src: NamedSource::new(self.filename, self.source.to_string()),
                at: (self.source.len() - 1, 1).into(),
                label: "expected expression".to_string(),
                help: None,
            }))?
        };

        while !self.is_at_end() && precedence < Precedence::of(&self.peek().expect("not allowed to be called if is_at_end()")) {
            let token = self.peek().expect("not allowed to be called if is_at_end()");

            d_expect!(Kind::Assign, Kind::PlusEq, Kind::MinusEq, Kind::MultiplyEq, Kind::DivideEq, Kind::ModuloEq, Kind::BitwiseAndEq, Kind::BitwiseOrEq, Kind::BitwiseXorEq, Kind::BitwiseShiftLeftEq, Kind::BitwiseShiftRightEq, Kind::RangeExclusive, Kind::RangeInclusive, Kind::LogicalOr, Kind::LogicalAnd, Kind::BitwiseOr, Kind::BitwiseXor, Kind::BitwiseAnd, Kind::Eq, Kind::NotEq, Kind::LessThan, Kind::LessThanEq, Kind::GreaterThan, Kind::GreaterThanEq, Kind::BitwiseShiftLeft, Kind::BitwiseShiftRight, Kind::Plus, Kind::Minus, Kind::Multiply, Kind::Divide, Kind::Modulo);
            let precedence = match token.kind {
                Kind::Assign
                    | Kind::PlusEq
                    | Kind::MinusEq
                    | Kind::MultiplyEq
                    | Kind::DivideEq
                    | Kind::ModuloEq
                    | Kind::BitwiseAndEq
                    | Kind::BitwiseOrEq
                    | Kind::BitwiseXorEq
                    | Kind::BitwiseShiftLeftEq
                    | Kind::BitwiseShiftRightEq
                    | Kind::RangeExclusive
                    | Kind::RangeInclusive
                    | Kind::LogicalOr
                    | Kind::LogicalAnd
                    | Kind::BitwiseOr
                    | Kind::BitwiseXor
                    | Kind::BitwiseAnd
                    | Kind::Eq
                    | Kind::NotEq
                    | Kind::LessThan
                    | Kind::LessThanEq
                    | Kind::GreaterThan
                    | Kind::GreaterThanEq
                    | Kind::BitwiseShiftLeft
                    | Kind::BitwiseShiftRight
                    | Kind::Plus
                    | Kind::Minus
                    | Kind::Multiply
                    | Kind::Divide
                    | Kind::Modulo
                => Ok(Precedence::of(&token)),
                _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                    token: token.kind,
                    src: NamedSource::new(self.filename, self.source.to_string()),
                    at: (token.span.start..token.span.end).into(),
                    label: "expected operator".to_string(),
                    help: None,
                }))
            }?;

            let operator = self.advance().expect("peek() above should rule this out");

            let right = self.parse_expression_with_precedence(precedence)?;

            left = Node::BinaryExpr {
                left: Box::new(left),
                operator: operator.kind,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_primary(&mut self) -> Result<Node> {
        d_expect!(Kind::Identifier, Kind::NumericLiteral, Kind::StringLiteral, Kind::LeftParen);
        match self.advance() {
            Some(token) => match token.kind {
                Kind::Identifier => self.parse_identifier(token),
                Kind::NumericLiteral => self.parse_numeric(token),
                Kind::StringLiteral => self.parse_string(token),
                Kind::LeftParen => {
                    let expr = self.parse_expression()?;

                    d_expect!(Kind::RightParen);
                    match self.peek() {
                        Some(token) => match token.kind {
                            Kind::RightParen => {
                                d_advance!(self);
                                Ok(expr)
                            },
                            _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                                token: token.kind,
                                src: NamedSource::new(self.filename, self.source.to_string()),
                                at: (token.span.start..token.span.end).into(),
                                label: "expected closing parentheses (`)`)".to_string(),
                                help: Some("try adding a closing parentheses (`)`) to close the expression".to_string()),
                            }))?
                        },
                        None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                            src: NamedSource::new(self.filename, self.source.to_string()),
                            at: (self.source.len() - 1, 1).into(),
                            label: "expected closing parentheses (`)`)".to_string(),
                            help: Some("try adding a closing parentheses (`)`) to close the expression".to_string()),
                        }))?
                    }
                },
                _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                    token: token.kind,
                    src: NamedSource::new(self.filename, self.source.to_string()),
                    at: (token.span.start..token.span.end).into(),
                    label: "expected expression".to_string(),
                    help: None,
                }))?
            },
            None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                src: NamedSource::new(self.filename, self.source.to_string()),
                at: (self.source.len() - 1, 1).into(),
                label: "expected expression".to_string(),
                help: None,
            }))?
        }
    }

    fn parse_identifier(&mut self, identifier: Token) -> Result<Node> {
        let mut expr = match identifier.value {
            Value::String(ident) => Node::Identifier(ident),
            _ => panic!("Kind::Identifier should always have a String value"),
        };

        while let Some(token) = self.peek() {
            d_expect!(Kind::LeftParen, Kind::PathSeparator, Kind::FieldAccess);
            match token.kind {
                Kind::PathSeparator => {
                    d_advance!(self);

                    let ident = self.expect(Kind::Identifier, "expected identifier after path separator")?;
                    let name = match ident.value {
                        Value::String(name) => name,
                        _ => panic!("Kind::Identifier should always have a String value"),
                    };

                    expr = Node::PathAccess {
                        base: Box::new(expr),
                        segment: name,
                    };
                },
                Kind::FieldAccess => {
                    d_advance!(self);

                    let ident = self.expect(Kind::Identifier, "expected field name")?;
                    let name = match ident.value {
                        Value::String(name) => name,
                        _ => panic!("Kind::Identifier should always have a String value"),
                    };

                    expr = Node::FieldAccess {
                        object: Box::new(expr),
                        field: name,
                    };
                },
                Kind::LeftParen => {
                    d_advance!(self);

                    let mut args = vec![];

                    while let Some(next) = self.peek() {
                        d_expect!(Kind::RightParen, "none");
                        if next.kind == Kind::RightParen {
                            d_advance!(self);
                            break;
                        }

                        args.push(self.parse_expression()?);

                        d_expect!(Kind::Comma, Kind::RightParen);
                        match self.peek() {
                            Some(token) => match token.kind {
                                Kind::Comma => {
                                    d_advance!(self);
                                },
                                Kind::RightParen => {
                                    d_advance!(self);
                                    break;
                                },
                                _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                                    token: token.kind,
                                    src: NamedSource::new(self.filename, self.source.to_string()),
                                    at: (token.span.start..token.span.end).into(),
                                    label: "expected comma or closing parentheses (`,` or `)`)".to_string(),
                                    help: None,
                                }))?
                            },
                            None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                                src: NamedSource::new(self.filename, self.source.to_string()),
                                at: (self.source.len() - 1, 1).into(),
                                label: "expected comma or closing parentheses (`,` or `)`)".to_string(),
                                help: None,
                            }))?
                        }
                    }

                    expr = Node::FunctionCall(Box::new(expr), args);
                },
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_numeric(&mut self, numeric: Token) -> Result<Node> {
        let value = match numeric.value {
            Value::Number(value) => value,
            _ => panic!("Kind::NumericLiteral should always have a Number value"),
        };

        let implicit = Ok(if value.contains('.') {
            Node::ImplicitFloat(value.clone())
        } else {
            Node::ImplicitInteger(value.clone())
        });

        d_expect!(Kind::Identifier, "none");
        match self.peek() {
            None => implicit,
            Some(token) => match token.kind {
                Kind::Identifier => match token.value {
                    Value::String(name) => {
                        match &name[..] {
                            "i8" => {
                                d_advance!(self);

                                Ok(Node::I8(value.parse::<i8>().map_err(|_| JoltError::Parser(ParserError::NumberOverflow {
                                    number: value,
                                    target: name,
                                    src: NamedSource::new(self.filename, self.source.to_string()),
                                    at: (numeric.span.start..numeric.span.end).into(),
                                }))?))
                            },
                            "i16" => {
                                d_advance!(self);

                                Ok(Node::I16(value.parse::<i16>().map_err(|_| JoltError::Parser(ParserError::NumberOverflow {
                                    number: value,
                                    target: name,
                                    src: NamedSource::new(self.filename, self.source.to_string()),
                                    at: (numeric.span.start..numeric.span.end).into(),
                                }))?))
                            },
                            "i32" => {
                                d_advance!(self);

                                Ok(Node::I32(value.parse::<i32>().map_err(|_| JoltError::Parser(ParserError::NumberOverflow {
                                    number: value,
                                    target: name,
                                    src: NamedSource::new(self.filename, self.source.to_string()),
                                    at: (numeric.span.start..numeric.span.end).into(),
                                }))?))
                            },
                            "i64" => {
                                d_advance!(self);

                                Ok(Node::I64(value.parse::<i64>().map_err(|_| JoltError::Parser(ParserError::NumberOverflow {
                                    number: value,
                                    target: name,
                                    src: NamedSource::new(self.filename, self.source.to_string()),
                                    at: (numeric.span.start..numeric.span.end).into(),
                                }))?))
                            },
                            "isize" => {
                                d_advance!(self);

                                Ok(Node::ISize(value.parse::<isize>().map_err(|_| JoltError::Parser(ParserError::NumberOverflow {
                                    number: value,
                                    target: name,
                                    src: NamedSource::new(self.filename, self.source.to_string()),
                                    at: (numeric.span.start..numeric.span.end).into(),
                                }))?))
                            },
                            "u8" => {
                                d_advance!(self);

                                Ok(Node::U8(value.parse::<u8>().map_err(|_| JoltError::Parser(ParserError::NumberOverflow {
                                    number: value,
                                    target: name,
                                    src: NamedSource::new(self.filename, self.source.to_string()),
                                    at: (numeric.span.start..numeric.span.end).into(),
                                }))?))
                            },
                            "u16" => {
                                d_advance!(self);

                                Ok(Node::U16(value.parse::<u16>().map_err(|_| JoltError::Parser(ParserError::NumberOverflow {
                                    number: value,
                                    target: name,
                                    src: NamedSource::new(self.filename, self.source.to_string()),
                                    at: (numeric.span.start..numeric.span.end).into(),
                                }))?))
                            },
                            "u32" => {
                                d_advance!(self);

                                Ok(Node::U32(value.parse::<u32>().map_err(|_| JoltError::Parser(ParserError::NumberOverflow {
                                    number: value,
                                    target: name,
                                    src: NamedSource::new(self.filename, self.source.to_string()),
                                    at: (numeric.span.start..numeric.span.end).into(),
                                }))?))
                            },
                            "u64" => {
                                d_advance!(self);

                                Ok(Node::U64(value.parse::<u64>().map_err(|_| JoltError::Parser(ParserError::NumberOverflow {
                                    number: value,
                                    target: name,
                                    src: NamedSource::new(self.filename, self.source.to_string()),
                                    at: (numeric.span.start..numeric.span.end).into(),
                                }))?))
                            },
                            "usize" => {
                                d_advance!(self);

                                Ok(Node::USize(value.parse::<usize>().map_err(|_| JoltError::Parser(ParserError::NumberOverflow {
                                    number: value,
                                    target: name,
                                    src: NamedSource::new(self.filename, self.source.to_string()),
                                    at: (numeric.span.start..numeric.span.end).into(),
                                }))?))
                            },
                            "f32" => {
                                d_advance!(self);

                                Ok(Node::F32(value.parse::<f32>().map_err(|_| JoltError::Parser(ParserError::NumberOverflow {
                                    number: value,
                                    target: name,
                                    src: NamedSource::new(self.filename, self.source.to_string()),
                                    at: (numeric.span.start..numeric.span.end).into(),
                                }))?))
                            },
                            "f64" => {
                                d_advance!(self);

                                Ok(Node::F64(value.parse::<f64>().map_err(|_| JoltError::Parser(ParserError::NumberOverflow {
                                    number: value,
                                    target: name,
                                    src: NamedSource::new(self.filename, self.source.to_string()),
                                    at: (numeric.span.start..numeric.span.end).into(),
                                }))?))
                            },
                            _ => implicit,
                        }
                    },
                    _ => panic!("Kind::Identifier should always have a String value"),
                },
                _ => implicit,
            }
        }
    }

    fn parse_string(&mut self, string: Token) -> Result<Node> {
        let value = match string.value {
            Value::String(value) => value,
            _ => panic!("Kind::StringLiteral should always have a String value"),
        };

        Ok(Node::String(value))
    }

    fn parse_attribute(&mut self) -> Result<Node> {
        let start = self.advance().expect("peek() above should rule this out");

        self.expect_help(Kind::LeftBracket, "expected opening bracket (`[`)", Some("attributes are defined as follows: `@[First]` or `@[First, Second]`"))?;

        let mut attributes = vec![];

        while let Some(next) = self.peek() {
            d_expect!(Kind::RightBracket, "none");
            if next.kind == Kind::RightBracket {
                d_advance!(self);

                if attributes.is_empty() {
                    return Err(JoltError::Parser(ParserError::EmptyAttributes {
                        src: NamedSource::new(self.filename, self.source.to_string()),
                        at: (start.span.start..next.span.end).into(),
                        label: "expected attribute".to_string(),
                        help: Some("remove the empty `@[]` if no attributes are used".to_string()),
                    }))?;
                }

                break;
            }

            attributes.push(match self.expect(Kind::Identifier, "expected attribute")?.value {
                Value::String(attribute) => attribute,
                _ => panic!("Kind::Identifier should always have a String value")
            });

            d_expect!(Kind::Comma, Kind::RightBracket);
            match self.peek() {
                Some(token) => match token.kind {
                    Kind::Comma => {
                        d_advance!(self);
                    },
                    Kind::RightBracket => {
                        d_advance!(self);
                        break;
                    },
                    _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                        token: token.kind,
                        src: NamedSource::new(self.filename, self.source.to_string()),
                        at: (token.span.start..token.span.end).into(),
                        label: "expected comma or closing bracket (`,` or `]`)".to_string(),
                        help: None,
                    }))?
                },
                None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                    src: NamedSource::new(self.filename, self.source.to_string()),
                    at: (self.source.len() - 1, 1).into(),
                    label: "expected comma or closing parentheses (`,` or `]`)".to_string(),
                    help: None,
                }))?
            }
        }

        d_expect!(Kind::StructKeyword, Kind::PubKeyword);
        match self.peek() {
            Some(token) => match token.kind {
                Kind::StructKeyword => self.parse_struct(false, attributes),
                Kind::PubKeyword => {
                    d_advance!(self);

                    d_expect!(Kind::StructKeyword);
                    match self.peek() {
                        Some(token) => match token.kind {
                            Kind::StructKeyword => self.parse_struct(false, attributes),
                            _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                                token: token.kind,
                                src: NamedSource::new(self.filename, self.source.to_string()),
                                at: (token.span.start..token.span.end).into(),
                                label: "expected struct definition".to_string(),
                                help: None,
                            }))?
                        },
                        None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                            src: NamedSource::new(self.filename, self.source.to_string()),
                            at: (self.source.len() - 1, 1).into(),
                            label: "expected struct definition".to_string(),
                            help: None,
                        }))?
                    }
                },
                _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                    token: token.kind,
                    src: NamedSource::new(self.filename, self.source.to_string()),
                    at: (token.span.start..token.span.end).into(),
                    label: "expected struct definition".to_string(),
                    help: None,
                }))?
            },
            None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                src: NamedSource::new(self.filename, self.source.to_string()),
                at: (self.source.len() - 1, 1).into(),
                label: "expected struct definition".to_string(),
                help: None,
            }))?
        }
    }

    fn parse_struct(&mut self, public: bool, attributes: Vec<String>) -> Result<Node> {
        d_advance!(self);

        let name = match self.expect(Kind::Identifier, "expected struct name")?.value {
            Value::String(name) => name,
            _ => panic!("Kind::Identifier should always have a String value"),
        };

        let mut implements = vec![];

        d_expect!(Kind::LeftBrace, Kind::Colon);
        match self.peek() {
            Some(token) => match token.kind {
                Kind::LeftBrace => {
                    d_advance!(self);
                },
                Kind::Colon => {
                    d_advance!(self);

                    while let Some(next) = self.peek() {
                        d_expect!(Kind::LeftBrace, "none");
                        if next.kind == Kind::LeftBrace {
                            d_advance!(self);
                            break;
                        }

                        implements.push(match self.expect(Kind::Identifier, "expected trait")?.value {
                            Value::String(name) => name,
                            _ => panic!("Kind::Identifier should always have a String value"),
                        });

                        d_expect!(Kind::Comma, Kind::LeftBrace);
                        match self.peek() {
                            Some(token) => match token.kind {
                                Kind::Comma => {
                                    d_advance!(self);
                                },
                                Kind::LeftBrace => {
                                    d_advance!(self);
                                    break;
                                },
                                _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                                    token: token.kind,
                                    src: NamedSource::new(self.filename, self.source.to_string()),
                                    at: (token.span.start..token.span.end).into(),
                                    label: "expected comma or opening brace (`,` or `{`)".to_string(),
                                    help: None,
                                }))?
                            },
                            None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                                src: NamedSource::new(self.filename, self.source.to_string()),
                                at: (self.source.len() - 1, 1).into(),
                                label: "expected comma or opening brace (`,` or `{`)".to_string(),
                                help: None,
                            }))?
                        }
                    }
                },
                _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                    token: token.kind,
                    src: NamedSource::new(self.filename, self.source.to_string()),
                    at: (token.span.start..token.span.end).into(),
                    label: "expected colon or opening brace (`:` or `{`)".to_string(),
                    help: None,
                }))?
            },
            None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                src: NamedSource::new(self.filename, self.source.to_string()),
                at: (self.source.len() - 1, 1).into(),
                label: "expected colon or opening brace (`:` or `{`)".to_string(),
                help: None,
            }))?
        }

        let mut fields = vec![];
        let mut methods = vec![];

        while let Some(token) = self.peek() {
            d_expect!(Kind::RightBrace, Kind::Identifier, Kind::MutKeyword, Kind::WeakKeyword, Kind::UnownedKeyword, Kind::FunKeyword, Kind::PubKeyword);
            match token.kind {
                Kind::RightBrace => {
                    break;
                },
                Kind::Identifier | Kind::MutKeyword | Kind::WeakKeyword | Kind::UnownedKeyword => {
                    fields.push(self.parse_field(false)?);
                },
                Kind::FunKeyword => {
                    methods.push(self.parse_function(false)?);
                },
                Kind::PubKeyword => {
                    d_advance!(self);

                    d_expect!(Kind::Identifier, Kind::MutKeyword, Kind::WeakKeyword, Kind::UnownedKeyword, Kind::FunKeyword);
                    match self.peek() {
                        Some(token) => match token.kind {
                            Kind::Identifier | Kind::MutKeyword | Kind::WeakKeyword | Kind::UnownedKeyword => {
                                fields.push(self.parse_field(true)?);
                            },
                            Kind::FunKeyword => {
                                methods.push(self.parse_function(true)?);
                            },
                            _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                                token: token.kind,
                                src: NamedSource::new(self.filename, self.source.to_string()),
                                at: (token.span.start..token.span.end).into(),
                                label: "expected field or method".to_string(),
                                help: None,
                            }))?
                        },
                        None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                            src: NamedSource::new(self.filename, self.source.to_string()),
                            at: (self.source.len() - 1, 1).into(),
                            label: "expected field or method".to_string(),
                            help: None,
                        }))?
                    }
                },
                _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                    token: token.kind,
                    src: NamedSource::new(self.filename, self.source.to_string()),
                    at: (token.span.start..token.span.end).into(),
                    label: "expected field, method or closing brace (`}`)".to_string(),
                    help: None,
                }))?
            }
        }

        match self.advance() {
            Some(_) => Ok(Node::Struct {
                public,
                attributes,
                implements,
                name,
                fields,
                methods,
            }),
            None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                src: NamedSource::new(self.filename, self.source.to_string()),
                at: (self.source.len() - 1, 1).into(),
                label: "expected field, method or closing brace (`}`)".to_string(),
                help: None,
            }))?
        }
    }

    fn parse_field(&mut self, public: bool) -> Result<Node> {
        let name;
        let type_;
        let mutable;
        let weak;
        let unowned;

        d_expect!(Kind::Identifier, Kind::MutKeyword, Kind::WeakKeyword, Kind::UnownedKeyword);
        match self.peek() {
            Some(token) => match token.kind {
                Kind::Identifier => {
                    d_advance!(self);

                    weak = false;
                    unowned = false;
                    mutable = false;

                    name = match token.value {
                        Value::String(name) => name,
                        _ => panic!("Kind::Identifier should always have a String value"),
                    };

                    self.expect(Kind::Colon, "expected colon (`:`)")?;

                    type_ = parse_path!(self)?;
                },
                Kind::MutKeyword => {
                    d_advance!(self);

                    weak = false;
                    unowned = false;
                    mutable = true;

                    name = match self.expect(Kind::Identifier, "expected field name")?.value {
                        Value::String(name) => name,
                        _ => panic!("Kind::Identifier should always have a String value"),
                    };

                    self.expect(Kind::Colon, "expected colon (`:`)")?;

                    type_ = parse_path!(self)?;
                },
                Kind::WeakKeyword => {
                    d_advance!(self);

                    weak = true;
                    unowned = false;

                    d_expect!(Kind::MutKeyword, Kind::Identifier);
                    match self.peek() {
                        Some(token) => match token.kind {
                            Kind::MutKeyword => {
                                d_advance!(self);

                                mutable = true;
                            },
                            Kind::Identifier => {
                                mutable = false;
                            },
                            _ => return Err(JoltError::Parser(ParserError::UnexpectedToken {
                                token: token.kind,
                                src: NamedSource::new(self.filename, self.source.to_string()),
                                at: (token.span.start..token.span.end).into(),
                                label: "expected `mut` keyword or field name".to_string(),
                                help: None,
                            }))?
                        },
                        None => return Err(JoltError::Parser(ParserError::TerminatedStatement {
                            src: NamedSource::new(self.filename, self.source.to_string()),
                            at: (self.source.len() - 1, 1).into(),
                            label: "expected `mut` keyword or field name".to_string(),
                            help: None,
                        }))?
                    }

                    name = match self.expect(Kind::Identifier, "expected field name")?.value {
                        Value::String(name) => name,
                        _ => panic!("Kind::Identifier should always have a String value"),
                    };

                    self.expect(Kind::Colon, "expected colon (`:`)")?;

                    type_ = parse_path!(self)?;
                },
                Kind::UnownedKeyword => {
                    d_advance!(self);

                    weak = false;
                    unowned = true;

                    d_expect!(Kind::MutKeyword, Kind::Identifier);
                    match self.peek() {
                        Some(token) => match token.kind {
                            Kind::MutKeyword => {
                                d_advance!(self);

                                mutable = true;
                            },
                            Kind::Identifier => {
                                mutable = false;
                            },
                            _ => return Err(JoltError::Parser(ParserError::UnexpectedToken {
                                token: token.kind,
                                src: NamedSource::new(self.filename, self.source.to_string()),
                                at: (token.span.start..token.span.end).into(),
                                label: "expected `mut` keyword or field name".to_string(),
                                help: None,
                            }))?
                        },
                        None => return Err(JoltError::Parser(ParserError::TerminatedStatement {
                            src: NamedSource::new(self.filename, self.source.to_string()),
                            at: (self.source.len() - 1, 1).into(),
                            label: "expected `mut` keyword or field name".to_string(),
                            help: None,
                        }))?
                    }

                    name = match self.expect(Kind::Identifier, "expected field name")?.value {
                        Value::String(name) => name,
                        _ => panic!("Kind::Identifier should always have a String value"),
                    };

                    self.expect(Kind::Colon, "expected colon (`:`)")?;

                    type_ = parse_path!(self)?;
                },
                _ => return Err(JoltError::Parser(ParserError::UnexpectedToken {
                    token: token.kind,
                    src: NamedSource::new(self.filename, self.source.to_string()),
                    at: (token.span.start..token.span.end).into(),
                    label: "expected `weak` keyword, `unowned` keyword, `mut` keyword or field name".to_string(),
                    help: None,
                }))?
            },
            None => return Err(JoltError::Parser(ParserError::TerminatedStatement {
                src: NamedSource::new(self.filename, self.source.to_string()),
                at: (self.source.len() - 1, 1).into(),
                label: "expected `weak` keyword, `unowned` keyword, `mut` keyword or field name".to_string(),
                help: None,
            }))?
        }

        self.expect_semi()?;

        Ok(Node::StructField {
            name,
            type_,
            public,
            mutable,
            weak,
            unowned,
        })
    }

    fn parse_trait(&mut self, public: bool) -> Result<Node> {
        d_advance!(self);

        let name = match self.expect(Kind::Identifier, "expected trait name")?.value {
            Value::String(name) => name,
            _ => panic!("Kind::Identifier should always have a String value"),
        };

        let mut implements = vec![];

        d_expect!(Kind::LeftBrace, Kind::Colon);
        match self.peek() {
            Some(token) => match token.kind {
                Kind::LeftBrace => {
                    d_advance!(self);
                },
                Kind::Colon => {
                    d_advance!(self);

                    while let Some(next) = self.peek() {
                        d_expect!(Kind::LeftBrace, "none");
                        if next.kind == Kind::LeftBrace {
                            d_advance!(self);
                            break;
                        }

                        implements.push(match self.expect(Kind::Identifier, "expected trait")?.value {
                            Value::String(name) => name,
                            _ => panic!("Kind::Identifier should always have a String value"),
                        });

                        d_expect!(Kind::Comma, Kind::LeftBrace);
                        match self.peek() {
                            Some(token) => match token.kind {
                                Kind::Comma => {
                                    d_advance!(self);
                                },
                                Kind::LeftBrace => {
                                    d_advance!(self);
                                    break;
                                },
                                _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                                    token: token.kind,
                                    src: NamedSource::new(self.filename, self.source.to_string()),
                                    at: (token.span.start..token.span.end).into(),
                                    label: "expected comma or opening brace (`,` or `{`)".to_string(),
                                    help: None,
                                }))?
                            },
                            None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                                src: NamedSource::new(self.filename, self.source.to_string()),
                                at: (self.source.len() - 1, 1).into(),
                                label: "expected comma or opening brace (`,` or `{`)".to_string(),
                                help: None,
                            }))?
                        }
                    }
                },
                _ => return Err(JoltError::Parser(ParserError::UnexpectedToken {
                    token: token.kind,
                    src: NamedSource::new(self.filename, self.source.to_string()),
                    at: (token.span.start..token.span.end).into(),
                    label: "".to_string(),
                    help: None,
                }))?
            },
            None => return Err(JoltError::Parser(ParserError::TerminatedStatement {
                src: NamedSource::new(self.filename, self.source.to_string()),
                at: (self.source.len() - 1, 1).into(),
                label: "expected colon or opening brace (`:` or `{`)".to_string(),
                help: None,
            }))?
        }

        let mut methods = vec![];

        while let Some(token) = self.peek() {
            d_expect!(Kind::RightBrace, Kind::FunKeyword, Kind::PubKeyword);
            match token.kind {
                Kind::RightBrace => {
                    break;
                },
                Kind::FunKeyword => {
                    methods.push(self.parse_function(false)?);
                },
                Kind::PubKeyword => {
                    d_advance!(self);

                    d_expect!(Kind::FunKeyword);
                    match self.peek() {
                        Some(token) => match token.kind {
                            Kind::FunKeyword => {
                                methods.push(self.parse_function(true)?);
                            },
                            _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                                token: token.kind,
                                src: NamedSource::new(self.filename, self.source.to_string()),
                                at: (token.span.start..token.span.end).into(),
                                label: "expected method".to_string(),
                                help: None,
                            }))?
                        },
                        None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                            src: NamedSource::new(self.filename, self.source.to_string()),
                            at: (self.source.len() - 1, 1).into(),
                            label: "expected method".to_string(),
                            help: None,
                        }))?
                    }
                },
                _ => Err(JoltError::Parser(ParserError::UnexpectedToken {
                    token: token.kind,
                    src: NamedSource::new(self.filename, self.source.to_string()),
                    at: (token.span.start..token.span.end).into(),
                    label: "expected method or closing brace (`}`)".to_string(),
                    help: None,
                }))?
            }
        }

        match self.advance() {
            Some(_) => Ok(Node::Trait {
                public,
                name,
                implements,
                methods,
            }),
            None => Err(JoltError::Parser(ParserError::TerminatedStatement {
                src: NamedSource::new(self.filename, self.source.to_string()),
                at: (self.source.len() - 1, 1).into(),
                label: "expected method or closing brace (`}`)".to_string(),
                help: None,
            }))?
        }
    }
}
