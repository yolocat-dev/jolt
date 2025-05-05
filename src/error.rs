use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::lexer::token::Kind;

#[derive(Debug, Clone, PartialEq, Error, Diagnostic)]
pub enum JoltError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Lexer(#[from] LexerError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    Parser(#[from] ParserError),
}

#[derive(Debug, Clone, PartialEq, Error, Diagnostic)]
pub enum LexerError {
    #[error("Unexpected character '{character}'")]
    #[diagnostic(code(jolt::lexer::unexpected_character))]
    UnexpectedCharacter {
        character: char,
        #[source_code]
        src: NamedSource<String>,
        #[label(primary, "here")]
        at: SourceSpan,
    },
    #[error("Undefined escape sequence '\\{character}'")]
    #[diagnostic(code(jolt::lexer::undefined_escape_sequence), help("try ecaping the `\\` with `\\\\`"))]
    UndefinedEscapeSequence {
        character: char,
        #[source_code]
        src: NamedSource<String>,
        #[label(primary, "undefined escape sequence '\\{character}'")]
        at: SourceSpan,
    },
    #[error("Terminated numeric, can't end with '{character}'")]
    #[diagnostic(code(jolt::lexer::terminated_numeric))]
    TerminatedNumeric {
        character: char,
        #[source_code]
        src: NamedSource<String>,
        #[label(primary, "terminated numeric, can't end with '{character}'")]
        at: SourceSpan,
        #[help]
        help: Option<String>,
    },
    #[error("Numerics can't contain two decimal points")]
    #[diagnostic(code(jolt::lexer::numeric_double_decimal), help("try removing the second `.`"))]
    NumericDoubleDecimal {
        #[source_code]
        src: NamedSource<String>,
        #[label(primary, "numerics can't contain two decimal points")]
        at: SourceSpan,
    },
}

#[derive(Debug, Clone, PartialEq, Error, Diagnostic)]
pub enum ParserError {
    #[error("Unexpected token '{token:#?}'")]
    #[diagnostic(code(jolt::parser::unexpected_token))]
    UnexpectedToken {
        token: Kind,
        #[source_code]
        src: NamedSource<String>,
        #[label(primary, "{label}")]
        at: SourceSpan,
        label: String,
        #[help]
        help: Option<String>,
    },
    #[error("Terminated statement")]
    #[diagnostic(code(jolt::parser::terminated_statement))]
    TerminatedStatement {
        #[source_code]
        src: NamedSource<String>,
        #[label(primary, "{label}")]
        at: SourceSpan,
        label: String,
        #[help]
        help: Option<String>,
    },
    #[error("Number overflow: '{number}' does not fit in the target type ({target})")]
    #[diagnostic(code(jolt::parser::number_overflow), help("try using a larger type"))]
    // TODO: this currently appears when '1.23' is parsed as an integer for example, which should
    //       output a different error like "unexpected decimal in integer"
    NumberOverflow {
        number: String,
        target: String,
        #[source_code]
        src: NamedSource<String>,
        #[label(primary, "does not fit in the target type ({target})")]
        at: SourceSpan,
    },
    #[error("Empty attributes")]
    #[diagnostic(code(jolt::parser::empty_attributes))]
    EmptyAttributes {
        #[source_code]
        src: NamedSource<String>,
        #[label(primary, "{label}")]
        at: SourceSpan,
        label: String,
        #[help]
        help: Option<String>,
    },
}

pub type Result<T> = miette::Result<T>;
