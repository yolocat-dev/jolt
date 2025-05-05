use std::env;

use error::Result;
use lexer::{token::Kind, Lexer};
use parser::Parser;

pub mod error;
pub mod lexer;
pub mod parser;

#[cfg(test)]
mod tests;

fn main() -> Result<()> {
    let args = env::args().collect::<Vec<_>>();

    let command = if args.len() > 1 {
        &args[1]
    } else {
        "parse"
    };

    match command {
        "lex" => {
            lex()
        },
        _ => {
            parse()
        }
    }
}

fn lex() -> Result<()> {
    let source = include_str!("../test.jt");

    let lexer = Lexer::new("test.jt", source);
    let tokens = lexer.lex()?;

    println!("{:#?}", tokens);

    Ok(())
}

fn parse() -> Result<()> {
    let source = include_str!("../test.jt");

    let lexer = Lexer::new("test.jt", source);
    let tokens = lexer.lex()?;

    let parser = Parser::new(tokens.into_iter().filter(|t| t.kind != Kind::Comment), "test.jt", source);
    let file = parser.parse()?;

    println!("{:#?}", file);

    Ok(())
}
