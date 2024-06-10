mod lexer;
mod parser;

use lexer::{lex, LexError};

use crate::parser::parse;

fn main() {
    let input = "1 + 2 - 3";
    let tokens = match lex(input) {
        Ok(list) => list,
        Err(error) => match error {
            LexError::InvalidToken(offending_token) => {
                eprintln!("The token `{}` is invalid", offending_token);
                std::process::exit(1)
            }
            LexError::PartialMultiCharToken(offending_token, expected_token) => {
                eprintln!(
                    "The token `{}` is partial of `{}`",
                    offending_token, expected_token
                );
                std::process::exit(1)
            }
        },
    };
    let ast = parse(tokens).unwrap();

    println!("{:?}", ast);
}
