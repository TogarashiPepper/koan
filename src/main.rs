mod lexer;
mod parser;

use lexer::{lex, LexError};

fn main() {
    let input = "○";
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

    println!("{:?}", tokens);
}
