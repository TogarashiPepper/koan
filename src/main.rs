mod lexer;

use lexer::{lex, LexError};

fn main() {
    let input = "○";
    let tokens = match lex(input) {
        Ok(list) => list,
        Err(error) => match error {
            LexError::InvalidToken(offending_token) => {
                eprintln!("The token `{}` is invalid", offending_token);
                std::process::exit(1)
            },
        }
    };

    println!("{:?}", tokens);
}
