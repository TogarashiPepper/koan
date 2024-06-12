mod error;
mod lexer;
mod parser;
mod interpreter;

use crate::{lexer::lex, parser::parse};

fn main() {
    let input = "1 + 2 - 3";
    let tokens = match lex(input) {
        Ok(list) => list,
        Err(err) => {
            eprintln!("{:?}", err);
            std::process::exit(1)
        }
    };
    let ast = parse(tokens).unwrap();

    println!("{:?}", ast);
}
