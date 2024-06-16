mod error;
mod lexer;
mod parser;
mod interpreter;

use crate::{lexer::lex, parser::parse};

fn main() {
    // let input = "1 + 2 - 3 * 4 / 5 - 0.6 + ○1";
    let input = "0.1 + 0.2 == 0.3";
    let tokens = match lex(input) {
        Ok(list) => list,
        Err(err) => {
            eprintln!("{:?}", err);
            std::process::exit(1)
        }
    };
    let ast = parse(tokens).unwrap();

    let res = match ast {
        parser::Ast::Expression(e) => e.eval(),
    };

    println!("{:?}", res);
}
