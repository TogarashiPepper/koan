mod error;
mod interpreter;
mod lexer;
mod parser;
mod value;

use std::collections::HashMap;

use crate::{
    interpreter::State,
    lexer::lex,
    parser::parse,
    value::Value
};

fn main() {
    let input = "let x = 1; let y = 41; x + y";
    let tokens = match lex(input) {
        Ok(list) => list,
        Err(err) => {
            eprintln!("{:?}", err);
            std::process::exit(1)
        }
    };
    let ast = parse(tokens).unwrap();

    let mut state = State {
        variables: HashMap::new(),
    };

    for statement in ast {
        let r = statement.eval(&mut state).unwrap();
        if r != Value::Nothing {
            println!("{}", r);
        }
    }
}
