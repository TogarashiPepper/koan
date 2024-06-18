mod error;
mod interpreter;
mod lexer;
mod parser;
mod value;

use std::{collections::HashMap, process::exit};

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use crate::{
    error::{KoanError, KoanErrorType, LexError, ParseError},
    interpreter::State,
    lexer::lex,
    parser::parse,
    value::Value,
};

fn main() {
    if let Err(err) = repl() {
        let err_string = match err.0 {
            KoanErrorType::LexErr(lerr) => match lerr {
                LexError::PartialMultiCharToken(f, s) => {
                    format!("Expected token `{s}` after `{f}`")
                }
                LexError::InvalidToken(tok) => format!("Character `{tok}` is not a valid token"),
            },
            KoanErrorType::ParseErr(perr) => match perr {
                ParseError::ExpectedLiteral(ty) => format!("Expected literal of type `{ty}`"),
                ParseError::ExpectedInfixOp => "Expected infix operator".to_owned(),
                ParseError::ExpectedFoundEof(expected) => {
                    format!("Expected `{expected:?}`, found EOF")
                }
                ParseError::ExpectedFound(e, f) => format!("Expected `{e:?}`, found `{f:?}`"),
            },
            KoanErrorType::InterpErr(ierr) => match ierr {
                error::InterpreterError::MismatchedTypes(op, l, r) => {
                    format!("Cannot apply operator `{op:?}` to types `{l}` and `{r}`")
                }
                error::InterpreterError::DivByZero => "Attempted to divide by zero".to_owned(),
                error::InterpreterError::UndefVar(varname) => {
                    format!("Variable `{varname}` is undefined")
                }
            },
        };

        eprintln!("{err_string}");
        exit(1);
    }
}

fn repl() -> Result<(), KoanError> {
    let mut state = State {
        variables: HashMap::new(),
    };
    let mut rl = DefaultEditor::new().unwrap();

    loop {
        let line = match rl.readline("λ ") {
            Ok(line) => line,
            Err(ReadlineError::Interrupted) => {
                eprintln!("REPL ended by CTRL-C");
                exit(1);
            }
            Err(ReadlineError::Eof) => {
                eprintln!("REPL ended by CTRL-D");
                exit(1);
            }
            Err(err) => {
                eprintln!("REPL ended with error: {err:?}");
                exit(1);
            }
        };

        let tokens = lex(&line)?;
        let ast = parse(tokens)?;

        for statement in ast {
            let r = statement.eval(&mut state)?;
            if r != Value::Nothing {
                println!("{}", r);
            }
        }
    }
}
