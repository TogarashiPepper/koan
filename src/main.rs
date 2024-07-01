mod error;
mod interpreter;
mod lexer;
mod parser;
mod state;
mod value;

use std::{path::PathBuf, process::exit};

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use crate::{
    error::{CliError, InterpreterError, KoanError, KoanErrorType, LexError, ParseError},
    lexer::lex,
    parser::parse,
    state::State,
    value::Value,
};

pub type Result<T> = std::result::Result<T, KoanError>;

fn main() {
    let mut arg_it = std::env::args();
    arg_it.next();
    let arg = arg_it.next().unwrap_or_else(|| "repl".to_owned());

    if arg == "repl" {
        if let Err(err) = repl() {
            handle_err(err)
        }
    } else {
        let path: PathBuf = arg.into();
        if let Err(err) = run_file(path) {
            handle_err(err)
        }
    }
}

// TODO: make this a display impl for KoanError
fn handle_err(err: KoanError) -> ! {
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
            ParseError::Unexpected(unexpected) => {
                format!("Unexpected `{unexpected:?}` token found")
            }
        },
        KoanErrorType::InterpErr(ierr) => match ierr {
            InterpreterError::MismatchedTypes(op, l, r) => {
                format!("Cannot apply operator `{op:?}` to types `{l}` and `{r}`")
            }
            InterpreterError::DivByZero => "Attempted to divide by zero".to_owned(),
            InterpreterError::UndefVar(varname) => {
                format!("Variable `{varname}` is undefined")
            }
            InterpreterError::MismatchedUnOp(op, ty) => {
                format!("Cannot apply unary operator `{op:?}` for type `{ty}`")
            }
            InterpreterError::UndefFunc(fnname) => format!("Function `{fnname}` is undefined"),
            InterpreterError::MismatchedArity(name, got, expected) => {
                format!("Function `{name}` got {got} arguments but expected {expected}")
            }
        },
        KoanErrorType::CliErr(cerr) => match cerr {
            CliError::FileError(ioerr) => {
                format!("Got error kind `{ioerr}` when trying to read file")
            }
        },
    };

    eprintln!("{err_string}");
    exit(1);
}

fn repl() -> Result<()> {
    let mut state = State::new();
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

fn run_file(path: PathBuf) -> Result<()> {
    let file = std::fs::read_to_string(path)
        .map_err(|err| KoanError::from(CliError::FileError(err.kind())))?;
    let mut state = State::new();

    let tokens = lex(&file)?;
    let ast = parse(tokens)?;

    for statement in ast {
        let _ = statement.eval(&mut state)?;
    }

    Ok(())
}
