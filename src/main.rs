mod error;
mod interpreter;
mod lexer;
mod parser;
mod state;
mod value;

use std::{io::stdout, path::PathBuf, process::exit};

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use crate::{
    error::{handle_err, CliError, KoanError, Result},
    lexer::lex,
    parser::parse,
    state::State,
    value::Value,
};

fn main() {
    let mut arg_it = std::env::args();
    arg_it.next();
    let arg = arg_it.next().unwrap_or_else(|| "repl".to_owned());

    if arg == "repl" {
        if let Err(err) = repl() {
            handle_err(err);
            exit(1);
        }
    } else {
        let path: PathBuf = arg.into();
        if let Err(err) = run_file(path) {
            handle_err(err);
            exit(1);
        }
    }
}

fn repl() -> Result<()> {
    let mut state = State::new();
    let mut rl = DefaultEditor::new().unwrap();
    let mut stdout = stdout().lock();

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
            let r = statement.eval(&mut state, &mut stdout)?;
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
    let mut stdout = stdout().lock();

    let tokens = lex(&file)?;
    let ast = parse(tokens)?;

    for statement in ast {
        let _ = statement.eval(&mut state, &mut stdout)?;
    }

    Ok(())
}
