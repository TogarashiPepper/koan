use std::{io::stdout, path::PathBuf, process::exit};

use koan::{
    error::{handle_err, CliError, KoanError, Result},
    interpreter::IntrpCtx,
    lexer::lex,
    parser::parse,
    state::State,
    compiler::compile,
};

#[cfg(feature = "repl")]
use koan::repl::repl;

fn main() {
    let garbage = lex("1 + 2").and_then(parse).unwrap();
    compile(garbage.0[0].clone(), garbage.1);

    // let mut arg_it = std::env::args();
    // arg_it.next();
    // let arg = arg_it.next().unwrap_or_else(|| "repl".to_owned());
    //
    // if arg == "repl" {
    //     #[cfg(feature = "repl")]
    //     if let Err(err) = repl() {
    //         eprintln!("{}", handle_err(err));
    //         exit(1);
    //     }
    // } else {
    //     let path: PathBuf = arg.into();
    //     if let Err(err) = run_file(path) {
    //         eprintln!("{}", handle_err(err));
    //         exit(1);
    //     }
    // }
}

fn run_file(path: PathBuf) -> Result<()> {
    let file = std::fs::read_to_string(path)
        .map_err(|err| KoanError::from(CliError::FileError(err.kind())))?;
    let mut state = State::new();

    let tokens = lex(&file)?;
    let (ast, pool) = parse(tokens)?;
    let mut ctx = IntrpCtx {
        writer: stdout().lock(),
        state: &mut state,
        pool: &pool,
    };

    for statement in ast {
        let _ = ctx.eval_ast(statement)?;
    }

    Ok(())
}
