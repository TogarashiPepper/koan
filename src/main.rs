use std::{io::stdout, path::PathBuf, process::exit};

use koan::{
    compiler::Compiler, error::{handle_err, CliError, KoanError, Result}, interpreter::IntrpCtx, lexer::lex, parser::parse, state::State
};

#[cfg(feature = "repl")]
use koan::repl::repl;

// fn main() {
//     let mut arg_it = std::env::args();
//     arg_it.next();
//     let arg = arg_it.next().unwrap_or_else(|| "repl".to_owned());
//
//     if arg == "repl" {
//         #[cfg(feature = "repl")]
//         if let Err(err) = repl() {
//             eprintln!("{}", handle_err(err));
//             exit(1);
//         }
//     } else {
//         let path: PathBuf = arg.into();
//         if let Err(err) = run_file(path) {
//             eprintln!("{}", handle_err(err));
//             exit(1);
//         }
//     }
// }

fn main() {
    let prg = "let x = 1; { let y = 2; { let z = 3; let q = 4; x + y + z + q } }";
    let prg = lex(prg).and_then(parse).unwrap();

    let mut compiler = Compiler::default();
    for stmt in prg.0 {
        compiler.compile(stmt, &prg.1).unwrap();
    }
    
    let mut vm = compiler.finish();

    println!("{:#?}", vm.run());
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
