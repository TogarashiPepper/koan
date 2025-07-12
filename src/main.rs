use std::{io::stdout, path::PathBuf, process::exit};

use koan::{
    compiler::Compiler,
    error::{CliError, KoanError, Result, handle_err},
    interpreter::IntrpCtx,
    lexer::lex,
    parser::parse,
    state::State,
};

#[cfg(feature = "repl")]
use koan::repl::repl;

fn main() {
    let mut arg_it = std::env::args();
    arg_it.next();
    let arg = arg_it.next().unwrap_or_else(|| "repl".to_owned());

    if arg == "repl" {
        #[cfg(feature = "repl")]
        if let Err(err) = repl() {
            eprintln!("{}", handle_err(err));
            exit(1);
        }
    } else {
        let path: PathBuf = arg.into();
        if let Err(err) = run_file(path) {
            eprintln!("{}", handle_err(err));
            exit(1);
        }
    }
}

// fn main() {
// let prg = "let x = 1; let n = x + 2 + x; { let y = \"Hello, \"; { let z = 3; let q = 4; print(x * y * z * q) } } print(n);";
// TODO: fix this, reports 5 when should print 7
// value returned from block 2 erroneously left on stack
// idea: insert dummy value into local array with a sentinel depth
// con: would have to violate vec being sorted by depth
// let prg = "let x = 1; { let n = 2; n } { let y = 3; let q = 4; print(y + q) }";
//     let prg = lex(prg).and_then(parse).unwrap();
//
//     let mut compiler = Compiler::default();
//     for stmt in prg.0 {
//         compiler.compile(stmt, &prg.1).unwrap();
//     }
//
//     let mut vm = compiler.finish();
//
//     vm.run().unwrap();
// }

fn run_file(path: PathBuf) -> Result<()> {
    let file = std::fs::read_to_string(path)
        .map_err(|err| KoanError::from(CliError::FileError(err.kind())))?;
    // let mut state = State::new();
    //
    // let tokens = lex(&file)?;
    // let (ast, pool) = parse(tokens)?;
    // let mut ctx = IntrpCtx {
    //     writer: stdout().lock(),
    //     state: &mut state,
    //     pool: &pool,
    // };
    //
    // for statement in ast {
    //     let _ = ctx.eval_ast(statement)?;
    // }

    let (ast, pool) = lex(&file).and_then(parse)?;

    let mut compiler = Compiler::new();
    for statement in ast {
        compiler.compile(statement, &pool).unwrap();
    }
    let mut vm = compiler.finish();
    // vm.dbg_chunk();

    vm.run()?;
    // println!("{:?}", vm.stack);

    // if let Some(v) = vm.stack.pop() {
    //     println!("{v}");
    // }

    Ok(())
}
