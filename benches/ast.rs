use std::rc::Rc;

use criterion::{BatchSize, Criterion, black_box, criterion_group, criterion_main};
use koan::{
    compiler::Compiler,
    interpreter::IntrpCtx,
    lexer::lex,
    parser::{Ast, parse},
    pool::ExprPool,
    state::State,
    value::Value,
    vm::{OpCode, VM},
};
use rand::Rng;

fn twalk_initrun((ast, mut state): ((Vec<Ast>, ExprPool), State)) {
    let mut ctx = IntrpCtx {
        writer: vec![],
        state: &mut state,
        pool: &ast.1,
    };

    for s in ast.0 {
        black_box(ctx.eval_ast(s).unwrap());
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    let toks = lex(include_str!("../foo.koan")).unwrap();
    let inp = include_str!("lexerdata.txt");

    c.bench_function("ast", |b| {
        b.iter_batched(
            || toks.clone(),
            |tkns| black_box(parse(tkns)),
            BatchSize::SmallInput,
        )
    });

    c.bench_function("lexer", |b| {
        b.iter(|| {
            lex(black_box(inp)).unwrap();
        });
    });

    c.bench_function("VM 5000x array mul", |b| {
        b.iter_batched(
            || {
                let mut vm = VM::new();
                let mut rng = rand::rng();

                let mut a1: Vec<u32> = vec![];
                let mut a2: Vec<u32> = vec![];
                for _ in 0..5000 {
                    a1.push(rng.random());
                    a2.push(rng.random());
                }

                vm.stack.push(Value::Array(Rc::new(
                    a1.into_iter().map(|n| Value::Num(n as f64)).collect(),
                )));

                vm.stack.push(Value::Array(Rc::new(
                    a2.into_iter().map(|n| Value::Num(n as f64)).collect(),
                )));

                vm.chunk.push(OpCode::Mul as u8);

                vm
            },
            |mut vm| {
                vm.run().unwrap();
            },
            BatchSize::SmallInput,
        );
    });

    c.bench_function("tree-walk interpreter 1000x array mul", |b| {
        b.iter_batched(
            || {
                (
                    lex(include_str!("thousandmul.koan"))
                        .and_then(parse)
                        .unwrap(),
                    State::new(),
                )
            },
            twalk_initrun,
            BatchSize::SmallInput,
        );
    });

    c.bench_function("tree-walk interpreter fib.koan", |b| {
        b.iter_batched(
            || {
                (
                    lex(include_str!("../fib.koan")).and_then(parse).unwrap(),
                    State::new(),
                )
            },
            twalk_initrun,
            BatchSize::SmallInput,
        )
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
