use criterion::{
    black_box, criterion_group, criterion_main, BatchSize, Criterion,
};
use koan::{lexer::lex, parser::parse};

fn criterion_benchmark(c: &mut Criterion) {
    let toks = lex(include_str!("../foo.koan")).unwrap();
    let ast = parse(toks.clone()).unwrap();

    c.bench_function("ast", |b| {
        b.iter_batched(
            || toks.clone(),
            |tkns| black_box(parse(tkns)),
            BatchSize::SmallInput,
        )
    });

    c.bench_function("tree-walk interpreter", |b| {
        b.iter_batched(|| ast.clone(), routine, size)
    })
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
