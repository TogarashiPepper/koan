use criterion::{
    black_box, criterion_group, criterion_main, BatchSize, Criterion,
};
use koan::{lexer::lex, parser::parse};

fn criterion_benchmark(c: &mut Criterion) {
    let toks = lex(r#"
        let thisisalongident = 2 + 3 * π / 3 + e * (2 / 3 + 1 ^ ○2222) - ○2;
        print(|thisisalongident * -1|);

        1 + thisisalongident
        "#)
    .unwrap();

    c.bench_function("ast", |b| {
        b.iter_batched(
            || toks.clone(),
            |tkns| black_box(parse(tkns)),
            BatchSize::SmallInput,
        )
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
