use std::{
    borrow::Cow::{self, Borrowed, Owned},
    fmt::Write,
    io::stdout,
};

use rustyline::{
    error::ReadlineError, highlight::Highlighter,
    validate::MatchingBracketValidator, Completer, Config, Editor, Helper,
    Hinter, Validator,
};
use syntect::{
    easy::HighlightLines,
    highlighting::ThemeSet,
    parsing::{SyntaxDefinition, SyntaxSet, SyntaxSetBuilder},
};

use crate::{
    error::Result, interpreter::IntrpCtx, lexer::lex, parser::parse_with_pool,
    pool::ExprPool, state::State, value::Value,
};

#[derive(Helper, Completer, Hinter, Validator)]
struct MyHelper {
    syn_set: SyntaxSet,
    #[rustyline(Validator)]
    validator: MatchingBracketValidator,
    colored_prompt: String,
}

impl Highlighter for MyHelper {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> Cow<'b, str> {
        if default {
            Borrowed(&self.colored_prompt)
        } else {
            Borrowed(prompt)
        }
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Owned("\x1b[1m".to_owned() + hint + "\x1b[m")
    }

    fn highlight<'l>(&self, line: &'l str, _: usize) -> Cow<'l, str> {
        let ts = ThemeSet::load_defaults();
        let theme = ts.themes["base16-mocha.dark"].clone();

        let mut highlighter = HighlightLines::new(
            self.syn_set.find_syntax_by_extension("koan").unwrap(),
            &theme,
        );

        let highlighted = highlighter
            .highlight_line(line, &self.syn_set)
            .unwrap()
            .into_iter()
            .fold(String::new(), |mut acc, (style, text)| {
                let _ = write!(
                    acc,
                    "\x1b[38;2;{};{};{}m{}\x1b[0m",
                    style.foreground.r,
                    style.foreground.g,
                    style.foreground.b,
                    text
                );

                acc
            });

        Cow::Owned(highlighted)
    }

    fn highlight_char(&self, _: &str, _: usize, _: bool) -> bool {
        true
    }
}

pub fn repl() -> Result<()> {
    let config = Config::builder().build();
    let syn_def = SyntaxDefinition::load_from_str(
        include_str!("../syntax/koan.sublime-syntax"),
        true,
        Some("koan"),
    )
    .unwrap();

    let mut builder = SyntaxSetBuilder::new();
    builder.add(syn_def);

    let h = MyHelper {
        syn_set: builder.build(),
        colored_prompt: "".to_owned(),
        validator: MatchingBracketValidator::new(),
    };
    let mut rl = Editor::with_config(config).unwrap();
    rl.set_helper(Some(h));
    let mut state = State::new();
    let mut pool = ExprPool::new();

    loop {
        let p = "λ ".to_owned();
        rl.helper_mut().expect("No helper").colored_prompt =
            format!("\x1b[1;32m{p}\x1b[0m");
        let readline = rl.readline(&p);
        match readline {
            Ok(line) => {
                let stdout = stdout().lock();

                let ast = lex(&line)
                    .and_then(|tks| parse_with_pool(tks, &mut pool))?;
                let mut ctx = IntrpCtx {
                    writer: stdout,
                    state: &mut state,
                    pool: &pool,
                };

                for statement in ast {
                    let r = ctx.eval_ast(statement)?;
                    if r != Value::Nothing {
                        println!("{}", r);
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("Interrupted");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("Encountered Eof");
                break;
            }
            Err(err) => {
                println!("Error: {err:?}");
                break;
            }
        }
    }

    Ok(())
}
