use std::ops::Range;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    PiTimes,
    Plus,
    Minus,
    Times,
    Slash,
    Equal,
    DoubleEqual,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token<'a> {
    variant: TokenType,
    location: Range<usize>,
    lexeme: &'a str,
}

pub struct TokenBuilder<'a> {
    variant: Option<TokenType>,
    idx: Option<usize>,
    lexeme: Option<&'a str>,
    input_string: &'a str,
}

impl<'a> TokenBuilder<'a> {
    fn new(input: &'a str) -> TokenBuilder<'a> {
        TokenBuilder {
            variant: None,
            idx: None,
            lexeme: None,
            input_string: input,
        }
    }

    fn variant(mut self, variant: TokenType) -> TokenBuilder<'a> {
        self.variant = Some(variant);
        self
    }

    fn lexeme(mut self, lexeme: &'a str) -> TokenBuilder<'a> {
        self.lexeme = Some(lexeme);
        self
    }

    /// Add the second character for multi-char tokens, the len is the length of the second
    /// character in bytes. The two characters are assumed to be next to each other within the input
    /// string, if they are not this method will not behave as expected.
    fn second(mut self, len: usize) -> TokenBuilder<'a> {
        let idx = self.idx.unwrap();
        self.lexeme = Some(&self.input_string[idx..idx + self.lexeme.unwrap().len() + len]);

        self
    }

    fn idx(mut self, idx: usize) -> TokenBuilder<'a> {
        self.idx = Some(idx);
        self
    }

    fn build(self) -> Token<'a> {
        if self.variant.is_some() && self.idx.is_some() && self.lexeme.is_some() {
            let idx = self.idx.unwrap();
            let variant = self.variant.unwrap();
            let location = idx..idx + self.lexeme.unwrap().len();
            let lexeme = &self.input_string[location.clone()];

            Token {
                variant,
                location,
                lexeme,
            }
        } else {
            panic!("Attempted to build builder without all of the fields filled out")
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum LexError {
    InvalidToken(String),
}

pub fn lex(input: &str) -> Result<Vec<Token<'_>>, LexError> {
    let mut it = input.chars().enumerate().peekable();
    let mut res = vec![];

    while let Some((idx, c)) = it.next() {
        use TokenType::*;

        let builder = TokenBuilder::new(input)
            .lexeme(&input[idx..idx + c.len_utf8()])
            .idx(idx);

        // `Builder::build(match { ... })` or `match { ... }.build()` ?
        let token = TokenBuilder::build(match c {
            '○' => builder.variant(PiTimes),
            '+' => builder.variant(Plus),
            '-' => builder.variant(Minus),
            '*' => builder.variant(Times),
            '/' => builder.variant(Slash),
            '=' => match it.peek() {
                Some((_, '=')) => {
                    it.next();

                    builder.second('='.len_utf8()).variant(DoubleEqual)
                }
                None | Some(_) => builder.variant(Equal),
            },
            otherwise => {
                return Err(LexError::InvalidToken(
                    input[idx..idx + otherwise.len_utf8()].to_string(),
                ))
            }
        });

        res.push(token);
    }

    Ok(res)
}

#[cfg(test)]
mod tests {
    use super::{
        lex, LexError, Token,
        TokenType::{self, *},
    };

    fn ok(t: Token<'_>) -> Result<Vec<Token<'_>>, LexError> {
        Ok(vec![t])
    }

    fn lex_single(input: &'static str, expected: TokenType) {
        let got = lex(input);

        assert_eq!(
            got,
            ok(Token {
                variant: expected,
                location: 0..input.len(),
                lexeme: &input[0..input.len()]
            })
        )
    }

    #[test]
    fn lex_pitimes() {
        lex_single("○", PiTimes);
    }

    #[test]
    fn lex_plus() {
        lex_single("+", Plus);
    }

    #[test]
    fn lex_minus() {
        lex_single("-", Minus);
    }

    #[test]
    fn lex_times() {
        lex_single("*", Times);
    }

    #[test]
    fn lex_slash() {
        lex_single("/", Slash);
    }

    #[test]
    fn lex_double_equal() {
        lex_single("==", DoubleEqual);
    }

    #[test]
    fn lex_double_then_plus() {
        let got = lex("==+");
        assert_eq!(
            got,
            Ok(vec![
                Token {
                    variant: DoubleEqual,
                    location: 0..2,
                    lexeme: "==",
                },
                Token {
                    variant: Plus,
                    location: 2..3,
                    lexeme: "+",
                }
            ])
        )
    }

    #[test]
    fn lex_invalid_token() {
        let got = lex("~");
        assert_eq!(got, Err(LexError::InvalidToken("~".to_owned())));
    }
}
