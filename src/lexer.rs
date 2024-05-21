use std::ops::Range;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    PiTimes,
    Plus,
    Minus,
    Times,
    Slash,
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
    chr: Option<char>,
    input_string: &'a str,
}

impl<'a> TokenBuilder<'a> {
    fn new(input: &'a str) -> TokenBuilder<'a> {
        TokenBuilder {
            variant: None,
            idx: None,
            chr: None,
            input_string: input,
        }
    }

    fn variant(mut self, variant: TokenType) -> TokenBuilder<'a> {
        self.variant = Some(variant);
        self
    }

    fn chr(mut self, chr: char) -> TokenBuilder<'a> {
        self.chr = Some(chr);
        self
    }

    fn idx(mut self, idx: usize) -> TokenBuilder<'a> {
        self.idx = Some(idx);
        self
    }

    fn build(self) -> Token<'a> {
        if self.variant.is_some() && self.idx.is_some() && self.chr.is_some() {
            let idx = self.idx.unwrap();
            let variant = self.variant.unwrap();
            let location = idx..idx + self.chr.unwrap().len_utf8();
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
    let it = input.chars().enumerate().peekable();
    let mut res = vec![];

    for (idx, c) in it {
        use TokenType::*;

        let builder = TokenBuilder::new(input).chr(c).idx(idx);

        // `Builder::build(match { ... })` or `match { ... }.build()` ?
        let token = TokenBuilder::build(match c {
            '○' => builder.variant(PiTimes),
            '+' => builder.variant(Plus),
            '-' => builder.variant(Minus),
            '*' => builder.variant(Times),
            '/' => builder.variant(Slash),
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
    fn lex_invalid_token() {
        let got = lex("~");
        assert_eq!(got, Err(LexError::InvalidToken("~".to_owned())));
    }
}
