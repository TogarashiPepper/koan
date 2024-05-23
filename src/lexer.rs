use std::{
    iter::{Enumerate, Peekable},
    ops::Range,
    str::Chars,
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    Ident,
    Number,
    PiTimes,
    Plus,
    Minus,
    Times,
    Slash,
    Equal,
    DoubleEqual,
    Greater,
    GreaterEqual,
    Lesser,
    LesserEqual,
    LParen,
    RParen,
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

    /// Helper method for multi-char tokens, it will check if the next char matches the specified
    /// char and if it does it will consume it and return the token for the pair of characters.
    /// If the next char does not match the specified char it will return the token for the single
    /// character.
    /// If the first provided token in the token pair is `None`, then that means if the next char
    /// does not match the specified char then it will return `None`, indicating that the function
    /// got an invalid char pair.
    fn variant_pair(
        self,
        iterator: &mut Peekable<Enumerate<Chars>>,
        char: char,
        (single_char_token, char_pair_token): (Option<TokenType>, TokenType),
    ) -> Option<TokenBuilder<'a>> {
        match iterator.peek() {
            Some((_, next_char)) if *next_char == char => {
                iterator.next();

                Some(self.second(char.len_utf8()).variant(char_pair_token))
            }
            // TODO: probably avoid Some(_) here later on for cases of multi-chars with the same start char
            None | Some(_) => match single_char_token {
                Some(single_char) => Some(self.variant(single_char)),
                None => None,
            },
        }
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
        match (self.variant, self.idx, self.lexeme) {
            (Some(variant), Some(idx), Some(lexeme)) => Token {
                variant,
                location: idx..idx + lexeme.len(),
                lexeme,
            },
            _ => panic!("Attempted to build builder without all of the fields filled out"),
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

        if c.is_whitespace() {
            continue;
        }

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
            '(' => builder.variant(LParen),
            ')' => builder.variant(RParen),
            // TODO: Reduce code duplication for multi-char tokens
            '=' => match it.peek() {
                Some((_, '=')) => {
                    it.next();

                    builder.second('='.len_utf8()).variant(DoubleEqual)
                }
                None | Some(_) => builder.variant(Equal),
            },
            '>' => match it.peek() {
                Some((_, '=')) => {
                    it.next();

                    builder.second('='.len_utf8()).variant(GreaterEqual)
                }
                None | Some(_) => builder.variant(Greater),
            },
            '<' => match it.peek() {
                Some((_, '=')) => {
                    it.next();

                    builder.second('='.len_utf8()).variant(LesserEqual)
                }
                None | Some(_) => builder.variant(Lesser),
            },
            'a'..='z' | 'A'..='Z' | '_' => {
                let start = idx;
                let mut end = idx;

                // TODO: Iterator version of this?
                while let Some((_, k)) = it.peek() {
                    if k.is_alphabetic() {
                        it.next();

                        end += 1;
                    } else {
                        break;
                    }
                }

                builder.second(end - start).variant(Ident)
            }
            '0'..='9' => {
                let start = idx;
                let mut end = idx;
                let mut seen_dot = false;

                // TODO: Iterator version of this?
                while let Some((_, k)) = it.peek() {
                    if k.is_ascii_digit() {
                        it.next();

                        end += 1;
                    } else if *k == '.' && !seen_dot {
                        seen_dot = true;

                        it.next();
                        end += 1;

                        // Consume digit or else un-consume dot
                        match it.peek() {
                            Some((_, '0'..='9')) => {
                                it.next();
                                end += 1
                            }
                            _ => end -= 1,
                        }
                    } else {
                        break;
                    }
                }

                builder.second(end - start).variant(Number)
            }
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
    fn lex_greater_and_greater_equal() {
        lex_single(">", Greater);
        lex_single(">=", GreaterEqual);
    }

    #[test]
    fn lex_lesser_and_lesser_equal() {
        lex_single("<", Lesser);
        lex_single("<=", LesserEqual);
    }

    #[test]
    fn lex_l_r_parens() {
        lex_single("(", LParen);
        lex_single(")", RParen);
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
    fn lex_ident() {
        lex_single("thisonehasnocaps", Ident);
        lex_single("THISONEhascaps", Ident);
        lex_single("_thisoneHasanunderscore", Ident);
    }

    #[test]
    fn lex_number() {
        lex_single("1234567890", Number);
        lex_single("3.141592653589793", Number);
        lex_single("0", Number);
    }

    #[test]
    fn lex_invalid_token() {
        let got = lex("~");
        assert_eq!(got, Err(LexError::InvalidToken("~".to_owned())));
    }
}
