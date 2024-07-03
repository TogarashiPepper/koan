use std::{
    iter::{Map, Peekable},
    ops::Range,
    str::Chars,
};

use crate::error::{KoanError, LexError};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Operator {
    PiTimes,
    Sqrt,
    Power,
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
    NotEqual,
    DoublePipe,
    DoubleAnd,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    Ident,
    Number,
    Op(Operator),
    LParen,
    RParen,
    Let,
    Semicolon,
    String,
    Fun,
    Comma,
    LCurly,
    RCurly,
    LBracket,
    RBracket,
}

impl Operator {
    pub fn is_inf_op(&self) -> bool {
        matches!(
            self,
            Operator::Plus
                | Operator::Minus
                | Operator::Times
                | Operator::Slash
                | Operator::DoubleEqual
                | Operator::Greater
                | Operator::GreaterEqual
                | Operator::Lesser
                | Operator::LesserEqual
                | Operator::Power
                | Operator::NotEqual
                | Operator::DoubleAnd
                | Operator::DoublePipe
        )
    }

    pub fn is_pre_op(&self) -> bool {
        matches!(self, Operator::PiTimes | Operator::Minus | Operator::Sqrt)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token<'a> {
    pub variant: TokenType,
    pub location: Range<usize>,
    pub lexeme: &'a str,
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

    /// Helper method for multi-char variants, it will check if the next char matches the specified
    /// char and if it does it will consume it and build the variant for the pair of characters.
    /// If the next char does not match the specified char it will return the token for the single
    /// character.
    ///
    /// If the first provided token in the token pair is `None`, then that means if the next char
    /// does not match the specified char then it will return `LexError::PartialMultiCharToken` as
    /// there's no fallback variant.
    fn variant_pair(
        self,
        iterator: &mut Peekable<Map<Chars, impl FnMut(char) -> (usize, char)>>,
        (first_char, next_char): (char, char),
        (single_char_token, char_pair_token): (Option<TokenType>, TokenType),
    ) -> Result<TokenBuilder<'a>, KoanError> {
        match iterator.peek() {
            Some((_, peek_char)) if *peek_char == next_char => {
                iterator.next();

                Ok(self.second(next_char.len_utf8()).variant(char_pair_token))
            }
            // TODO: probably avoid Some(_) here later on for cases of multi-chars with the same start char
            None | Some(_) => match single_char_token {
                Some(single_char) => Ok(self.variant(single_char)),
                None => Err(LexError::PartialMultiCharToken(first_char, next_char).into()),
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

pub fn lex(input: &str) -> Result<Vec<Token<'_>>, KoanError> {
    let mut res = vec![];
    let mut idx = 0;
    // Poor man's enumerate that accounts for multi-byte characters
    let mut it = input
        .chars()
        .map(|c| {
            let tmp = idx;
            idx += c.len_utf8();

            (tmp, c)
        })
        .peekable();

    while let Some((idx, c)) = it.next() {
        use Operator::*;
        use TokenType::*;

        if c.is_whitespace() {
            continue;
        }

        let builder = TokenBuilder::new(input)
            .lexeme(&input[idx..idx + c.len_utf8()])
            .idx(idx);

        // `Builder::build(match { ... })` or `match { ... }.build()` ?
        let token = TokenBuilder::build(match c {
            '○' => builder.variant(Op(PiTimes)),
            '√' => builder.variant(Op(Sqrt)),
            '×' => builder.variant(Op(Times)),
            '≠' => builder.variant(Op(NotEqual)),
            '≤' => builder.variant(Op(LesserEqual)),
            '≥' => builder.variant(Op(GreaterEqual)),
            '^' => builder.variant(Op(Power)),
            '+' => builder.variant(Op(Plus)),
            '-' => builder.variant(Op(Minus)),
            '*' => builder.variant(Op(Times)),
            '/' => builder.variant(Op(Slash)),
            '(' => builder.variant(LParen),
            ')' => builder.variant(RParen),
            ';' => builder.variant(Semicolon),
            ',' => builder.variant(Comma),
            '{' => builder.variant(LCurly),
            '}' => builder.variant(RCurly),
            '[' => builder.variant(LBracket),
            ']' => builder.variant(RBracket),
            '|' => builder.variant_pair(&mut it, ('|', '|'), (None, Op(DoublePipe)))?,
            '&' => builder.variant_pair(&mut it, ('&', '&'), (None, Op(DoubleAnd)))?,
            '!' => builder.variant_pair(&mut it, ('!', '='), (None, Op(NotEqual)))?,
            '=' => builder.variant_pair(&mut it, ('=', '='), (Some(Op(Equal)), Op(DoubleEqual)))?,
            '>' => {
                builder.variant_pair(&mut it, ('>', '='), (Some(Op(Greater)), Op(GreaterEqual)))?
            }
            '<' => {
                builder.variant_pair(&mut it, ('<', '='), (Some(Op(Lesser)), Op(LesserEqual)))?
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut end = idx;
                while let Some((_, k)) = it.peek() {
                    if k.is_alphanumeric() || *k == '_' {
                        it.next();

                        end += 1;
                    } else {
                        break;
                    }
                }

                builder.second(end - idx).variant(match &input[idx..=end] {
                    "let" => TokenType::Let,
                    "fun" => TokenType::Fun,
                    _ => TokenType::Ident,
                })
            }
            '0'..='9' => {
                let mut end = idx;
                let mut seen_dot = false;
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

                builder.second(end - idx).variant(TokenType::Number)
            }
            '"' => {
                let start = idx + 1;
                let mut end = idx + 1;
                it.next();

                for (_, k) in it.by_ref() {
                    end += 1;

                    if k == '"' {
                        break;
                    }
                }

                res.push(Token {
                    variant: TokenType::String,
                    location: start..end,
                    lexeme: &input[start..end],
                });
                continue;
            }
            otherwise => {
                return Err(LexError::InvalidToken(
                    input[idx..idx + otherwise.len_utf8()].to_string(),
                )
                .into())
            }
        });

        res.push(token);
    }

    Ok(res)
}

#[cfg(test)]
mod tests {
    use crate::error::KoanError;

    use super::{
        lex, LexError,
        Operator::*,
        Token,
        TokenType::{self, *},
    };

    fn ok(t: Token<'_>) -> Result<Vec<Token<'_>>, KoanError> {
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
        lex_single("○", Op(PiTimes));
    }

    #[test]
    fn lex_ident_p1() {
        lex_single("p1", Ident);
    }

    #[test]
    fn lex_pitimes_one() {
        let got = lex("○1");
        assert_eq!(
            got,
            Ok(vec![
                Token {
                    variant: Op(PiTimes),
                    location: 0.."○".len(),
                    lexeme: "○",
                },
                Token {
                    variant: Number,
                    location: 3..4,
                    lexeme: "1",
                }
            ])
        )
    }

    #[test]
    fn lex_plus() {
        lex_single("+", Op(Plus));
    }

    #[test]
    fn lex_minus() {
        lex_single("-", Op(Minus));
    }

    #[test]
    fn lex_times() {
        lex_single("*", Op(Times));
    }

    #[test]
    fn lex_slash() {
        lex_single("/", Op(Slash));
    }

    #[test]
    fn lex_double_equal() {
        lex_single("==", Op(DoubleEqual));
    }

    #[test]
    fn lex_greater_and_greater_equal() {
        lex_single(">", Op(Greater));
        lex_single(">=", Op(GreaterEqual));
    }

    #[test]
    fn lex_lesser_and_lesser_equal() {
        lex_single("<", Op(Lesser));
        lex_single("<=", Op(LesserEqual));
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
                    variant: Op(DoubleEqual),
                    location: 0..2,
                    lexeme: "==",
                },
                Token {
                    variant: Op(Plus),
                    location: 2..3,
                    lexeme: "+",
                }
            ])
        )
    }

    #[test]
    fn lex_one_plus_two_minus_three() {
        let got = lex("1 + 2 - 3");
        assert_eq!(
            got,
            Ok(vec![
                Token {
                    variant: Number,
                    location: 0..1,
                    lexeme: "1",
                },
                Token {
                    variant: Op(Plus),
                    location: 2..3,
                    lexeme: "+",
                },
                Token {
                    variant: Number,
                    location: 4..5,
                    lexeme: "2",
                },
                Token {
                    variant: Op(Minus),
                    location: 6..7,
                    lexeme: "-",
                },
                Token {
                    variant: Number,
                    location: 8..9,
                    lexeme: "3",
                }
            ])
        )
    }

    #[test]
    fn lex_string() {
        let input = r#""Hello, World""#;
        let expected = TokenType::String;
        let got = lex(input);

        assert_eq!(
            got,
            ok(Token {
                variant: expected,
                location: 1..13,
                lexeme: dbg!(&input[1..13])
            })
        );
    }

    #[test]
    fn lex_ident() {
        lex_single("thisonehasnocaps", Ident);
        lex_single("THISONEhascaps", Ident);
        lex_single("_thisoneHasanunderscore", Ident);
        lex_single("thisoneHas_anunderscoreinbetween", Ident);
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
        assert_eq!(got, Err(LexError::InvalidToken("~".to_owned()).into()));
    }
}
