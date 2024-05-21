use std::ops::Range;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    PiTimes,
    Plus,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token<'a> {
    variant: TokenType,
    location: Range<usize>,
    lexeme: &'a str,
}

#[derive(Debug, PartialEq)]
pub enum LexError {
    InvalidToken(String),
}

pub fn lex<'a>(input: &'a str) -> Result<Vec<Token<'a>>, LexError> {
    let it = input.chars().enumerate().peekable();
    let mut res = vec![];

    for (idx, c) in it {
        use TokenType::*;

        // TODO: Make this code more DRY, perhaps a macro or some kind of helper?
        let token = match c {
            x @ '○' => Token {
                variant: PiTimes,
                location: idx..idx + x.len_utf8(),
                lexeme: &input[idx..idx + x.len_utf8()],
            },
            x @ '+' => Token {
                variant: Plus,
                location: idx..idx + x.len_utf8(),
                lexeme: &input[idx..idx + x.len_utf8()],
            },
            _ => return Err(LexError::InvalidToken(input[idx..=idx].to_string())),
        };

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

    fn ok<'a>(t: Token<'a>) -> Result<Vec<Token<'a>>, LexError> {
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
}
