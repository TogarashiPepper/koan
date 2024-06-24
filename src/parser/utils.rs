use std::iter::Peekable;

use crate::{
    error::{KoanError, ParseError},
    lexer::{Token, TokenType},
};

/// Consume the next token, error-ing if the type doesn't match what is expected
pub fn expect<'a>(
    it: &mut Peekable<impl Iterator<Item = Token<'a>>>,
    ty: TokenType,
) -> Result<Token<'a>, KoanError> {
    Err(match it.next() {
        Some(t) if t.variant == ty => return Ok(t),
        Some(t) => ParseError::ExpectedFound(ty, t.variant).into(),
        None => ParseError::ExpectedFoundEof(ty).into(),
    })
}

pub fn check<'a>(it: &mut Peekable<impl Iterator<Item = Token<'a>>>, ty: TokenType) -> bool {
    match it.peek() {
        Some(t) => t.variant == ty,
        None => false,
    }
}

pub fn multi_expect<'a, const N: usize>(
    it: &mut Peekable<impl Iterator<Item = Token<'a>>>,
    tys: &[TokenType; N],
) -> Result<[Token<'a>; N], KoanError> {
    let mut res = Vec::with_capacity(N);

    for expected in tys {
        res.push(expect(it, *expected)?);
    }

    Ok(res.try_into().unwrap())
}

