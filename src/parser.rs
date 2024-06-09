use std::iter::Peekable;

use crate::lexer::{Token, TokenType};

/// Wrapper around TokenType that ensures its an operator token, rather than a quote or literal
struct Operator(TokenType);

impl TryFrom<TokenType> for Operator {
    type Error = &'static str;

    fn try_from(value: TokenType) -> Result<Self, Self::Error> {
        Ok(match value {
            x @ (TokenType::PiTimes
            | TokenType::Plus
            | TokenType::Minus
            | TokenType::Times
            | TokenType::Slash
            | TokenType::Equal
            | TokenType::DoubleEqual
            | TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::Lesser
            | TokenType::LesserEqual) => Self(x),
            _ => return Err("The provided TokenType is not an operator"),
        })
    }
}

enum Ast {}

enum Expr {
    // Convert this to BinOp(BinOp) for impls on it?
    BinOp {
        lhs: Box<Expr>,
        op: Operator,
        rhs: Box<Expr>,
    },
    Ident(String),
    // TODO: Add typed integer literals to avoid JS-esque strangeness
    NumLit(f64),
}

pub fn parse(tokens: Vec<Token<'_>>) -> Ast {
    todo!()
}

pub fn expr(tokens: Vec<Token<'_>>) -> Expr {
    expr_bp(tokens.into_iter().peekable())
}

fn expr_bp<'a>(it: Peekable<impl Iterator<Item = Token<'a>>>) -> Expr {
    todo!()
}
