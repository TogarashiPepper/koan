use std::{backtrace::Backtrace, iter::Peekable};

use crate::lexer::{Token, TokenType};

#[derive(Debug)]
pub struct ParseError(ParseErrorType, Backtrace);

impl ParseError {
    fn new(variant: ParseErrorType) -> Self {
        Self(variant, Backtrace::capture())
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseErrorType {
    ExpectedLiteral(String),
    ExpectedInfixOp,
}

#[derive(Debug, PartialEq)]
pub enum Ast {
    Expression(Expr),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    // Convert this to BinOp(BinOp) for impls on it?
    BinOp {
        lhs: Box<Expr>,
        op: TokenType,
        rhs: Box<Expr>,
    },
    Ident(String),
    // TODO: Add typed integer literals to avoid JS-esque strangeness
    NumLit(f64),
}

pub fn parse(tokens: Vec<Token<'_>>) -> Result<Ast, ParseError> {
    let expr = expr(tokens);

    Ok(Ast::Expression(expr?))
}

pub fn expr(tokens: Vec<Token<'_>>) -> Result<Expr, ParseError> {
    expr_bp(&mut tokens.into_iter().peekable(), 0)
}

fn expr_bp<'a>(
    it: &mut Peekable<impl Iterator<Item = Token<'a>>>,
    min_bp: u8,
) -> Result<Expr, ParseError> {
    let mut lhs = match it.next() {
        Some(
            x @ Token {
                variant: TokenType::Number,
                ..
            },
        ) => Expr::NumLit(x.lexeme.parse().unwrap()), // Ok to unwrap because it's a lexeme
        Some(_) | None => {
            return Err(ParseError::new(ParseErrorType::ExpectedLiteral(
                "number".to_string(),
            )))
        }
    };

    while let Some(op) = it.peek() {
        if !op.variant.is_inf_op() {
            return Err(ParseError::new(ParseErrorType::ExpectedInfixOp));
        };

        // Its okay, tokens are cheap 👍
        let op = op.clone();

        let (l_bp, r_bp) = infix_binding_power(op.variant);

        if l_bp < min_bp {
            break;
        }

        it.next();
        let rhs = expr_bp(it, r_bp)?;

        lhs = Expr::BinOp {
            lhs: Box::new(lhs),
            op: op.variant,
            rhs: Box::new(rhs),
        }
    }

    Ok(lhs)
}

fn infix_binding_power(op: TokenType) -> (u8, u8) {
    match op {
        TokenType::Plus | TokenType::Minus => (3, 4),
        TokenType::Times | TokenType::Slash => (5, 6),
        TokenType::DoubleEqual
        | TokenType::Greater
        | TokenType::GreaterEqual
        | TokenType::Lesser
        | TokenType::LesserEqual => (1, 2),
        _ => panic!("Expected operator, found some other token"),
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::lex, parser::Ast};

    use super::*;

    fn assert_parse(input: &'static str, expected: Ast) {
        // For the sake of testing the parser, we'll assume the lexer is correct
        let tokens = lex(input).unwrap();
        let ast = match parse(tokens) {
            Ok(x) => x,
            Err(err) => {
                eprintln!("error: {:?}\nbacktrace: {}", err.0, err.1);
                std::process::exit(1)
            }
        };

        assert_eq!(ast, expected);
    }

    #[test]
    fn simple_binop() {
        use Expr::*;

        assert_parse(
            "1 + 2",
            Ast::Expression(BinOp {
                lhs: Box::new(NumLit(1.0)),
                op: TokenType::Plus,
                rhs: Box::new(NumLit(2.0)),
            }),
        )
    }

    #[test]
    fn mult_binop_no_prec() {
        use Expr::*;

        assert_parse(
            "1 + 2 - 3",
            Ast::Expression(BinOp {
                lhs: Box::new(BinOp {
                    lhs: Box::new(NumLit(1.0)),
                    op: TokenType::Plus,
                    rhs: Box::new(NumLit(2.0)),
                }),
                op: TokenType::Minus,
                rhs: Box::new(NumLit(3.0)),
            }),
        )
    }
}
