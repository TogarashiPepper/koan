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
    PreOp {
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
        Some(Token { lexeme: "(", .. }) => {
            let lhs = expr_bp(it, 0)?;
            assert_eq!(it.next().map(|x| x.lexeme), Some(")"));

            lhs
        }
        Some(x) if x.variant.is_pre_op() => {
            let ((), r_bp) = prefix_binding_power(x.variant);
            let rhs = expr_bp(it, r_bp)?;

            Expr::PreOp {
                op: x.variant,
                rhs: Box::new(rhs),
            }
        }
        Some(_) | None => {
            return Err(ParseError::new(ParseErrorType::ExpectedLiteral(
                "number".to_string(),
            )))
        }
    };

    while let Some(op) = it.peek() {
        // Its okay, tokens are cheap 👍
        let op = op.clone();

        let Some((l_bp, r_bp)) = infix_binding_power(op.variant) else {
            break;
        };

        if !op.variant.is_inf_op() {
            return Err(ParseError::new(ParseErrorType::ExpectedInfixOp));
        };

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

fn infix_binding_power(op: TokenType) -> Option<(u8, u8)> {
    Some(match op {
        TokenType::Plus | TokenType::Minus => (3, 4),
        TokenType::Times | TokenType::Slash => (5, 6),
        TokenType::DoubleEqual
        | TokenType::Greater
        | TokenType::GreaterEqual
        | TokenType::Lesser
        | TokenType::LesserEqual => (1, 2),
        _ => return None,
    })
}

fn prefix_binding_power(op: TokenType) -> ((), u8) {
    match op {
        TokenType::PiTimes => ((), 7),
        _ => panic!("Expected prefix operator, found some other token"),
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::lex, parser::Ast};
    use super::*;

    // `Box::new` is a lot to write across all test cases, and `box` is reserved so while single-char
    // func name is a bit yucky, I think its worth the marginally shorter test cases.
    fn b<T>(x: T) -> Box<T> {
        Box::new(x)
    }

    fn assert_parse_expr(input: &'static str, expected: Expr) {
        assert_parse(input, Ast::Expression(expected));
    }

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

        assert_parse_expr(
            "1 + 2",
            BinOp {
                lhs: b(NumLit(1.0)),
                op: TokenType::Plus,
                rhs: b(NumLit(2.0)),
            },
        )
    }

    #[test]
    fn simple_preop() {
        use Expr::*;

        assert_parse_expr(
            "○1",
            PreOp {
                op: TokenType::PiTimes,
                rhs: b(NumLit(1.0)),
            },
        )
    }

    #[test]
    fn mult_binop_no_prec() {
        use Expr::*;

        assert_parse_expr(
            "1 + 2 - 3",
            BinOp {
                lhs: b(BinOp {
                    lhs: b(NumLit(1.0)),
                    op: TokenType::Plus,
                    rhs: b(NumLit(2.0)),
                }),
                op: TokenType::Minus,
                rhs: b(NumLit(3.0)),
            },
        )
    }

    #[test]
    fn paren_plus_times() {
        use Expr::*;

        assert_parse_expr(
            "(1 + 2) * 3",
            BinOp {
                lhs: b(BinOp {
                    lhs: b(NumLit(1.0)),
                    op: TokenType::Plus,
                    rhs: b(NumLit(2.0)),
                }),
                op: TokenType::Times,
                rhs: b(NumLit(3.0)),
            },
        )
    }

    #[test]
    fn plus_times() {
        use Expr::*;

        assert_parse_expr(
            "1 + 2 * 3",
            BinOp {
                lhs: b(NumLit(1.0)),
                op: TokenType::Plus,
                rhs: b(BinOp {
                    lhs: b(NumLit(2.0)),
                    op: TokenType::Times,
                    rhs: b(NumLit(3.0)),
                }),
            },
        )
    }
}
