use std::iter::Peekable;

use crate::{
    error::{KoanError, ParseError},
    lexer::{Operator, Token, TokenType},
};

#[derive(Debug, PartialEq)]
pub enum Ast {
    Expression(Expr),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    // Convert this to BinOp(BinOp) for impls on it?
    BinOp {
        lhs: Box<Expr>,
        op: Operator,
        rhs: Box<Expr>,
    },
    PreOp {
        op: Operator,
        rhs: Box<Expr>,
    },
    Ident(String),
    // TODO: Add typed integer literals to avoid JS-esque strangeness
    NumLit(f64),
}

pub fn parse(tokens: Vec<Token<'_>>) -> Result<Ast, KoanError> {
    let expr = expr(tokens);

    Ok(Ast::Expression(expr?))
}

pub fn expr(tokens: Vec<Token<'_>>) -> Result<Expr, KoanError> {
    expr_bp(&mut tokens.into_iter().peekable(), 0)
}

fn expr_bp<'a>(
    it: &mut Peekable<impl Iterator<Item = Token<'a>>>,
    min_bp: u8,
) -> Result<Expr, KoanError> {
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
        Some(Token {
            variant: TokenType::Op(op),
            ..
        }) => {
            let ((), r_bp) = prefix_binding_power(op);
            let rhs = expr_bp(it, r_bp)?;

            Expr::PreOp {
                op,
                rhs: Box::new(rhs),
            }
        }
        Some(_) | None => return Err(ParseError::ExpectedLiteral("number".to_string()).into()),
    };

    while let Some(Token {
        variant: TokenType::Op(op),
        ..
    }) = it.peek()
    {
        let op = *op;

        let (l_bp, r_bp) = infix_binding_power(op);

        if !op.is_inf_op() {
            return Err(ParseError::ExpectedInfixOp.into());
        };

        if l_bp < min_bp {
            break;
        }

        it.next();
        let rhs = expr_bp(it, r_bp)?;

        lhs = Expr::BinOp {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        }
    }

    Ok(lhs)
}

fn infix_binding_power(op: Operator) -> (u8, u8) {
    use Operator::*;

    if !op.is_inf_op() {
        panic!("Expected infix op")
    }

    match op {
        Plus | Minus => (3, 4),
        Times | Slash => (5, 6),
        DoubleEqual | Greater | GreaterEqual | Lesser | LesserEqual => (1, 2),
        _ => unreachable!(),
    }
}

fn prefix_binding_power(op: Operator) -> ((), u8) {
    match op {
        Operator::PiTimes => ((), 7),
        _ => panic!("Expected prefix operator, found some other token"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer::lex, parser::Ast};

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
                op: Operator::Plus,
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
                op: Operator::PiTimes,
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
                    op: Operator::Plus,
                    rhs: b(NumLit(2.0)),
                }),
                op: Operator::Minus,
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
                    op: Operator::Plus,
                    rhs: b(NumLit(2.0)),
                }),
                op: Operator::Times,
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
                op: Operator::Plus,
                rhs: b(BinOp {
                    lhs: b(NumLit(2.0)),
                    op: Operator::Times,
                    rhs: b(NumLit(3.0)),
                }),
            },
        )
    }
}
