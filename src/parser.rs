mod utils;

use std::iter::Peekable;

use crate::{
    error::{KoanError, ParseError},
    lexer::{Operator, Token, TokenType},
    parser::utils::{expect, multi_expect, check},
};

#[derive(Debug, PartialEq)]
pub enum Ast {
    Expression(Expr),
    Statement(Expr),
    Block(Vec<Ast>),
    LetDecl(String, Expr),
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
    StrLit(String),
    FunCall(String, Vec<Expr>),
}

pub fn parse(tokens: Vec<Token<'_>>) -> Result<Vec<Ast>, KoanError> {
    let mut it = tokens.into_iter().peekable();

    program(&mut it, false)
}

/// in_block parameter tells the program parser to error, or simply return the ast when it
/// encounters a `}` character
fn program<'a>(
    it: &mut Peekable<impl Iterator<Item = Token<'a>>>,
    in_block: bool,
) -> Result<Vec<Ast>, KoanError> {
    let mut program = vec![];

    while let Some(p) = it.peek() {
        program.push(match p.variant {
            TokenType::Let => let_decl(it)?,
            TokenType::RCurly => {
                if !in_block {
                    return Err(ParseError::Unexpected(TokenType::RCurly).into());
                }

                break;
            }
            TokenType::LCurly => block(it)?,
            _ => {
                let expr = expr_bp(it, 0)?;
                match check(it, TokenType::Semicolon) {
                    true => {
                        it.next();
                        Ast::Statement(expr)
                    }
                    false => Ast::Expression(expr),
                }
            }
        });
    }

    Ok(program)
}

fn block<'a>(it: &mut Peekable<impl Iterator<Item = Token<'a>>>) -> Result<Ast, KoanError> {
    let _ = expect(it, TokenType::LCurly).and_then(|_| program(it, true));
    let _ = expect(it, TokenType::RCurly)?;

    Ok(Ast::Block(block))
}

fn fun_call<'a>(
    it: &mut Peekable<impl Iterator<Item = Token<'a>>>,
    ident: String,
) -> Result<Expr, KoanError> {
    let _ = expect(it, TokenType::LParen)?;

    let mut params = vec![];

    loop {
        match it.peek() {
            Some(tok) => match tok.variant {
                TokenType::RParen => {
                    it.next();
                    break;
                }
                _ => {
                    let param = expr_bp(it, 0)?;
                    params.push(param);

                    if let Some(Token {
                        variant: TokenType::RParen,
                        ..
                    }) = it.peek()
                    {
                        it.next();
                        break;
                    }

                    let _ = expect(it, TokenType::Comma)?;
                }
            },
            None => return Err(ParseError::ExpectedFoundEof(TokenType::RParen).into()),
        }
    }

    Ok(Expr::FunCall(ident, params))
}

fn let_decl<'a>(it: &mut Peekable<impl Iterator<Item = Token<'a>>>) -> Result<Ast, KoanError> {
    let [_, ident, _] = multi_expect(
        it,
        &[
            TokenType::Let,
            TokenType::Ident,
            TokenType::Op(Operator::Equal),
        ],
    )?;
    let body = expr_bp(it, 0)?;
    let _ = expect(it, TokenType::Semicolon)?;

    Ok(Ast::LetDecl(ident.lexeme.to_owned(), body))
}

fn expr_bp<'a>(
    it: &mut Peekable<impl Iterator<Item = Token<'a>>>,
    min_bp: u8,
) -> Result<Expr, KoanError> {
    let mut lhs = match it.next() {
        Some(Token {
            variant: TokenType::Number,
            lexeme,
            ..
        }) => Expr::NumLit(lexeme.parse().unwrap()), // Ok to unwrap because it's a lexeme
        Some(Token {
            variant: TokenType::String,
            lexeme,
            ..
        }) => Expr::StrLit(lexeme.to_owned()),
        Some(Token {
            variant: TokenType::LParen,
            ..
        }) => {
            let lhs = expr_bp(it, 0)?;
            assert_eq!(it.next().map(|x| x.variant), Some(TokenType::RParen));

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
        Some(
            x @ Token {
                variant: TokenType::Ident,
                lexeme,
                ..
            },
        ) => {
            if let Some(TokenType::LParen) = it.peek().map(|x| x.variant) {
                fun_call(it, lexeme.to_owned())?
            } else {
                Expr::Ident(x.lexeme.to_owned())
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
        Operator::PiTimes | Operator::Minus => ((), 7),
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
                panic!("error: {:?}\nbacktrace: {}", err.0, err.1);
            }
        };

        assert_eq!(ast[0], expected);
    }

    #[test]
    fn let_declaration() {
        assert_parse(
            "let x = 10;",
            Ast::LetDecl("x".to_string(), Expr::NumLit(10.0)),
        );
    }

    #[test]
    fn let_decl_then_expr() {
        assert_parse(
            "let x = 1 + 2;",
            Ast::LetDecl(
                "x".to_string(),
                Expr::BinOp {
                    lhs: Box::new(Expr::NumLit(1.0)),
                    op: Operator::Plus,
                    rhs: Box::new(Expr::NumLit(2.0)),
                },
            ),
        )
    }

    #[test]
    fn funcall_nonempty() {
        assert_parse(
            "print(1 + 2, 3)",
            Ast::Expression(Expr::FunCall(
                "print".to_owned(),
                vec![
                    Expr::BinOp {
                        lhs: Box::new(Expr::NumLit(1.0)),
                        op: Operator::Plus,
                        rhs: Box::new(Expr::NumLit(2.0)),
                    },
                    Expr::NumLit(3.0),
                ],
            )),
        )
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
