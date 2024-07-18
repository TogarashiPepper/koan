mod utils;

use std::iter::Peekable;

use crate::{
    error::{ParseError, Result},
    lexer::{Operator, Token, TokenType},
};

#[derive(Debug, PartialEq, Clone)]
pub enum Ast {
    Expression(Expr),
    Statement(Expr),
    Block(Vec<Ast>),
    LetDecl(String, Expr),
    FunDecl {
        name: String,
        params: Vec<String>,
        body: Box<Ast>,
    },
}

#[derive(Debug, PartialEq, Clone)]
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
    Array(Vec<Expr>),
}

pub fn parse(tokens: Vec<Token<'_>>) -> Result<Vec<Ast>> {
    let mut it = TokenStream(tokens.into_iter().peekable());

    it.program(false)
}

pub struct TokenStream<'a, T: Iterator<Item = Token<'a>>>(Peekable<T>);

impl<'a, T: Iterator<Item = Token<'a>>> TokenStream<'a, T> {
    /// in_block parameter tells the program parser to error, or simply return the ast when it
    /// encounters a `}` character
    fn program(&mut self, in_block: bool) -> Result<Vec<Ast>> {
        let mut program = vec![];

        while let Some(p) = self.0.peek() {
            program.push(match p.variant {
                TokenType::Let => self.let_decl()?,
                TokenType::Fun => self.fun_def()?,
                TokenType::RCurly => {
                    if !in_block {
                        return Err(
                            ParseError::Unexpected(TokenType::RCurly).into()
                        );
                    }

                    break;
                }
                TokenType::LCurly => self.block()?,
                _ => {
                    let expr = self.expr_bp(0)?;
                    match self.check(TokenType::Semicolon) {
                        true => {
                            self.0.next();
                            Ast::Statement(expr)
                        }
                        false => Ast::Expression(expr),
                    }
                }
            });
        }

        Ok(program)
    }

    fn block(&mut self) -> Result<Ast> {
        let _ = self.expect(TokenType::LCurly)?;
        let block = self.program(true)?;
        let _ = self.expect(TokenType::RCurly)?;

        Ok(Ast::Block(block))
    }

    fn fun_def(&mut self) -> Result<Ast> {
        let [_, ident] =
            self.multi_expect(&[TokenType::Fun, TokenType::Ident])?;
        let params = self.list(
            (Some(TokenType::LParen), TokenType::RParen),
            TokenType::Comma,
            // list method already peaked b4 calling
            |stream| stream.expect(TokenType::Ident),
        )?;
        let block = self.block()?;

        Ok(Ast::FunDecl {
            name: ident.lexeme.to_owned(),
            params: params.into_iter().map(|t| t.lexeme.to_string()).collect(),
            body: Box::new(block),
        })
    }

    fn fun_call(&mut self, ident: String) -> Result<Expr> {
        let params = self.list(
            (Some(TokenType::LParen), TokenType::RParen),
            TokenType::Comma,
            |stream| stream.expr_bp(0),
        )?;

        Ok(Expr::FunCall(ident, params))
    }

    fn let_decl(&mut self) -> Result<Ast> {
        let [_, ident, _] = self.multi_expect(&[
            TokenType::Let,
            TokenType::Ident,
            TokenType::Op(Operator::Equal),
        ])?;
        let body = self.expr_bp(0)?;
        let _ = self.expect(TokenType::Semicolon)?;

        Ok(Ast::LetDecl(ident.lexeme.to_owned(), body))
    }

    fn expr_bp(&mut self, min_bp: u8) -> Result<Expr> {
        let mut lhs = match self.0.next() {
            Some(tok) => match tok.variant {
                TokenType::Number => {
                    if self.check(TokenType::Ident) {
                        let ident = self.expect(TokenType::Ident)?;

                        Expr::BinOp {
                            lhs: Box::new(Expr::NumLit(
                                tok.lexeme.parse().unwrap(),
                            )),
                            op: Operator::Times,
                            rhs: Box::new(Expr::Ident(ident.lexeme.to_owned())),
                        }
                    } else {
                        Expr::NumLit(tok.lexeme.parse().unwrap()) // Ok to unwrap because it's a lexeme
                    }
                }

                TokenType::String => Expr::StrLit(tok.lexeme.to_owned()),
                TokenType::LParen => {
                    let lhs = self.expr_bp(0)?;
                    let _ = self.expect(TokenType::RParen)?;

                    lhs
                }
                TokenType::Pipe => {
                    let lhs = self.expr_bp(0)?;
                    let _ = self.expect(TokenType::Pipe)?;

                    Expr::PreOp {
                        op: Operator::Abs,
                        rhs: Box::new(lhs),
                    }
                }
                TokenType::Op(op) => {
                    let ((), r_bp) = prefix_binding_power(op);
                    let rhs = self.expr_bp(r_bp)?;

                    Expr::PreOp {
                        op,
                        rhs: Box::new(rhs),
                    }
                }
                TokenType::Ident => {
                    if self.check(TokenType::LParen) {
                        self.fun_call(tok.lexeme.to_owned())?
                    } else {
                        Expr::Ident(tok.lexeme.to_owned())
                    }
                }
                TokenType::LBracket => {
                    let ls = self.list(
                        (None, TokenType::RBracket),
                        TokenType::Comma,
                        |s| s.expr_bp(0),
                    )?;

                    Expr::Array(ls)
                }
                _ => {
                    return Err(ParseError::ExpectedLiteral(
                        "number".to_owned(),
                    )
                    .into())
                }
            },
            None => {
                return Err(
                    ParseError::ExpectedLiteral("number".to_owned()).into()
                )
            }
        };

        while let Some(op) = self.0.peek().map(|x| x.variant) {
            let op = match op {
                TokenType::Op(o) if o.is_inf_op() => o,
                TokenType::RParen
                | TokenType::Comma
                | TokenType::RBracket
                | TokenType::RCurly
                | TokenType::Semicolon => break,
                _ => return Err(ParseError::ExpectedInfixOp.into()),
            };

            let (l_bp, r_bp) = infix_binding_power(op);

            if l_bp < min_bp {
                break;
            }

            self.0.next();
            let rhs = self.expr_bp(r_bp)?;

            lhs = Expr::BinOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }
        }

        Ok(lhs)
    }
}

fn infix_binding_power(op: Operator) -> (u8, u8) {
    use Operator::*;

    if !op.is_inf_op() {
        panic!("Expected infix op")
    }

    match op {
        Power => (9, 10),
        Times | Slash => (7, 8),
        Plus | Minus => (5, 6),
        DoubleEqual | NotEqual | Greater | GreaterEqual | Lesser
        | LesserEqual => (3, 4),
        DoublePipe | DoubleAnd => (1, 2),
        _ => unreachable!(),
    }
}

fn prefix_binding_power(op: Operator) -> ((), u8) {
    match op {
        Operator::PiTimes
        | Operator::Minus
        | Operator::Sqrt
        | Operator::Not => ((), 11),
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
    fn fun_declaration() {
        assert_parse(
            "fun foo(p1, p2, p3) {}",
            Ast::FunDecl {
                name: "foo".to_owned(),
                params: vec!["p1".to_owned(), "p2".to_owned(), "p3".to_owned()],
                body: Box::new(Ast::Block(vec![])),
            },
        )
    }

    #[test]
    fn fun_declaration_empty() {
        assert_parse(
            "fun foo() {}",
            Ast::FunDecl {
                name: "foo".to_owned(),
                params: vec![],
                body: Box::new(Ast::Block(vec![])),
            },
        )
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
