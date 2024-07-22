use crate::{
    error::{ParseError, Result},
    lexer::{Operator, Token, TokenType},
    parser::{self, infix_binding_power, TokenStream},
    pool::{Expr, ExprRef},
};

impl<'a, T: Iterator<Item = Token<'a>>> TokenStream<'a, T> {
    pub fn expr_bp(&mut self, min_bp: u8) -> Result<ExprRef> {
        let mut lhs = match self.it.next() {
            Some(tok) => match tok.variant {
                TokenType::Number => {
                    if self.check(TokenType::Ident) {
                        let ident = self.expect(TokenType::Ident)?;
                        let lhs = self
                            .pool
                            .push(Expr::NumLit(tok.lexeme.parse().unwrap()));
                        let rhs = self
                            .pool
                            .push(Expr::Ident(ident.lexeme.to_owned()));

                        self.pool.push(Expr::BinOp {
                            lhs,
                            op: Operator::Times,
                            rhs,
                        })
                    } else {
                        self.pool
                            .push(Expr::NumLit(tok.lexeme.parse().unwrap()))
                    }
                }

                TokenType::String => {
                    self.pool.push(Expr::StrLit(tok.lexeme.to_owned()))
                }
                TokenType::LParen => {
                    let lhs = self.expr_bp(0)?;
                    let _ = self.expect(TokenType::RParen)?;

                    lhs
                }
                TokenType::Pipe => {
                    let rhs = self.expr_bp(0)?;
                    let _ = self.expect(TokenType::Pipe)?;

                    self.pool.push(Expr::PreOp {
                        op: Operator::Abs,
                        rhs,
                    })
                }
                TokenType::Op(op) => {
                    let ((), r_bp) = parser::prefix_binding_power(op);
                    let rhs = self.expr_bp(r_bp)?;

                    self.pool.push(Expr::PreOp { op, rhs })
                }
                TokenType::Ident => {
                    if self.check(TokenType::LParen) {
                        self.fun_call(tok.lexeme.to_owned())?
                    } else {
                        self.pool.push(Expr::Ident(tok.lexeme.to_owned()))
                    }
                }
                TokenType::LBracket => {
                    let ls = self.list(
                        (None, TokenType::RBracket),
                        TokenType::Comma,
                        |s| s.expr_bp(0),
                    )?;

                    self.pool.push(Expr::Array(ls))
                }
                TokenType::If => {
                    let cond = self.expr_bp(0)?;
                    let body = self.block()?;

                    let mut else_body = None;

                    if self.check(TokenType::Else) {
                        let _ = self.expect(TokenType::Else)?;
                        else_body = Some(self.block()?);
                    }

                    self.pool.push(Expr::IfElse {
                        cond,
                        body,
                        else_body,
                    })
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

        while let Some(op) = self.it.peek().map(|x| x.variant) {
            let op = match op {
                TokenType::Op(o) if o.is_inf_op() => o,
                TokenType::RParen
                | TokenType::Comma
                | TokenType::RBracket
                | TokenType::LCurly
                | TokenType::RCurly
                | TokenType::Semicolon
                | TokenType::Pipe => break,
                _ => return Err(ParseError::ExpectedInfixOp.into()),
            };

            let (l_bp, r_bp) = infix_binding_power(op);

            if l_bp < min_bp {
                break;
            }

            self.it.next();
            let rhs = self.expr_bp(r_bp)?;

            lhs = self.pool.push(Expr::BinOp { lhs, op, rhs });
        }

        Ok(lhs)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        error::Result,
        lexer::{lex, Operator, Token},
        parser::TokenStream,
        pool::{Expr, ExprPool, ExprRef},
    };

    fn expr_eq(
        lhs: ExprRef,
        rhs: ExprRef,
        l_pool: &ExprPool,
        r_pool: &ExprPool,
    ) -> bool {
        let lhs = l_pool.get(lhs);
        let rhs = r_pool.get(rhs);

        match (lhs, rhs) {
            (
                Expr::BinOp {
                    lhs: ll,
                    op: lop,
                    rhs: lr,
                },
                Expr::BinOp {
                    lhs: rl,
                    op: rop,
                    rhs: rr,
                },
            ) => {
                lop == rop
                    && expr_eq(*ll, *rl, l_pool, r_pool)
                    && expr_eq(*lr, *rr, l_pool, r_pool)
            }
            (
                Expr::PreOp { op: lop, rhs: lr },
                Expr::PreOp { op: rop, rhs: rr },
            ) => lop == rop && expr_eq(*lr, *rr, l_pool, r_pool),
            (Expr::Ident(l), Expr::Ident(r)) => l == r,
            (Expr::NumLit(l), Expr::NumLit(r)) => l == r,
            (Expr::StrLit(l), Expr::StrLit(r)) => l == r,
            (Expr::FunCall(lname, lparams), Expr::FunCall(rname, rparams)) => {
                let it = lparams
                    .iter()
                    .zip(rparams)
                    .all(|(l, r)| expr_eq(*l, *r, l_pool, r_pool));

                lname == rname && lparams.len() == rparams.len() && it
            }
            (Expr::Array(larr), Expr::Array(rarr)) => {
                let it = larr
                    .iter()
                    .zip(rarr)
                    .all(|(l, r)| expr_eq(*l, *r, l_pool, r_pool));

                larr.len() == rarr.len() && it
            }

            _ => false,
        }
    }

    pub fn parse_expr(tokens: Vec<Token<'_>>) -> Result<(ExprPool, ExprRef)> {
        let mut pool = ExprPool::new();

        let mut it = TokenStream {
            it: tokens.into_iter().peekable(),
            pool: &mut pool,
        };

        let exp = it.expr_bp(0)?;
        Ok((pool, exp))
    }

    fn assert_expr<F: FnOnce(&mut ExprPool) -> ExprRef>(input: &str, func: F) {
        let (pool, exp_ref) = lex(input).and_then(parse_expr).unwrap();
        let mut expected_pool = ExprPool::new();
        let res = func(&mut expected_pool);

        assert!(expr_eq(exp_ref, res, &pool, &expected_pool));
    }

    #[test]
    fn funcall_nonempty() {
        assert_expr("print(1 + 2, 3)", |pool| {
            let binop = Expr::BinOp {
                lhs: pool.push(Expr::NumLit(1.0)),
                op: Operator::Plus,
                rhs: pool.push(Expr::NumLit(2.0)),
            };

            let exp = Expr::FunCall(
                "print".to_owned(),
                vec![pool.push(binop), pool.push(Expr::NumLit(3.0))],
            );

            pool.push(exp)
        })
    }

    #[test]
    fn simple_binop() {
        use Expr::*;

        assert_expr("1 + 2", |pool| {
            let lhs = pool.push(NumLit(1.0));
            let rhs = pool.push(NumLit(2.0));
            pool.push(Expr::BinOp {
                lhs,
                op: Operator::Plus,
                rhs,
            })
        })
    }

    #[test]
    fn simple_preop() {
        use Expr::*;

        assert_expr("○1", |pool| {
            let rhs = pool.push(NumLit(1.0));

            pool.push(PreOp {
                op: Operator::PiTimes,
                rhs,
            })
        })
    }

    #[test]
    fn mult_binop_no_prec() {
        use Expr::*;

        assert_expr("1 + 2 - 3", |pool| {
            let one = pool.push(NumLit(1.0));
            let two = pool.push(NumLit(2.0));
            let three = pool.push(NumLit(3.0));

            let plus_op = pool.push(BinOp {
                lhs: one,
                op: Operator::Plus,
                rhs: two,
            });

            pool.push(BinOp {
                lhs: plus_op,
                op: Operator::Minus,
                rhs: three,
            })
        })
    }

    #[test]
    fn paren_plus_times() {
        use Expr::*;

        assert_expr("(1 + 2) * 3", |pool| {
            let one = pool.push(NumLit(1.0));
            let two = pool.push(NumLit(2.0));
            let three = pool.push(NumLit(3.0));

            let plus_op = pool.push(BinOp {
                lhs: one,
                op: Operator::Plus,
                rhs: two,
            });

            pool.push(BinOp {
                lhs: plus_op,
                op: Operator::Times,
                rhs: three,
            })
        })
    }

    #[test]
    fn plus_times() {
        use Expr::*;

        assert_expr("1 + 2 * 3", |pool| {
            let one = pool.push(NumLit(1.0));
            let two = pool.push(NumLit(2.0));
            let three = pool.push(NumLit(3.0));

            let times_op = pool.push(BinOp {
                lhs: two,
                op: Operator::Times,
                rhs: three,
            });

            pool.push(BinOp {
                lhs: one,
                op: Operator::Plus,
                rhs: times_op,
            })
        })
    }
}
