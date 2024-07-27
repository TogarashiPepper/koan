mod utils;

use std::iter::Peekable;

use crate::{
    error::{ParseError, Result},
    lexer::{Operator, Token, TokenType},
    pool::{Expr, ExprPool, ExprRef},
    value::ValTy,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Ast {
    Expression(ExprRef),
    Statement(ExprRef),
    Block(Vec<Ast>),
    LetDecl {
        name: String,
        ty: Option<ValTy>,
        body: ExprRef,
    },
    FunDecl {
        name: String,
        params: Vec<(String, ValTy)>,
        body: Box<Ast>,
    },
}

pub fn parse(tokens: Vec<Token<'_>>) -> Result<(Vec<Ast>, ExprPool)> {
    let mut pool = ExprPool::with_capacity(200);
    let res = parse_with_pool(tokens, &mut pool)?;

    Ok((res, pool))
}

pub fn parse_with_pool(
    tokens: Vec<Token<'_>>,
    pool: &mut ExprPool,
) -> Result<Vec<Ast>> {
    let mut it = TokenStream {
        it: tokens.into_iter().peekable(),
        pool,
    };

    it.program(false)
}

pub struct TokenStream<'a, T: Iterator<Item = Token<'a>>> {
    pub it: Peekable<T>,
    pub pool: &'a mut ExprPool,
}

impl<'a, T: Iterator<Item = Token<'a>>> TokenStream<'a, T> {
    /// in_block parameter tells the program parser to error, or simply return the ast when it
    /// encounters a `}` character
    pub fn program(&mut self, in_block: bool) -> Result<Vec<Ast>> {
        let mut program = vec![];

        while let Some(p) = self.it.peek() {
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
                            self.it.next();
                            Ast::Statement(expr)
                        }
                        false => Ast::Expression(expr),
                    }
                }
            });
        }

        Ok(program)
    }

    pub fn block(&mut self) -> Result<Ast> {
        let _ = self.expect(TokenType::LCurly)?;
        let block = self.program(true)?;
        let _ = self.expect(TokenType::RCurly)?;

        Ok(Ast::Block(block))
    }

    pub fn fun_def(&mut self) -> Result<Ast> {
        let [_, ident] =
            self.multi_expect(&[TokenType::Fun, TokenType::Ident])?;
        // TODO: parse type annotations
        let params = self.list(
            (Some(TokenType::LParen), TokenType::RParen),
            TokenType::Comma,
            // list method already peaked b4 calling
            |stream| stream.expect(TokenType::Ident),
        )?;
        let block = self.block()?;

        Ok(Ast::FunDecl {
            name: ident.lexeme.to_owned(),
            params: params
                .into_iter()
                .map(|t| (t.lexeme.to_string(), ValTy::Number))
                .collect(),
            body: Box::new(block),
        })
    }

    pub fn fun_call(&mut self, ident: String) -> Result<ExprRef> {
        let params = self.list(
            (Some(TokenType::LParen), TokenType::RParen),
            TokenType::Comma,
            |stream| stream.expr_bp(0),
        )?;

        Ok(self.pool.push(Expr::FunCall(ident, params)))
    }

    pub fn let_decl(&mut self) -> Result<Ast> {
        let [_, ident, _] = self.multi_expect(&[
            TokenType::Let,
            TokenType::Ident,
            TokenType::Op(Operator::Equal),
        ])?;
        let body = self.expr_bp(0)?;
        let _ = self.expect(TokenType::Semicolon)?;

        Ok(Ast::LetDecl {
            name: ident.lexeme.to_owned(),
            body,
            ty: todo!(),
        })
    }
}

pub fn infix_binding_power(op: Operator) -> (u8, u8) {
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

pub fn prefix_binding_power(op: Operator) -> ((), u8) {
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

    fn assert_parse_expr(input: &'static str, expected: ExprRef) {
        assert_parse(input, Ast::Expression(expected));
    }

    fn assert_parse(input: &'static str, expected: Ast) {
        // For the sake of testing the parser, we'll assume the lexer is correct
        let tokens = lex(input).unwrap();
        let (ast, _pool) = match parse(tokens) {
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
                params: vec![
                    ("p1".to_owned(), ValTy::Number),
                    ("p2".to_owned(), ValTy::Number),
                    ("p3".to_owned(), ValTy::Number),
                ],
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

    // TODO:
    // #[test]
    // fn let_declaration() {
    //     assert_parse(
    //         "let x = 10;",
    //         Ast::LetDecl("x".to_string(), Expr::NumLit(10.0)),
    //     );
    // }
    //
    // #[test]
    // fn let_decl_then_expr() {
    //     assert_parse(
    //         "let x = 1 + 2;",
    //         Ast::LetDecl(
    //             "x".to_string(),
    //             Expr::BinOp {
    //                 lhs: Box::new(Expr::NumLit(1.0)),
    //                 op: Operator::Plus,
    //                 rhs: Box::new(Expr::NumLit(2.0)),
    //             },
    //         ),
    //     )
    // }
}
