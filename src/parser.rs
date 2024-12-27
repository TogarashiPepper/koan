mod utils;

use std::{
    collections::HashSet,
    iter::Peekable,
};

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
        ret: ValTy,
        body: Box<Ast>,
    },
}

impl Ast {
    pub fn ast_eq(l: Ast, l_pool: &ExprPool, r: Ast, r_pool: &ExprPool) -> bool {
        match (l, r) {
            (Ast::Expression(l), Ast::Expression(r))
            | (Ast::Statement(l), Ast::Statement(r)) => {
                Expr::expr_eq(l, r, l_pool, r_pool)
            }
            (Ast::Block(l), Ast::Block(r)) => {
                l.into_iter().zip(r).all(|(le, ri)| le == ri)
            }
            (
                Ast::LetDecl {
                    name: lname,
                    ty: lty,
                    body: lbody,
                },
                Ast::LetDecl { name, ty, body },
            ) => lname == name && lty == ty && Expr::expr_eq(lbody, body, l_pool, r_pool),
            (
                Ast::FunDecl {
                    name,
                    params,
                    ret,
                    body,
                },
                Ast::FunDecl {
                    name: rname,
                    params: rparams,
                    ret: rret,
                    body: rbody,
                },
            ) => {
                name == rname
                    && params == rparams
                    && ret == rret
                    && Ast::ast_eq(*body, l_pool, *rbody, r_pool)
            }

            _ => false,
        }
    }
}

pub fn parse(tokens: Vec<Token<'_>>) -> Result<(Vec<Ast>, ExprPool)> {
    let mut pool = ExprPool::with_capacity(200);
    let res = parse_with_pool(tokens, &mut pool)?;

    Ok((res, pool))
}

pub fn parse_with_pool(tokens: Vec<Token<'_>>, pool: &mut ExprPool) -> Result<Vec<Ast>> {
    let variables = HashSet::from(["π".to_owned(), "e".to_owned()]);

    let mut it = TokenStream {
        it: tokens.into_iter().peekable(),
        pool,
        variables,
    };

    it.program(false)
}

pub struct TokenStream<'a, T: Iterator<Item = Token<'a>>> {
    pub it: Peekable<T>,
    pub pool: &'a mut ExprPool,
    pub variables: HashSet<String>,
}

impl<'a, T: Iterator<Item = Token<'a>>> TokenStream<'a, T> {
    /// in_block parameter tells the program parser to error, or simply return the ast when it
    /// encounters a `}` character
    pub fn program(&mut self, in_block: bool) -> Result<Vec<Ast>> {
        let mut program = vec![];

        while let Some(p) = self.it.peek() {
            program.push(match p.variant {
                TokenType::Let => self.let_decl()?,
                TokenType::Fun => {
                    if !in_block {
                        self.fun_def()?
                    } else {
                        return Err(ParseError::FunctionNotTopLevel.into());
                    }
                }
                TokenType::RCurly => {
                    if !in_block {
                        return Err(ParseError::Unexpected(TokenType::RCurly).into());
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
        self.variables.clear();

        let [_, ident] = self.multi_expect(&[TokenType::Fun, TokenType::Ident])?;
        let params: Vec<(String, ValTy)> = self
            .list(
                (Some(TokenType::LParen), TokenType::RParen),
                TokenType::Comma,
                // list method already peaked b4 calling
                |stream| {
                    let ident = stream.expect(TokenType::Ident)?;
                    let mut ty = ValTy::Number;
                    if stream.check(TokenType::Colon) {
                        stream.expect(TokenType::Colon)?;

                        ty = stream.expect(TokenType::Ident)?.lexeme.parse()?;
                    }

                    Ok((ident.lexeme.to_owned(), ty))
                },
            )?
            .into_iter()
            .collect();

        dbg!(&params);

        for (name, _) in params.iter() {
            self.variables.insert(name.to_owned());
        }

        let mut ret_ty = ValTy::Number;
        if self.check(TokenType::Arrow) {
            ret_ty = self.expect(TokenType::Ident)?.lexeme.parse()?;
        }

        let block = self.block()?;

        self.variables.clear();

        Ok(Ast::FunDecl {
            name: ident.lexeme.to_owned(),
            params,
            ret: ret_ty,
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

    // TODO: parse let ascription type
    pub fn let_decl(&mut self) -> Result<Ast> {
        let [_, ident, _] = self.multi_expect(&[
            TokenType::Let,
            TokenType::Ident,
            TokenType::Op(Operator::Equal),
        ])?;
        let body = self.expr_bp(0)?;
        let _ = self.expect(TokenType::Semicolon)?;
        let ident = ident.lexeme.to_owned();

        if self.variables.contains(&ident) {
            return Err(ParseError::Shadowed(ident).into());
        } else {
            self.variables.insert(ident.clone());
        }

        Ok(Ast::LetDecl {
            name: ident,
            body,
            ty: None,
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
        DoubleEqual | NotEqual | Greater | GreaterEqual | Lesser | LesserEqual => (3, 4),
        DoublePipe | DoubleAnd => (1, 2),
        _ => unreachable!(),
    }
}

pub fn prefix_binding_power(op: Operator) -> ((), u8) {
    match op {
        Operator::PiTimes | Operator::Minus | Operator::Sqrt | Operator::Not => ((), 11),
        _ => panic!("Expected prefix operator, found some other token"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer::lex, parser::Ast, pool::ExprPool};

    fn assert_parse<F>(input: &'static str, func: F)
    where
        F: FnOnce(&mut ExprPool) -> Ast,
    {
        // For the sake of testing the parser, we'll assume the lexer is correct
        let mut ex_pool = ExprPool::new();
        let expected = func(&mut ex_pool);

        let tokens = lex(input).unwrap();
        let (ast, got_pool) = parse(tokens).unwrap();

        assert!(Ast::ast_eq(ast[0].clone(), &got_pool, expected, &ex_pool));
    }

    #[test]
    fn fun_declaration() {
        assert_parse("fun foo(p1, p2, p3) {}", |_| Ast::FunDecl {
            name: "foo".to_owned(),
            params: vec![
                ("p1".to_owned(), ValTy::Number),
                ("p2".to_owned(), ValTy::Number),
                ("p3".to_owned(), ValTy::Number),
            ],
            ret: ValTy::Number,
            body: Box::new(Ast::Block(vec![])),
        })
    }

    #[test]
    fn fun_declaration_empty() {
        assert_parse("fun foo() {}", |_| Ast::FunDecl {
            name: "foo".to_owned(),
            params: vec![],
            ret: ValTy::Number,
            body: Box::new(Ast::Block(vec![])),
        })
    }

    #[test]
    fn let_declaration() {
        assert_parse("let x = 10;", |pool| Ast::LetDecl {
            name: "x".to_owned(),
            ty: None,
            body: pool.push(Expr::NumLit(10.0)),
        });
    }

    #[test]
    fn let_decl_then_expr() {
        assert_parse("let x = 1 + 2;", |pool| {
            let lhs = pool.push(Expr::NumLit(1.0));
            let rhs = pool.push(Expr::NumLit(2.0));
            Ast::LetDecl {
                    name: "x".to_owned(),
                    ty: None,
                    body: pool.push(Expr::BinOp {
                        lhs,
                        op: Operator::Plus,
                        rhs,
                    }),
                }
        })
    }
}
