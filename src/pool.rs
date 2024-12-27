use crate::{lexer::Operator, parser::Ast};

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    // Convert this to BinOp(BinOp) for impls on it?
    BinOp {
        lhs: ExprRef,
        op: Operator,
        rhs: ExprRef,
    },
    PreOp {
        op: Operator,
        rhs: ExprRef,
    },
    Ident(String),
    // TODO: Add typed integer literals to avoid JS-esque strangeness
    NumLit(f64),
    StrLit(String),
    FunCall(String, Vec<ExprRef>),
    Array(Vec<ExprRef>),
    IfElse {
        cond: ExprRef,
        body: Ast,
        else_body: Option<Ast>,
    },
}

impl Expr {
    pub fn expr_eq(lhs: ExprRef, rhs: ExprRef, l_pool: &ExprPool, r_pool: &ExprPool) -> bool {
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
                    && Self::expr_eq(*ll, *rl, l_pool, r_pool)
                    && Self::expr_eq(*lr, *rr, l_pool, r_pool)
            }
            (Expr::PreOp { op: lop, rhs: lr }, Expr::PreOp { op: rop, rhs: rr }) => {
                lop == rop && Self::expr_eq(*lr, *rr, l_pool, r_pool)
            }
            (Expr::Ident(l), Expr::Ident(r)) => l == r,
            (Expr::NumLit(l), Expr::NumLit(r)) => l == r,
            (Expr::StrLit(l), Expr::StrLit(r)) => l == r,
            (Expr::FunCall(lname, lparams), Expr::FunCall(rname, rparams)) => {
                let it = lparams
                    .iter()
                    .zip(rparams)
                    .all(|(l, r)| Self::expr_eq(*l, *r, l_pool, r_pool));

                lname == rname && lparams.len() == rparams.len() && it
            }
            (Expr::Array(larr), Expr::Array(rarr)) => {
                let it = larr
                    .iter()
                    .zip(rarr)
                    .all(|(l, r)| Self::expr_eq(*l, *r, l_pool, r_pool));

                larr.len() == rarr.len() && it
            }

            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ExprRef(usize);

#[derive(Debug, PartialEq, Clone)]
pub struct ExprPool(pub Vec<Expr>);

impl ExprPool {
    pub fn new() -> Self {
        ExprPool(vec![])
    }

    pub fn with_capacity(n: usize) -> Self {
        ExprPool(Vec::with_capacity(n))
    }

    pub fn get(&self, exp_ref: ExprRef) -> &Expr {
        &self.0[exp_ref.0]
    }

    pub fn push(&mut self, expr: Expr) -> ExprRef {
        let idx = self.0.len();
        self.0.push(expr);

        ExprRef(idx)
    }
}

impl Default for ExprPool {
    fn default() -> Self {
        Self::new()
    }
}
