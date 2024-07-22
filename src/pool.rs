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

