use crate::lexer::Operator;

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
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExprRef(usize);

#[derive(Debug, PartialEq, Clone)]
pub struct ExprPool(Vec<Expr>);

impl ExprPool {
    pub fn new() -> Self {
        ExprPool(vec![])
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
