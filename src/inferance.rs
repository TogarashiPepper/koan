use crate::{
    lexer::Operator,
    parser::Ast,
    pool::{Expr, ExprPool, ExprRef},
    value::ValTy,
    error::Result,
};

/// Performs type inference on the provided Ast
pub fn infer(ast: Ast, pool: ExprPool) -> Ast {
    ast
}

pub fn infer_exp(expr: ExprRef, pool: &ExprPool) -> Result<ValTy> {
    let exp = pool.get(expr);

    Ok(match exp {
        Expr::BinOp { lhs, op, rhs } => {
            match op {
                Operator::Power => todo!(),
                Operator::Plus => todo!(),
                Operator::Minus => todo!(),
                Operator::Times => {
                    match (infer_exp(lhs, pool), infer_exp(rhs, pool)) {
                        _ => todo!()
                    }
                },
                Operator::Slash
                | Operator::DoubleEqual
                | Operator::Greater
                | Operator::GreaterEqual
                | Operator::Lesser
                | Operator::LesserEqual
                | Operator::NotEqual
                | Operator::DoublePipe
                | Operator::DoubleAnd => ValTy::Number,

                _ => unreachable!(),
            }
        }
        Expr::PreOp { op, rhs } => todo!(),
        Expr::Ident(_) => todo!(),
        Expr::NumLit(_) => ValTy::Number,
        Expr::StrLit(_) => ValTy::String,
        Expr::FunCall(_, _) => todo!(),
        Expr::Array(_) => ValTy::Array,
        Expr::IfElse {
            cond,
            body,
            else_body,
        } => todo!(),
    })
}
