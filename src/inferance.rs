use std::collections::HashMap;

use crate::{
    error::{InterpError, Result}, lexer::Operator, parser::Ast, pool::{Expr, ExprPool, ExprRef}, state::Function, value::ValTy
};


#[derive(Debug)]
pub struct StateSim {
    // Array of environments, index = depth (i.e. `0` = global scope)
    pub variables: Vec<HashMap<String, Option<ValTy>>>,
    pub functions: HashMap<String, Function>,
}

/// Performs type inference on the provided Ast
pub fn infer(ast: Ast, pool: ExprPool) -> Ast {
    ast
}

pub fn infer_exp(expr: ExprRef, pool: &ExprPool) -> Result<ValTy> {
    use ValTy::{Array, Number, String};

    let exp = pool.get(expr);

    Ok(match exp {
        Expr::BinOp { lhs, op, rhs } => {
            let lhs = infer_exp(*lhs, pool)?;
            let rhs = infer_exp(*rhs, pool)?;

            match op {
                Operator::Power => match (lhs, rhs) {
                    (Array, Array) | (Number, Array) | (Array, Number) => Array,
                    (Number, Number) => Number,

                    (l, r) => {
                        return Err(InterpError::MismatchedTypes(
                            Operator::Power,
                            l,
                            r,
                        )
                        .into())
                    }
                },
                Operator::Plus => match (lhs, rhs) {
                    (Number, Number) => Number,
                    (Array, Number) | (Number, Array) | (Array, Array) => Array,
                    (String, String) => String,

                    (l, r) => {
                        return Err(InterpError::MismatchedTypes(
                            Operator::Plus,
                            l,
                            r,
                        )
                        .into())
                    }
                },
                Operator::Minus => match (lhs, rhs) {
                    (Number, Number) => Number,
                    (Number, Array) | (Array, Number) | (Array, Array) => Array,

                    (l, r) => {
                        return Err(InterpError::MismatchedTypes(
                            Operator::Minus,
                            l,
                            r,
                        )
                        .into())
                    }
                },
                Operator::Times => match (lhs, rhs) {
                    (Number, Array) | (Array, Number) | (Array, Array) => Array,
                    (String, Number) | (Number, String) => String,
                    (Number, Number) => Number,

                    (l, r) => {
                        return Err(InterpError::MismatchedTypes(
                            Operator::Times,
                            l,
                            r,
                        )
                        .into())
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
                | Operator::DoubleAnd => Number,

                _ => unreachable!(),
            }
        }
        Expr::PreOp { op: _, rhs } => {
            let rhs = infer_exp(*rhs, pool)?;
            match rhs {
                Number => Number,
                Array => Array,

                r => {
                    return Err(InterpError::MismatchedUnOp(
                        Operator::PiTimes,
                        r,
                    )
                    .into())
                }
            }
        }
        Expr::NumLit(_) => Number,
        Expr::StrLit(_) => String,
        Expr::Array(_) => Array,
        Expr::Ident(_) => todo!(),
        Expr::FunCall(_, _) => todo!(),
        Expr::IfElse {
            cond,
            body,
            else_body,
        } => todo!(),
    })
}
