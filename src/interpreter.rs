use crate::{
    error::{InterpreterError, KoanError},
    lexer::Operator,
    parser::{Ast, Expr},
    value::Value,
};

use core::f64;
use std::collections::HashMap;

#[derive(Debug)]
pub struct State {
    pub variables: HashMap<String, Value>,
}

impl Expr {
    pub fn eval(self, s: &mut State) -> Result<Value, KoanError> {
        match self {
            Expr::BinOp { lhs, op, rhs } if op.is_inf_op() => match op {
                Operator::Plus => lhs.eval(s)? + rhs.eval(s)?,
                Operator::Minus => lhs.eval(s)? - rhs.eval(s)?,
                Operator::Times => lhs.eval(s)? * rhs.eval(s)?,
                Operator::Slash => lhs.eval(s)? / rhs.eval(s)?,
                Operator::DoubleEqual => {
                    Ok(Value::Num((lhs.eval(s)? == rhs.eval(s)?) as u8 as f64))
                }
                Operator::Greater
                | Operator::GreaterEqual
                | Operator::Lesser
                | Operator::LesserEqual => {
                    let lhs = lhs.eval(s)?;
                    let rhs = rhs.eval(s)?;

                    let r = match op {
                        Operator::Greater => lhs > rhs,
                        Operator::GreaterEqual => lhs >= rhs,
                        Operator::Lesser => lhs < rhs,
                        Operator::LesserEqual => lhs <= rhs,
                        _ => unreachable!(),
                    };

                    Ok(Value::Num(r as u8 as f64))
                }
                _ => unreachable!(),
            },
            Expr::PreOp { op, rhs } if op.is_pre_op() => match op {
                Operator::PiTimes => rhs.eval(s)? * Value::Num(f64::consts::PI),
                _ => unreachable!(),
            },
            Expr::Ident(ident) => s
                .variables
                .get(&ident)
                .ok_or_else(|| InterpreterError::UndefVar(ident).into())
                .cloned(),
            Expr::StrLit(s) => Ok(Value::UTF8(s)),
            Expr::NumLit(n) => Ok(Value::Num(n)),
            Expr::BinOp { .. } => unreachable!(),
            Expr::PreOp { .. } => unreachable!(),
        }
    }
}

impl Ast {
    pub fn eval(self, s: &mut State) -> Result<Value, KoanError> {
        match self {
            Ast::Expression(e) => e.eval(s),
            Ast::LetDecl(ident, body) => {
                let v = body.eval(s)?;
                s.variables.insert(ident, v);
                Ok(Value::Nothing)
            }
        }
    }
}
