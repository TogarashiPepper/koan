use crate::{
    error::{InterpreterError, KoanError},
    lexer::Operator,
    parser::{Ast, Expr},
    state::State,
    value::Value,
};

use core::f64;
use std::collections::HashMap;

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
                Operator::Minus => {
                    let res = rhs.eval(s)?;
                    -res
                }
                _ => unreachable!(),
            },
            Expr::FunCall(name, params) => todo!(),
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
            Ast::Statement(e) => {
                let _ = e.eval(s)?;
                Ok(Value::Nothing)
            }
            Ast::Block(mut b) => {
                let last = b.pop();

                // TODO: make this not absolutely terrible, atm its a performance nightmare
                // TODO: AND it doesn't work for variable reassignment (once that becomes a thing)
                // TODO: potential solutions include forcing a let .. in kind of FP-like structure
                // TODO: or perhaps some kind of more complex scoping solution, learning towards
                // TODO: former ATM
                let mut block_state = State {
                    variables: s.variables.clone(),
                };

                for node in b {
                    node.eval(&mut block_state)?;
                }

                Ok(match last {
                    Some(a) => a.eval(&mut block_state)?,
                    None => Value::Nothing,
                })
            }
            Ast::FunDecl { name, params, body } => todo!(),
        }
    }
}
