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
                Operator::Power => {
                    let l = lhs.eval(s)?;
                    let r = rhs.eval(s)?;

                    match (l, r) {
                        (Value::Num(ln), Value::Num(rn)) => Ok(Value::Num(ln.powf(rn))),
                        (l, r) => Err(InterpreterError::MismatchedTypes(
                            Operator::Power,
                            l.ty_str(),
                            r.ty_str(),
                        )
                        .into()),
                    }
                }
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
                Operator::Sqrt => {
                    let res = rhs.eval(s)?;
                    match res {
                        Value::Num(n) => Ok(Value::Num(n.sqrt())),
                        invalid => Err(InterpreterError::MismatchedUnOp(
                            Operator::Sqrt,
                            invalid.ty_str(),
                        )
                        .into()),
                    }
                }

                _ => unreachable!(),
            },
            Expr::FunCall(name, params) => match name.as_str() {
                "print" => {
                    for p in params {
                        print!("{} ", p.eval(s)?);
                    }
                    println!();

                    Ok(Value::Nothing)
                }
                _ => todo!(),
            },
            Expr::Ident(ident) => s
                .get(&ident)
                .ok_or_else(|| InterpreterError::UndefVar(ident).into()),
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
                s.set(ident, v);
                Ok(Value::Nothing)
            }
            Ast::Statement(e) => {
                let _ = e.eval(s)?;
                Ok(Value::Nothing)
            }
            Ast::Block(mut b) => {
                // Enter new scope
                s.variables.push(HashMap::new());

                let last = b.pop();

                for node in b {
                    node.eval(s)?;
                }

                Ok(match last {
                    Some(a) => {
                        let res = a.eval(s)?;
                        s.variables.pop();

                        res
                    }
                    None => Value::Nothing,
                })
            }
            Ast::FunDecl { name, params, body } => todo!(),
        }
    }
}
