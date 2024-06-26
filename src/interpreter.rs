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

                    l.pow(r)
                }
                Operator::DoubleEqual => {
                    Ok(Value::Num((lhs.eval(s)? == rhs.eval(s)?) as u8 as f64))
                }
                Operator::NotEqual => Ok(Value::Num((lhs.eval(s)? != rhs.eval(s)?) as u8 as f64)),
                Operator::Greater
                | Operator::GreaterEqual
                | Operator::Lesser
                | Operator::LesserEqual => {
                    use std::cmp::PartialOrd;

                    let lhs = lhs.eval(s)?;
                    let rhs = rhs.eval(s)?;

                    let op = match op {
                        Operator::Greater => PartialOrd::gt,
                        Operator::GreaterEqual => PartialOrd::ge,
                        Operator::Lesser => PartialOrd::lt,
                        Operator::LesserEqual => PartialOrd::le,
                        _ => unreachable!(),
                    };

                    match (lhs, rhs) {
                        (ls @ Value::Array(_), r @ Value::Num(_)) => {
                            ls.map(|l| Ok(Value::Num(op(&l, &r) as u8 as f64)))
                        }
                        (l @ Value::Num(_), rs @ Value::Array(_)) => {
                            rs.map(|r| Ok(Value::Num(op(&l, &r) as u8 as f64)))
                        }
                        (l, r) => Ok(Value::Num(op(&l, &r) as u8 as f64)),
                    }
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

                    res.sqrt()
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
            Expr::Array(a) => Ok(Value::Array(
                a.into_iter()
                    .map(|x| x.eval(s))
                    .collect::<Result<Vec<Value>, KoanError>>()?
                    .into(),
            )),
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
