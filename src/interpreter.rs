use crate::{
    error::{InterpreterError, KoanError},
    lexer::Operator,
    parser::Expr,
};

use core::f64;
use std::ops::{Add, Div, Mul, Sub};

#[derive(Debug)]
pub enum Value {
    Num(f64),
    UTF8(String),
}

const COMPARISON_TOLERANCE: f64 = 1e-14;

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Num(l), Value::Num(r)) => {
                let max = l.abs().max(r.abs());
                let ct = COMPARISON_TOLERANCE * max;

                (l - r).abs() <= ct
            },
            (Value::UTF8(l), Value::UTF8(r)) => l == r,
            _ => false,
        }
    }
}

impl Value {
    fn ty_str(&self) -> &'static str {
        match self {
            Value::Num(_) => "number",
            Value::UTF8(_) => "string",
        }
    }
}

impl Expr {
    pub fn eval(self) -> Result<Value, KoanError> {
        match self {
            Expr::BinOp { lhs, op, rhs } if op.is_inf_op() => match op {
                Operator::Plus => lhs.eval()? + rhs.eval()?,
                Operator::Minus => lhs.eval()? - rhs.eval()?,
                Operator::Times => lhs.eval()? * rhs.eval()?,
                Operator::Slash => lhs.eval()? / rhs.eval()?,
                Operator::DoubleEqual => Ok(Value::Num((lhs.eval()? == rhs.eval()?) as u8 as f64)),
                Operator::Greater
                | Operator::GreaterEqual
                | Operator::Lesser
                | Operator::LesserEqual => {
                    let lhs = lhs.eval()?;
                    let rhs = rhs.eval()?;

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
                Operator::PiTimes => rhs.eval()? * Value::Num(f64::consts::PI),
                _ => unreachable!(),
            },
            Expr::Ident(_) => todo!(),
            Expr::NumLit(n) => Ok(Value::Num(n)),
            Expr::BinOp { .. } => unreachable!(),
            Expr::PreOp { .. } => unreachable!(),
        }
    }
}

impl Add for Value {
    type Output = Result<Value, KoanError>;

    fn add(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Value::Num(l), Value::Num(r)) => Value::Num(l + r),
            (Value::UTF8(_), Value::UTF8(_)) => todo!(),
            (l, r) => {
                return Err(InterpreterError::MismatchedTypes(
                    Operator::Plus,
                    l.ty_str(),
                    r.ty_str(),
                )
                .into())
            }
        })
    }
}

impl Sub for Value {
    type Output = Result<Value, KoanError>;

    fn sub(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Value::Num(l), Value::Num(r)) => Value::Num(l - r),
            (l, r) => {
                return Err(InterpreterError::MismatchedTypes(
                    Operator::Minus,
                    l.ty_str(),
                    r.ty_str(),
                )
                .into())
            }
        })
    }
}

impl Mul for Value {
    type Output = Result<Value, KoanError>;

    fn mul(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Value::Num(l), Value::Num(r)) => Value::Num(l * r),
            (Value::Num(l), Value::UTF8(r)) | (Value::UTF8(r), Value::Num(l)) => {
                let l = l.floor().abs() as usize;

                Value::UTF8(r.repeat(l))
            }
            (l, r) => {
                return Err(InterpreterError::MismatchedTypes(
                    Operator::Times,
                    l.ty_str(),
                    r.ty_str(),
                )
                .into())
            }
        })
    }
}

impl Div for Value {
    type Output = Result<Value, KoanError>;

    fn div(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Value::Num(l), Value::Num(r)) => {
                if r == 0.0 {
                    return Err(InterpreterError::DivByZero.into());
                }

                Value::Num(l / r)
            }
            (l, r) => {
                return Err(InterpreterError::MismatchedTypes(
                    Operator::Slash,
                    l.ty_str(),
                    r.ty_str(),
                )
                .into())
            }
        })
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Num(l), Value::Num(r)) => l.partial_cmp(r),
            _ => None,
        }
    }
}
