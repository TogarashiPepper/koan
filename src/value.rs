use std::ops::{Add, Div, Mul, Sub};

use crate::{
    error::{InterpreterError, KoanError},
    lexer::Operator,
};

const COMPARISON_TOLERANCE: f64 = 1e-14;

#[derive(Debug, Clone)]
pub enum Value {
    Num(f64),
    UTF8(String),
    Nothing,
}

impl Value {
    fn ty_str(&self) -> &'static str {
        match self {
            Value::Num(_) => "number",
            Value::UTF8(_) => "string",
            Value::Nothing => "nothing",
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Num(n) => write!(f, "{}", n),
            Value::UTF8(s) => write!(f, "{}", s),
            Value::Nothing => write!(f, "nothing"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Num(l), Value::Num(r)) => {
                let max = l.abs().max(r.abs());
                let ct = COMPARISON_TOLERANCE * max;

                (l - r).abs() <= ct
            }
            (Value::UTF8(l), Value::UTF8(r)) => l == r,
            (Value::Nothing, Value::Nothing) => true,
            _ => false,
        }
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

impl Add for Value {
    type Output = Result<Value, KoanError>;

    fn add(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Value::Num(l), Value::Num(r)) => Value::Num(l + r),
            (Value::UTF8(l), Value::UTF8(r)) => Value::UTF8(format!("{l}{r}")),
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
