use std::{
    fmt::Write,
    ops::{Add, Div, Mul, Neg, Sub},
    rc::Rc,
};

use crate::{error::InterpreterError, lexer::Operator, Result};

const COMPARISON_TOLERANCE: f64 = 1e-14;

#[derive(Debug, Clone)]
pub enum Value {
    Num(f64),
    UTF8(String),
    Array(Rc<Vec<Value>>),
    Nothing,
}

impl Value {
    pub fn ty_str(&self) -> &'static str {
        match self {
            Value::Num(_) => "number",
            Value::UTF8(_) => "string",
            Value::Nothing => "nothing",
            Value::Array(_) => "array",
        }
    }

    pub fn map<F>(&self, mut f: F) -> Result<Self>
    where
        F: FnMut(Value) -> Result<Value>,
    {
        match self {
            Value::Array(ls) => {
                let res = ls
                    .iter()
                    .map(|x| f(x.clone()))
                    .collect::<Result<Vec<Self>>>()?;

                Ok(Value::Array(res.into()))
            }
            x => f(x.clone()),
        }
    }

    pub fn sqrt(&self) -> Result<Self> {
        match self {
            Value::Num(n) => Ok(Value::Num(n.sqrt())),
            a @ Value::Array(_) => a.map(|x| x.sqrt()),
            invalid => {
                Err(InterpreterError::MismatchedUnOp(Operator::Sqrt, invalid.ty_str()).into())
            }
        }
    }

    pub fn pow(self, r: Value) -> Result<Self> {
        match (self, r) {
            (Value::Num(ln), Value::Num(rn)) => Ok(Value::Num(ln.powf(rn))),
            (a @ Value::Array(_), n @ Value::Num(_)) => a.map(|x| x.pow(n.clone())),
            (n @ Value::Num(_), a @ Value::Array(_)) => a.map(|x| n.clone().pow(x)),
            (l, r) => {
                Err(
                    InterpreterError::MismatchedTypes(Operator::Power, l.ty_str(), r.ty_str())
                        .into(),
                )
            }
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Num(n) => write!(f, "{}", n),
            Value::UTF8(s) => write!(f, "{:?}", s),
            Value::Nothing => write!(f, "nothing"),
            Value::Array(v) => {
                let mut buf = String::new();
                write!(buf, "[")?;

                for x in v.iter() {
                    write!(buf, "{x}, ")?;
                }

                buf.truncate(buf.len() - 2);
                write!(buf, "]")?;
                write!(f, "{}", buf)
            }
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
    type Output = Result<Value>;

    fn add(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Value::Num(l), Value::Num(r)) => Value::Num(l + r),
            (Value::UTF8(l), Value::UTF8(r)) => Value::UTF8(format!("{l}{r}")),
            (ls @ Value::Array(_), r) => {
                return ls.map(|l| l + r.clone());
            }
            (l, rs @ Value::Array(_)) => {
                return rs.map(|r| l.clone() + r);
            }
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
    type Output = Result<Value>;

    fn sub(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Value::Num(l), Value::Num(r)) => Value::Num(l - r),
            (ls @ Value::Array(_), r) => {
                return ls.map(|l| l - r.clone());
            }
            (l, rs @ Value::Array(_)) => {
                return rs.map(|r| l.clone() - r);
            }
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
    type Output = Result<Value>;

    fn mul(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Value::Num(l), Value::Num(r)) => Value::Num(l * r),
            (Value::Num(l), Value::UTF8(r)) | (Value::UTF8(r), Value::Num(l)) => {
                let l = l.floor().abs() as usize;

                Value::UTF8(r.repeat(l))
            }
            (ls @ Value::Array(_), r) => {
                return ls.map(|l| l * r.clone());
            }
            (l, rs @ Value::Array(_)) => {
                return rs.map(|r| l.clone() * r);
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
    type Output = Result<Value>;

    fn div(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Value::Num(l), Value::Num(r)) => {
                if r == 0.0 {
                    return Err(InterpreterError::DivByZero.into());
                }

                Value::Num(l / r)
            }
            (ls @ Value::Array(_), r) => {
                return ls.map(|l| l / r.clone());
            }
            (l, rs @ Value::Array(_)) => {
                return rs.map(|r| l.clone() / r);
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

impl Neg for Value {
    type Output = Result<Value>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Num(n) => Ok(Value::Num(-n)),
            a @ Value::Array(_) => a.map(Neg::neg),
            t => Err(InterpreterError::MismatchedUnOp(Operator::Minus, t.ty_str()).into()),
        }
    }
}
