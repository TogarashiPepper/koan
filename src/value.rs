use std::{
    fmt::{Display, Write},
    ops::{Add, Div, Mul, Neg, Not, Sub},
    rc::Rc, str::FromStr,
};

use crate::{
    error::{InterpError, KoanError, ParseError, Result},
    lexer::Operator,
};

const COMPARISON_TOLERANCE: f64 = 1e-14;

#[derive(Debug, Clone)]
pub enum Value {
    Num(f64),
    UTF8(String),
    Array(Rc<Vec<Value>>),
    Nothing,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValTy {
    Number,
    String,
    Array,
    Nothing,
}

impl FromStr for ValTy {
    type Err = KoanError;

    fn from_str(s: &str) -> Result<Self> {
        Ok(match s {
            "number" => Self::Number,
            "string" => Self::String,
            "array" => Self::Array,

            _ => return Err(ParseError::InvalidType(s.to_owned()).into())
        })
    }
}

impl Display for ValTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res = match self {
            ValTy::Number => "number",
            ValTy::String => "string",
            ValTy::Array => "array",
            ValTy::Nothing => "nothing",
        };

        write!(f, "{}", res)
    }
}

impl Value {
    pub fn ty_str(&self) -> ValTy {
        match self {
            Value::Num(_) => ValTy::Number,
            Value::UTF8(_) => ValTy::String,
            Value::Nothing => ValTy::Nothing,
            Value::Array(_) => ValTy::Array,
        }
    }

    pub fn as_num(&self, fn_name: &str) -> Result<f64> {
        match self {
            Value::Num(n) => Ok(*n),
            t => Err(InterpError::InvalidParamTy(fn_name.to_owned(), t.ty_str()).into()),
        }
    }

    /// Similar to option.expect or map, tries to apply the value to the Num variant, erroring if
    /// its a different variant
    pub fn in_num<F>(&self, fn_name: &str, f: F) -> Result<Value>
    where
        F: FnOnce(f64) -> f64,
    {
        match self {
            Value::Num(n) => Ok(Value::Num(f(*n))),
            t => Err(InterpError::InvalidParamTy(fn_name.to_owned(), t.ty_str()).into()),
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

    pub fn zip<F>(&self, rhs: Rc<Vec<Value>>, mut op: F) -> Result<Self>
    where
        F: FnMut(Value, Value) -> Result<Value>,
    {
        match self {
            Value::Array(left) => {
                if left.len() != rhs.len() {
                    return Err(InterpError::BinOpArrInvalidLength.into());
                }

                let mut res = vec![];
                for (l, r) in left.iter().zip(rhs.iter()) {
                    res.push(op(l.clone(), r.clone())?);
                }

                Ok(Value::Array(Rc::new(res)))
            }
            _ => unreachable!(),
        }
    }

    pub fn sqrt(&self) -> Result<Self> {
        match self {
            Value::Num(n) => Ok(Value::Num(n.sqrt())),
            a @ Value::Array(_) => a.map(|x| x.sqrt()),
            invalid => {
                Err(InterpError::MismatchedUnOp(Operator::Sqrt, invalid.ty_str()).into())
            }
        }
    }

    pub fn pow(self, r: Value) -> Result<Self> {
        match (self, r) {
            (Value::Num(ln), Value::Num(rn)) => Ok(Value::Num(ln.powf(rn))),
            (a @ Value::Array(_), n @ Value::Num(_)) => a.map(|x| x.pow(n.clone())),
            (n @ Value::Num(_), a @ Value::Array(_)) => a.map(|x| n.clone().pow(x)),
            (l @ Value::Array(_), Value::Array(r)) => l.zip(r, Value::pow),
            (l, r) => {
                Err(
                    InterpError::MismatchedTypes(Operator::Power, l.ty_str(), r.ty_str())
                        .into(),
                )
            }
        }
    }

    pub fn abs(&self) -> Result<Self> {
        match self {
            Value::Num(n) => Ok(Value::Num(n.abs())),
            a @ Value::Array(_) => a.map(|x| x.abs()),
            invalid => {
                Err(InterpError::MismatchedUnOp(Operator::Abs, invalid.ty_str()).into())
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
            (Value::Array(l), Value::Array(r)) => l == r,
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
            (l @ Value::Array(_), Value::Array(r)) => l.zip(r, Value::add)?,
            (ls @ Value::Array(_), r) => {
                return ls.map(|l| l + r.clone());
            }
            (l, rs @ Value::Array(_)) => {
                return rs.map(|r| l.clone() + r);
            }
            (l, r) => {
                return Err(InterpError::MismatchedTypes(
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
            (l @ Value::Array(_), Value::Array(r)) => l.zip(r, Value::sub)?,
            (ls @ Value::Array(_), r) => {
                return ls.map(|l| l - r.clone());
            }
            (l, rs @ Value::Array(_)) => {
                return rs.map(|r| l.clone() - r);
            }
            (l, r) => {
                return Err(InterpError::MismatchedTypes(
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
            (l @ Value::Array(_), Value::Array(r)) => l.zip(r, Value::mul)?,
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
                return Err(InterpError::MismatchedTypes(
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
            (l @ Value::Array(_), Value::Array(r)) => l.zip(r, Value::mul)?,
            (Value::Num(l), Value::Num(r)) => {
                if r == 0.0 {
                    return Err(InterpError::DivByZero.into());
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
                return Err(InterpError::MismatchedTypes(
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
            t => Err(InterpError::MismatchedUnOp(Operator::Minus, t.ty_str()).into()),
        }
    }
}

impl Not for Value {
    type Output = Result<Value>;

    fn not(self) -> Self::Output {
        match self {
            n @ Value::Num(_) => Ok(if n == Value::Num(0.0) {
                Value::Num(1.0)
            } else {
                Value::Num(0.0)
            }),
            a @ Value::Array(_) => a.map(Value::not),
            v => Err(InterpError::MismatchedUnOp(Operator::Not, v.ty_str()).into()),
        }
    }
}
