use std::collections::HashMap;

use crate::{
    error::{InterpreterError, Result},
    lexer::Operator,
    parser::{Ast, Expr},
    state::State,
    value::Value,
};

use core::f64;
use std::io::Write;

impl Expr {
    pub fn eval<T: Write>(self, s: &mut State, out: &mut T) -> Result<Value> {
        match self {
            Expr::BinOp { lhs, op, rhs } if op.is_inf_op() => match op {
                Operator::Plus => lhs.eval(s, out)? + rhs.eval(s, out)?,
                Operator::Minus => lhs.eval(s, out)? - rhs.eval(s, out)?,
                Operator::Times => lhs.eval(s, out)? * rhs.eval(s, out)?,
                Operator::Slash => lhs.eval(s, out)? / rhs.eval(s, out)?,
                Operator::Power => {
                    let l = lhs.eval(s, out)?;
                    let r = rhs.eval(s, out)?;

                    l.pow(r)
                }
                Operator::DoubleEqual => Ok(Value::Num(
                    (lhs.eval(s, out)? == rhs.eval(s, out)?) as u8 as f64,
                )),
                Operator::NotEqual => Ok(Value::Num(
                    (lhs.eval(s, out)? != rhs.eval(s, out)?) as u8 as f64,
                )),
                Operator::DoubleAnd | Operator::DoublePipe => todo!(),
                Operator::Greater
                | Operator::GreaterEqual
                | Operator::Lesser
                | Operator::LesserEqual => {
                    use std::cmp::PartialOrd;

                    let lhs = lhs.eval(s, out)?;
                    let rhs = rhs.eval(s, out)?;

                    let op = match op {
                        Operator::Greater => PartialOrd::gt,
                        Operator::GreaterEqual => PartialOrd::ge,
                        Operator::Lesser => PartialOrd::lt,
                        Operator::LesserEqual => PartialOrd::le,
                        _ => unreachable!(),
                    };

                    match (lhs, rhs) {
                        (l @ Value::Array(_), Value::Array(r)) => {
                            l.zip(r, |l, r| Ok(Value::Num(op(&l, &r) as u8 as f64)))
                        }
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
                Operator::PiTimes => rhs.eval(s, out)? * Value::Num(f64::consts::PI),
                Operator::Minus => {
                    let res = rhs.eval(s, out)?;
                    -res
                }
                Operator::Sqrt => {
                    let res = rhs.eval(s, out)?;

                    res.sqrt()
                }
                Operator::Not => {
                    let res = rhs.eval(s, out)?;

                    !res
                }
                Operator::Abs => {
                    let res = rhs.eval(s, out)?;

                    res.abs()
                }
                _ => unreachable!(),
            },
            Expr::FunCall(name, params) => match name.as_str() {
                "print" => {
                    for p in params {
                        let v = p.eval(s, out)?;
                        write!(out, "{v} ").unwrap();
                    }
                    writeln!(out).unwrap();

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
                    .map(|x| x.eval(s, out))
                    .collect::<Result<Vec<Value>>>()?
                    .into(),
            )),
            Expr::BinOp { .. } => unreachable!(),
            Expr::PreOp { .. } => unreachable!(),
        }
    }
}

impl Ast {
    pub fn eval<T: Write>(self, s: &mut State, out: &mut T) -> Result<Value> {
        match self {
            Ast::Expression(e) => e.eval(s, out),
            Ast::LetDecl(ident, body) => {
                if ident == "π" {
                    return Err(InterpreterError::AssignmentToPi.into());
                }

                let v = body.eval(s, out)?;
                s.set(ident, v);
                Ok(Value::Nothing)
            }
            Ast::Statement(e) => {
                let _ = e.eval(s, out)?;
                Ok(Value::Nothing)
            }
            Ast::Block(mut b) => {
                // Enter new scope
                s.variables.push(HashMap::new());

                let last = b.pop();

                for node in b {
                    node.eval(s, out)?;
                }

                Ok(match last {
                    Some(a) => {
                        let res = a.eval(s, out)?;
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

#[cfg(test)]
mod tests {
    use std::{io::stdout, rc::Rc};

    use crate::{lexer::lex, parser::parse, state::State, value::Value};

    fn assert_interp(input: &'static str, expected: Value) {
        let mut state = State::new();
        let mut stdout = stdout().lock();
        let ast = lex(input).and_then(parse).unwrap().pop().unwrap();
        let val = ast.eval(&mut state, &mut stdout).unwrap();

        assert_eq!(val, expected);
    }

    #[test]
    fn binop_nums() {
        assert_interp("1 + 2 * 2", Value::Num(5.0));
    }

    #[test]
    fn binop_num_arr() {
        assert_interp(
            "1 + [0, 1, 2]",
            Value::Array(Rc::new(vec![
                Value::Num(1.0),
                Value::Num(2.0),
                Value::Num(3.0),
            ])),
        );
    }

    #[test]
    fn unop_arr_num() {
        assert_interp(
            "√[4, 9, 25]",
            Value::Array(Rc::new(vec![
                Value::Num(2.0),
                Value::Num(3.0),
                Value::Num(5.0),
            ])),
        );
    }

    #[test]
    fn scope_test() {
        assert_interp(
            "let x = 10; { let x = 12; { let x = 13; x } }",
            Value::Num(13.0),
        );
    }

    #[test]
    fn arrlit() {
        assert_interp(
            "[1, 2, 3]",
            Value::Array(Rc::new(vec![
                Value::Num(1.0),
                Value::Num(2.0),
                Value::Num(3.0),
            ])),
        );
    }

    #[test]
    fn tolerant_comp() {
        assert_interp("0.1 + 0.2 == 0.3", Value::Num(1.0));
    }
}
