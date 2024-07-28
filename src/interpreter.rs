use std::{collections::HashMap, rc::Rc};

use crate::{
    error::{InterpError, Result}, inferance::annotate, lexer::Operator, parser::Ast, pool::{Expr, ExprPool, ExprRef}, state::{Function, State}, value::Value
};

use core::f64;
use std::io::Write;

pub struct IntrpCtx<'a, W> {
    pub writer: W,
    pub state: &'a mut State,
    // Not strictly necessary, but to make it clear to the compiler that exprpool is immutable
    pub pool: &'a ExprPool,
}

impl<W: Write> IntrpCtx<'_, W> {
    pub fn eval(&mut self, exp_ref: ExprRef) -> Result<Value> {
        match self.pool.get(exp_ref) {
            Expr::BinOp { lhs, op, rhs } if op.is_inf_op() => match op {
                Operator::Plus => self.eval(*lhs)? + self.eval(*rhs)?,
                Operator::Minus => self.eval(*lhs)? - self.eval(*rhs)?,
                Operator::Times => self.eval(*lhs)? * self.eval(*rhs)?,
                Operator::Slash => self.eval(*lhs)? / self.eval(*rhs)?,
                Operator::Power => {
                    let l = self.eval(*lhs)?;
                    let r = self.eval(*rhs)?;

                    l.pow(r)
                }
                Operator::DoubleEqual => Ok(Value::Num(
                    (self.eval(*lhs)? == self.eval(*rhs)?) as u8 as f64,
                )),
                Operator::NotEqual => Ok(Value::Num(
                    (self.eval(*lhs)? != self.eval(*rhs)?) as u8 as f64,
                )),
                Operator::DoubleAnd => {
                    let lhs = self.eval(*lhs)?;
                    let rhs = self.eval(*rhs)?;

                    let res = lhs == Value::Num(1.0) && rhs == Value::Num(1.0);

                    Ok(Value::Num(res as u8 as f64))
                }
                Operator::DoublePipe => {
                    let lhs = self.eval(*lhs)?;
                    let rhs = self.eval(*rhs)?;

                    let res = lhs == Value::Num(1.0) || rhs == Value::Num(1.0);

                    Ok(Value::Num(res as u8 as f64))
                }
                Operator::Greater
                | Operator::GreaterEqual
                | Operator::Lesser
                | Operator::LesserEqual => {
                    use std::cmp::PartialOrd;

                    let lhs = self.eval(*lhs)?;
                    let rhs = self.eval(*rhs)?;

                    let op = match op {
                        Operator::Greater => PartialOrd::gt,
                        Operator::GreaterEqual => PartialOrd::ge,
                        Operator::Lesser => PartialOrd::lt,
                        Operator::LesserEqual => PartialOrd::le,
                        _ => unreachable!(),
                    };

                    match (lhs, rhs) {
                        (l @ Value::Array(_), Value::Array(r)) => l
                            .zip(r, |l, r| {
                                Ok(Value::Num(op(&l, &r) as u8 as f64))
                            }),
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
                Operator::PiTimes => {
                    self.eval(*rhs)? * Value::Num(f64::consts::PI)
                }
                Operator::Minus => {
                    let res = self.eval(*rhs)?;
                    -res
                }
                Operator::Sqrt => {
                    let res = self.eval(*rhs)?;

                    res.sqrt()
                }
                Operator::Not => {
                    let res = self.eval(*rhs)?;

                    !res
                }
                Operator::Abs => {
                    let res = self.eval(*rhs)?;

                    res.abs()
                }
                _ => unreachable!(),
            },

            Expr::FunCall(name, params) => match name.as_str() {
                "print" => {
                    for p in params {
                        let v = self.eval(*p)?;
                        write!(self.writer, "{v} ").unwrap();
                    }
                    writeln!(self.writer).unwrap();

                    Ok(Value::Nothing)
                }
                "floor" => {
                    // TODO: get some helpers to assert common preconditions: arity, types etc.

                    if params.is_empty() {
                        return Err(InterpError::MismatchedArity(
                            name.to_owned(),
                            params.len(),
                            1,
                        )
                        .into());
                    }

                    let mut params = params.clone();

                    if params.len() == 1 {
                        match self.eval(params.pop().unwrap())? {
                            n @ Value::Num(_) => n.in_num(name, f64::floor),
                            a @ Value::Array(_) => {
                                a.map(|n| n.in_num(name, f64::floor))
                            }
                            t => Err(InterpError::InvalidParamTy(
                                name.to_owned(),
                                t.ty_str(),
                            )
                            .into()),
                        }
                    } else {
                        let retvals = params
                            .into_iter()
                            .map(|exp| self.eval(exp))
                            .map(|x| x?.in_num("floor", f64::floor))
                            .collect::<Result<Vec<Value>>>()?;

                        Ok(Value::Array(Rc::new(retvals)))
                    }
                }
                "range" => {
                    if params.len() != 1 {
                        return Err(InterpError::MismatchedArity(
                            name.to_owned(),
                            params.len(),
                            1,
                        )
                        .into());
                    }

                    let mut params = params.clone();

                    let v =
                        self.eval(params.pop().unwrap())?.as_num(name)?.floor()
                            as u64;

                    if v > 4096 {
                        return Err(InterpError::RangeTooLarge(v).into());
                    }

                    let arr = (0..v)
                        .map(|elem| Value::Num(elem as f64))
                        .collect::<Vec<Value>>();

                    Ok(Value::Array(Rc::new(arr)))
                }
                _ => match self.state.functions.get(name).cloned() {
                    Some(f) => {
                        // TODO: dont do this, figure out something better for perf
                        let p_names = f.params.clone();
                        let p_exprs = params;

                        assert_eq!(p_names.len(), p_exprs.len());

                        let mut old_vars = State::new().variables;

                        for (pn, pe) in p_names.into_iter().zip(p_exprs) {
                            let v = self.eval(*pe)?;

                            if v.ty_str() != pn.1 {
                                return Err(InterpError::InvalidParamTy(
                                    name.to_owned(),
                                    v.ty_str(),
                                )
                                .into());
                            }

                            old_vars.last_mut().unwrap().insert(pn.0, v);
                        }

                        // Replace current scope with the function's scope
                        std::mem::swap(
                            &mut self.state.variables,
                            &mut old_vars,
                        );

                        // TODO: dont do this (x2), maybe some way to share/eval by-ref
                        let res = self.eval_ast(f.body.clone())?;

                        // Set the current scope back to the non-function one
                        std::mem::swap(
                            &mut self.state.variables,
                            &mut old_vars,
                        );

                        Ok(res)
                    }
                    None => Err(InterpError::UndefFunc(name.to_owned()).into()),
                },
            },

            Expr::Ident(ident) => self
                .state
                .get(ident)
                .ok_or_else(|| InterpError::UndefVar(ident.to_owned()).into()),
            Expr::StrLit(s) => Ok(Value::UTF8(s.to_owned())),
            Expr::NumLit(n) => Ok(Value::Num(*n)),
            Expr::Array(a) => Ok(Value::Array(
                a.iter()
                    .map(|x| self.eval(*x))
                    .collect::<Result<Vec<Value>>>()?
                    .into(),
            )),
            Expr::BinOp { .. } => unreachable!(),
            Expr::PreOp { .. } => unreachable!(),
            Expr::IfElse {
                cond,
                body,
                else_body,
            } => {
                let boolean = self.eval(*cond)?;
                let mut res = Value::Nothing;

                match boolean {
                    Value::Num(n) => {
                        if n == 1.0 {
                            res = self.eval_ast(body.clone())?;
                        } else if n == 0.0 {
                            if let Some(x) = else_body {
                                res = self.eval_ast(x.clone())?;
                            }
                        } else {
                            return Err(InterpError::InvalidIfNum.into());
                        }

                        Ok(res)
                    }
                    _ => Err(InterpError::InvalidIfTy.into()),
                }
            }
        }
    }

    pub fn eval_ast(&mut self, ast: Ast) -> Result<Value> {
        println!("{:#?}", annotate(ast, self.pool));
        Ok(Value::Nothing)
        // match ast {
        //     Ast::Expression(e) => self.eval(e),
        //     Ast::LetDecl {
        //         name: ident,
        //         body,
        //         ty: _,
        //     } => {
        //         if ident == "π" {
        //             return Err(InterpError::AssignmentToPi.into());
        //         }
        //
        //         let v = self.eval(body)?;
        //         self.state.set(ident, v);
        //         Ok(Value::Nothing)
        //     }
        //     Ast::Statement(e) => {
        //         let _ = self.eval(e)?;
        //         Ok(Value::Nothing)
        //     }
        //     Ast::Block(mut b) => {
        //         // Enter new scope
        //         self.state.variables.push(HashMap::new());
        //
        //         let last = b.pop();
        //
        //         for node in b {
        //             self.eval_ast(node)?;
        //         }
        //
        //         Ok(match last {
        //             Some(a) => {
        //                 let res = self.eval_ast(a)?;
        //                 self.state.variables.pop();
        //
        //                 res
        //             }
        //             None => Value::Nothing,
        //         })
        //     }
        //     Ast::FunDecl { name, params, body } => {
        //         if self.state.variables.len() != 1 {
        //             return Err(InterpError::NonTopLevelFnDef.into());
        //         }
        //
        //         self.state.functions.insert(
        //             name,
        //             Function {
        //                 params,
        //                 body: *body,
        //             },
        //         );
        //
        //         Ok(Value::Nothing)
        //     }
        // }
    }
}

#[cfg(test)]
mod tests {
    use std::{io::stdout, rc::Rc};

    use crate::{
        interpreter::IntrpCtx, lexer::lex, parser::parse, state::State,
        value::Value,
    };

    fn assert_interp(input: &'static str, expected: Value) {
        let (mut ast, pool) = lex(input).and_then(parse).unwrap();
        let ast = ast.pop().unwrap();

        let mut ctx = IntrpCtx {
            writer: stdout().lock(),
            state: &mut State::new(),
            pool: &pool,
        };

        let val = ctx.eval_ast(ast).unwrap();

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
