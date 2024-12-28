use std::collections::HashMap;

use crate::{
    error::{InterpError, KoanError, Result},
    lexer::Operator,
    parser::Ast,
    pool::{Expr, ExprPool, ExprRef},
    value::ValTy,
};

// TODO: replace this with State and give state a generic param
#[derive(Debug)]
pub struct StateSim {
    // Array of environments, index = depth (i.e. `0` = global scope)
    pub variables: Vec<HashMap<String, ValTy>>,
    // TODO: Make tuple into struct, .0 is params, .1 is return type
    pub functions: HashMap<String, (Vec<ValTy>, ValTy)>,
}

impl StateSim {
    fn new() -> Self {
        let mut global = HashMap::new();
        global.insert("π".to_owned(), ValTy::Number);
        global.insert("e".to_owned(), ValTy::Number);

        StateSim {
            variables: vec![global],
            functions: HashMap::new(),
        }
    }

    fn get(&self, k: &str) -> Option<ValTy> {
        for scope in self.variables.iter().rev() {
            if let Some(x) = scope.get(k).copied() {
                return Some(x);
            }
        }

        None
    }

    fn set(&mut self, k: String, v: ValTy) {
        // Unwrap is okay bc we don't let length fall <1
        let x = self.variables.last_mut().unwrap();
        x.insert(k, v);
    }
}

/// Performs type inference on the provided Ast
pub fn infer(ast: &Ast, pool: &ExprPool, state_sim: &mut StateSim) -> Result<ValTy> {
    match ast {
        Ast::Expression(e) => infer_exp(*e, pool, state_sim),
        Ast::Statement(_) => Ok(ValTy::Nothing),
        Ast::Block(statements) => {
            state_sim.variables.push(HashMap::new());
            let len = statements.len();

            if len >= 2 {
                for stmt in &statements[0..=len - 2] {
                    // We don't care about the type but we want any variables registered to be
                    // added to `state_sim`
                    let _ = infer(stmt, pool, state_sim)?;
                }
            }

            let res = if len >= 1 {
                let ret_val = &statements[len - 1];

                infer(ret_val, pool, state_sim)
            } else {
                Ok(ValTy::Nothing)
            };

            state_sim.variables.pop();

            res
        }
        Ast::LetDecl { name, ty: _, body } => {
            let inferred = infer_exp(*body, pool, state_sim)?;
            state_sim.set(name.to_owned(), inferred);

            Ok(inferred)
        }
        Ast::FunDecl {
            name,
            params,
            ret,
            body,
        } => {
            let body_ty = infer(body, pool, state_sim)?;

            if *ret != body_ty {
                return Err(InterpError::MismatchedReturnTy(*ret, body_ty).into());
            }

            state_sim.functions.insert(
                name.to_owned(),
                (params.iter().map(|x| x.1).collect(), *ret),
            );

            Ok(ValTy::Nothing)
        }
    }
}

pub fn annotate(ast: Ast, pool: &ExprPool) -> Result<Ast> {
    let mut state_sim = StateSim::new();

    annotate_inner(ast, pool, &mut state_sim)
}

fn annotate_inner(ast: Ast, pool: &ExprPool, sim: &mut StateSim) -> Result<Ast> {
    match ast {
        Ast::Expression(_) | Ast::Statement(_) => Ok(ast),
        b @ Ast::Block(_) => {
            infer(&b, pool, sim)?;

            let Ast::Block(stmts) = b else { unreachable!() };

            let stmts = stmts
                .into_iter()
                .map(|st| annotate_inner(st, pool, sim))
                .collect::<Result<Vec<Ast>>>()?;

            Ok(Ast::Block(stmts))
        }
        Ast::LetDecl { ref name, ty, body } => {
            let inferred = infer(&ast, pool, sim)?;

            if let Some(ty) = ty {
                if ty != inferred {
                    return Err(InterpError::InvalidLetType(
                        inferred.to_string(),
                        ty.to_string(),
                    )
                    .into());
                }
            }

            let annotated = Ast::LetDecl {
                name: name.to_owned(),
                ty: Some(inferred),
                body,
            };

            Ok(annotated)
        }
        x @ Ast::FunDecl { .. } => Ok(x),
    }
}

/// `infer_exp` is used to perform type inference on an `Expr`. The resolver is a function that takes an
/// identifier and infers the type of the value held by that idenftifier. This parameter is used to
/// allow the inference for the AST to provide the `Expr` the information it needs without
/// providing it the entire scope tree every time.
pub fn infer_exp(
    expr: ExprRef,
    pool: &ExprPool,
    state_sim: &mut StateSim,
) -> Result<ValTy> {
    use ValTy::{Array, Number, String};

    let exp = pool.get(expr);

    Ok(match exp {
        Expr::BinOp { lhs, op, rhs } => {
            let lhs = infer_exp(*lhs, pool, state_sim)?;
            let rhs = infer_exp(*rhs, pool, state_sim)?;

            match op {
                Operator::Power => match (lhs, rhs) {
                    (Array, Array) | (Number, Array) | (Array, Number) => Array,
                    (Number, Number) => Number,

                    (l, r) => {
                        return Err(
                            InterpError::MismatchedTypes(Operator::Power, l, r).into()
                        )
                    }
                },
                Operator::Slash => match (lhs, rhs) {
                    (Array, Array) | (Number, Array) | (Array, Number) => Array,
                    (Number, Number) => Number,

                    (l, r) => {
                        return Err(
                            InterpError::MismatchedTypes(Operator::Slash, l, r).into()
                        )
                    }
                },
                Operator::Plus => match (lhs, rhs) {
                    (Number, Number) => Number,
                    (Array, Number) | (Number, Array) | (Array, Array) => Array,
                    (String, String) => String,

                    (l, r) => {
                        return Err(
                            InterpError::MismatchedTypes(Operator::Plus, l, r).into()
                        )
                    }
                },
                Operator::Minus => match (lhs, rhs) {
                    (Number, Number) => Number,
                    (Number, Array) | (Array, Number) | (Array, Array) => Array,

                    (l, r) => {
                        return Err(
                            InterpError::MismatchedTypes(Operator::Minus, l, r).into()
                        )
                    }
                },
                Operator::Times => match (lhs, rhs) {
                    (Number, Array) | (Array, Number) | (Array, Array) => Array,
                    (String, Number) | (Number, String) => String,
                    (Number, Number) => Number,

                    (l, r) => {
                        return Err(
                            InterpError::MismatchedTypes(Operator::Times, l, r).into()
                        )
                    }
                },
                Operator::DoubleEqual
                | Operator::Greater
                | Operator::GreaterEqual
                | Operator::Lesser
                | Operator::LesserEqual
                | Operator::NotEqual
                | Operator::DoublePipe
                | Operator::DoubleAnd => Number,

                _ => unreachable!(),
            }
        }
        Expr::PreOp { op: _, rhs } => {
            let rhs = infer_exp(*rhs, pool, state_sim)?;
            match rhs {
                Number => Number,
                Array => Array,

                r => return Err(InterpError::MismatchedUnOp(Operator::PiTimes, r).into()),
            }
        }
        Expr::NumLit(_) => Number,
        Expr::StrLit(_) => String,
        Expr::Array(_) => Array,
        Expr::Ident(name) => state_sim
            .get(name)
            .ok_or_else(|| KoanError::from(InterpError::UndefVar(name.to_owned())))?,

        Expr::FunCall(name, params) => {
            let func = state_sim
                .functions
                .get(name)
                .ok_or_else(|| KoanError::from(InterpError::UndefFunc(name.to_owned())))?
                .clone();

            // Ensure params are typed correctly
            for (given, expected) in params.iter().zip(func.0.iter()) {
                let ty_given = infer_exp(*given, pool, state_sim)?;

                if ty_given != *expected {
                    return Err(
                        InterpError::InvalidParamTy(name.to_owned(), ty_given).into()
                    );
                }
            }

            func.1
        }
        Expr::IfElse {
            cond,
            body,
            else_body,
        } => {
            // Ensure cond is a number
            let cond_ty = infer_exp(*cond, pool, state_sim)?;

            if cond_ty != Number {
                return Err(InterpError::InvalidIfTy.into());
            }

            let body_ty = infer(body, pool, state_sim)?;

            // Infer requires &mut but we only have &
            if let Some(else_body) = else_body {
                let else_ty = infer(else_body, pool, state_sim)?;

                if body_ty != else_ty {
                    return Err(InterpError::IfBodyMismatch(body_ty, else_ty).into());
                }
            } else if body_ty != ValTy::Nothing {
                return Err(InterpError::IfExprWithoutElse.into());
            }

            body_ty
        }
    })
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::lex,
        parser::{parse, Ast}, value::ValTy,
    };

    use super::{infer_exp, StateSim};

    fn test_infer_exp(input: String, expected: ValTy) {
        let (ast, pool) = lex(input.as_str()).and_then(parse).unwrap();
        let mut state_sim = StateSim::new();

        if let Ast::Expression(expr) = ast[0] {
            let ty = infer_exp(expr, &pool, &mut state_sim).unwrap();

            assert_eq!(ty, expected);
        } else {
            panic!()
        }
    }

    #[test]
    fn array_binop() {
        for sym in ["+", "-", "^", "*", "/"] {
            test_infer_exp(format!("2 {sym} [1, 2, 3]"), ValTy::Array);
            test_infer_exp(format!("[1, 2, 3] {sym} 2"), ValTy::Array);
            test_infer_exp(format!("[] {sym} 2"), ValTy::Array);
            test_infer_exp(format!("[] {sym} []"), ValTy::Array);
        }
    }

    #[test]
    fn array_unop() {
        for sym in ["-", "○", "√"] {
            test_infer_exp(format!("{sym} [1, 2, 3]"), ValTy::Array);
        }
    }

    #[test]
    fn scalar_binop() {
        todo!();
    }
}
