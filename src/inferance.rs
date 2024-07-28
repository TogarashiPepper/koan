use std::collections::HashMap;

use crate::{
    error::{InterpError, Result},
    lexer::Operator,
    parser::Ast,
    pool::{Expr, ExprPool, ExprRef},
    state::{Function, State},
    value::ValTy,
};

// TODO: replace this with State and give state a generic param
#[derive(Debug)]
pub struct StateSim {
    // Array of environments, index = depth (i.e. `0` = global scope)
    pub variables: Vec<HashMap<String, ValTy>>,
    pub functions: HashMap<String, Function>,
}

impl StateSim {
    fn get(&self, k: &str) -> Option<ValTy> {
        for scope in self.variables.iter().rev() {
            if let Some(x) = scope.get(k) {
                return Some(x.clone());
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
pub fn infer(
    ast: &Ast,
    pool: &ExprPool,
    state_sim: &mut StateSim,
) -> Result<ValTy> {
    match ast {
        Ast::Expression(e) => infer_exp(e, pool, todo!()),
        Ast::Statement(_) => Ok(ValTy::Nothing),
        Ast::Block(statements) => {
            state_sim.variables.push(HashMap::new());
            let len = statements.len();

            if len >= 2 {
                for stmt in &statements[0..len - 2] {
                    infer(stmt, pool, state_sim);
                }
            }

            if len >= 1 {
                let ret_val = &statements[len - 1];

                infer(ret_val, pool, state_sim)
            } else {
                Ok(ValTy::Nothing)
            }
        }
        Ast::LetDecl { name, ty, body } => {
            let infered = infer_exp(*body, pool, todo!())?;
            state_sim.set(name.to_owned(), infered);

            Ok(infered)
        }
        Ast::FunDecl { name, params, body } => todo!(),
    }
}

pub fn annotate(ast: Ast, pool: &ExprPool) -> Result<Ast> {
    let mut global = HashMap::new();
    global.insert("π".to_owned(), ValTy::Number);
    global.insert("e".to_owned(), ValTy::Number);

    let mut state_sim = StateSim {
        variables: vec![global],
        functions: HashMap::new(),
    };

    match ast {
        Ast::Expression(_) | Ast::Statement(_) | Ast::Block(_) => Ok(ast),
        Ast::LetDecl {
            ref name,
            ty: _,
            body,
        } => {
            let inferred = infer(&ast, pool, &mut state_sim)?;
            let annotated = Ast::LetDecl {
                name: name.to_owned(),
                ty: Some(inferred),
                body,
            };

            Ok(annotated)
        }
        Ast::FunDecl { name, params, body } => todo!(),
    }
}

/// `infer_exp` is used to perform type inference on an `Expr`. The resolver is a function that takes an
/// identifier and infers the type of the value held by that idenftifier. This parameter is used to
/// allow the inference for the AST to provide the `Expr` the information it needs without
/// providing it the entire scope tree every time.
pub fn infer_exp<F>(
    expr: ExprRef,
    pool: &ExprPool,
    resolve: &mut F,
) -> Result<ValTy>
where
    F: FnMut(String) -> Result<ValTy>,
{
    use ValTy::{Array, Number, String};

    let exp = pool.get(expr);

    Ok(match exp {
        Expr::BinOp { lhs, op, rhs } => {
            let lhs = infer_exp(*lhs, pool, resolve)?;
            let rhs = infer_exp(*rhs, pool, resolve)?;

            match op {
                Operator::Power => match (lhs, rhs) {
                    (Array, Array) | (Number, Array) | (Array, Number) => Array,
                    (Number, Number) => Number,

                    (l, r) => {
                        return Err(InterpError::MismatchedTypes(
                            Operator::Power,
                            l,
                            r,
                        )
                        .into())
                    }
                },
                Operator::Plus => match (lhs, rhs) {
                    (Number, Number) => Number,
                    (Array, Number) | (Number, Array) | (Array, Array) => Array,
                    (String, String) => String,

                    (l, r) => {
                        return Err(InterpError::MismatchedTypes(
                            Operator::Plus,
                            l,
                            r,
                        )
                        .into())
                    }
                },
                Operator::Minus => match (lhs, rhs) {
                    (Number, Number) => Number,
                    (Number, Array) | (Array, Number) | (Array, Array) => Array,

                    (l, r) => {
                        return Err(InterpError::MismatchedTypes(
                            Operator::Minus,
                            l,
                            r,
                        )
                        .into())
                    }
                },
                Operator::Times => match (lhs, rhs) {
                    (Number, Array) | (Array, Number) | (Array, Array) => Array,
                    (String, Number) | (Number, String) => String,
                    (Number, Number) => Number,

                    (l, r) => {
                        return Err(InterpError::MismatchedTypes(
                            Operator::Times,
                            l,
                            r,
                        )
                        .into())
                    }
                },
                Operator::Slash
                | Operator::DoubleEqual
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
            let rhs = infer_exp(*rhs, pool, resolve)?;
            match rhs {
                Number => Number,
                Array => Array,

                r => {
                    return Err(InterpError::MismatchedUnOp(
                        Operator::PiTimes,
                        r,
                    )
                    .into())
                }
            }
        }
        Expr::NumLit(_) => Number,
        Expr::StrLit(_) => String,
        Expr::Array(_) => Array,
        Expr::Ident(name) => resolve(name.to_owned())?,
        Expr::FunCall(_, _) => todo!(),
        Expr::IfElse {
            cond,
            body,
            else_body,
        } => todo!(),
    })
}
