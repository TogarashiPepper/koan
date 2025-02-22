use crate::{
    error::Result,
    lexer::Operator,
    parser::Ast,
    pool::{Expr, ExprPool, ExprRef},
    value::Value,
    vm::{OpCode, VM},
};

impl VM {
    pub fn compile_expr(&mut self, eref: ExprRef, pool: &ExprPool) -> Result<()> {
        match pool.get(eref) {
            Expr::BinOp { lhs, op, rhs } => {
                self.compile_expr(*lhs, pool)?;
                self.compile_expr(*rhs, pool)?;

                self.chunk.push(match op {
                    Operator::Power => OpCode::Pow,
                    Operator::Plus => OpCode::Add,
                    Operator::Minus => OpCode::Sub,
                    Operator::Times => OpCode::Mul,
                    Operator::Slash => OpCode::Div,
                    Operator::DoubleEqual => OpCode::Eq,
                    Operator::NotEqual => OpCode::Neq,
                    Operator::Greater => OpCode::Greater,
                    Operator::GreaterEqual => OpCode::GreaterEq,
                    Operator::Lesser => OpCode::Lesser,
                    Operator::LesserEqual => OpCode::LesserEq,
                    Operator::DoublePipe => OpCode::Or,
                    Operator::DoubleAnd => OpCode::And,

                    Operator::Not
                    | Operator::Abs
                    | Operator::PiTimes
                    | Operator::Sqrt => panic!("invalid binop"),

                    Operator::Equal => panic!("= is not a valid bin op yet"),
                } as u8);
            }
            Expr::PreOp { op, rhs } => {
                self.compile_expr(*rhs, pool)?;

                self.chunk.push(match op {
                    Operator::Not => OpCode::Not,
                    Operator::Abs => OpCode::Abs,
                    Operator::PiTimes => OpCode::PiTimes,
                    Operator::Sqrt => OpCode::Sqrt,

                    _ => panic!("invalid unop"),
                } as u8);
            }
            Expr::NumLit(lit) => {
                let pos = self
                    .data
                    .iter()
                    .position(|e| e == &Value::Num(*lit))
                    .unwrap_or_else(|| {
                        self.data.push(Value::Num(*lit));
                        self.data.len() - 1
                    });

                // TODO: check pos < u8::MAX
                self.chunk
                    .extend_from_slice(&[OpCode::Load as u8, pos as u8]);
            }
            Expr::StrLit(lit) => {
                self.data.push(Value::UTF8(lit.to_owned()));

                self.chunk.extend_from_slice(&[
                    OpCode::Load as u8,
                    (self.data.len() - 1) as u8,
                ]);
            }
            Expr::Ident(_) => todo!(),
            Expr::FunCall(_, _) => todo!(),
            Expr::Array(_) => todo!(),
            Expr::IfElse {
                cond,
                body,
                else_body,
            } => todo!(),
        }

        Ok(())
    }

    pub fn compile(&mut self, ast: Ast, pool: &ExprPool) -> Result<()> {
        match ast {
            Ast::Expression(e) => self.compile_expr(e, pool)?,
            // TODO: pop off stack to discord? pop stack effect many elems?
            Ast::Statement(s) => {
                self.compile_expr(s, pool)?;
                self.chunk.push(OpCode::Discard as u8);
            },
            Ast::Block(stmts) => {
                // TODO: handle scoping, pop-ing values off stack etc

                for stmt in stmts {
                    self.compile(stmt, pool)?;
                }
            }
            Ast::LetDecl { name, ty, body } => todo!(),
            Ast::FunDecl {
                name,
                params,
                ret,
                body,
            } => todo!(),
        }

        Ok(())
    }
}
