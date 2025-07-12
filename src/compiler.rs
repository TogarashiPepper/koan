use std::collections::HashMap;

use crate::{
    error::{InterpError, Result, VmError},
    lexer::Operator,
    parser::Ast,
    pool::{Expr, ExprPool, ExprRef},
    value::{Function, Value},
    vm::{OpCode, VM},
};

#[derive(Debug)]
pub struct Local {
    name: String,
    depth: u32,
}

#[derive(Debug)]
pub struct Compiler {
    data: Vec<Value>,
    globals: HashMap<String, Value>,

    locals: Vec<Local>,
    scope_depth: u32,

    functions: Vec<Function>,
    current_fn: usize,
}

impl Compiler {
    // TODO: get rid of this
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let locals = vec![Local {
            name: String::new(),
            depth: 0,
        }];

        Compiler {
            data: vec![],

            locals,
            scope_depth: 0,

            functions: vec![Function {
                arity: 0,
                chunk: vec![],
                name: "<main>".to_owned(),
            }],
            globals: HashMap::from([
                ("π".into(), Value::Num(std::f64::consts::PI)),
                ("e".into(), Value::Num(std::f64::consts::E)),
                ("true".into(), Value::Num(1.0)),
                ("false".into(), Value::Num(0.0)),
            ]),

            current_fn: 0,
        }
    }

    pub fn finish(self) -> VM {
        VM {
            pc: 0,
            stack: vec![],
            globals: self.globals,
            functions: self.functions,
            current_fn: self.current_fn,
            data: self.data,
        }
    }

    fn chunk(&self) -> &[u8] {
        &self.functions.last().unwrap().chunk
    }

    fn chunk_mut(&mut self) -> &mut Vec<u8> {
        &mut self.functions.last_mut().unwrap().chunk
    }

    fn resolve_local(&self, name: &str) -> Option<usize> {
        self.locals.iter().position(|local| local.name == name)
    }

    fn emit_jump(&mut self, op: OpCode) -> usize {
        self.chunk_mut().extend_from_slice(&[op as u8, 0xff, 0xff]);
        self.chunk().len() - 2
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = self.chunk().len() - offset - 2;

        if jump > u16::MAX.into() {
            panic!("tried to jump over >u16::max bytes");
        }

        let [l, r] = (jump as u16).to_le_bytes();

        self.chunk_mut()[offset] = l;
        self.chunk_mut()[offset + 1] = r;
    }

    pub fn compile_expr(&mut self, eref: ExprRef, pool: &ExprPool) -> Result<()> {
        match pool.get(eref) {
            Expr::BinOp { lhs, op, rhs } => {
                self.compile_expr(*lhs, pool)?;
                self.compile_expr(*rhs, pool)?;

                self.chunk_mut().push(match op {
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

                self.chunk_mut().push(match op {
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
                    .position(|e| *e == Value::Num(*lit))
                    .unwrap_or_else(|| {
                        self.data.push(Value::Num(*lit));
                        self.data.len() - 1
                    });

                debug_assert!(pos < 256);
                self.chunk_mut()
                    .extend_from_slice(&[OpCode::Load as u8, pos as u8]);
            }
            Expr::StrLit(lit) => {
                self.data.push(Value::UTF8(lit.to_owned()));

                debug_assert!(self.data.len() < 256);
                let ld_idx = self.data.len() - 1;

                self.chunk_mut()
                    .extend_from_slice(&[OpCode::Load as u8, ld_idx as u8]);
            }
            Expr::Ident(name) => {
                if let Some(idx) = self.resolve_local(name) {
                    debug_assert!(idx < 256);

                    self.chunk_mut()
                        .extend_from_slice(&[OpCode::GetLocal as u8, idx as u8]);
                } else {
                    let idx = self
                        .data
                        .iter()
                        .position(|x| matches!(x, Value::UTF8(k) if k == name))
                        .unwrap_or_else(|| {
                            self.data.push(Value::UTF8(name.to_owned()));

                            self.data.len() - 1
                        });

                    // TODO: handle >255 locals (if even worth it?)
                    debug_assert!(idx < 256);
                    self.chunk_mut()
                        .extend_from_slice(&[OpCode::GetGlobal as u8, idx as u8]);
                }
            }
            Expr::FunCall(name, args) => {
                match name.as_str() {
                    "print" => {
                        // TODO: space deliminate rather than \n, like in readme
                        for arg in args {
                            self.compile_expr(*arg, pool)?;
                            self.chunk_mut().push(OpCode::Print as u8);
                        }
                    }
                    _ => todo!(),
                }
            }
            Expr::Array(exprs) => {
                let len = exprs.len();

                for expr in exprs {
                    self.compile_expr(*expr, pool)?;
                }

                debug_assert!(len < u16::MAX as usize);
                let len = (len as u16).to_le_bytes();

                self.chunk_mut().extend_from_slice(&[
                    OpCode::CreateArray as u8,
                    len[0],
                    len[1],
                ]);
            }
            Expr::Block(stmts) => {
                self.scope_depth += 1;

                // TODO: dont clone here
                let mut stmts = stmts.clone();

                let Some(last) = stmts.pop() else {
                    self.scope_depth -= 1;

                    return Ok(());
                };

                for stmt in stmts {
                    self.compile(stmt, pool)?;
                }

                let top_before = self.chunk().len();
                self.compile(last, pool)?;

                let effect = VM::calc_stack_effect(&self.chunk()[top_before..]);
                debug_assert!(effect == 0 || effect == 1);

                let mut discard = OpCode::Discard;
                if effect == 1 {
                    discard = OpCode::DiscardUnder;

                    // TODO: properly figure out how to handle how values returned from blocks
                    // live on the stack. Because this is... horrible.
                    // idea: transform { 1 } into { let <unnameable> = 1; <unnameable> }
                    // maybe this way depth can be scope_depth - 1, as it was before,
                    // which didnt work out because
                    // { let x = 1; { let y = 2; y + x } }
                    // errors (todo: investigate cause)
                    self.locals.push(Local {
                        name: String::new(),
                        depth: u32::MAX,
                    });
                }

                let mut local_count = 0;
                self.locals.retain(|local| {
                    if local.depth == self.scope_depth {
                        local_count += 1;
                    }

                    local.depth != self.scope_depth
                });

                for _ in 0..local_count {
                    self.chunk_mut().push(discard as u8);
                }

                self.scope_depth -= 1;
            }
            Expr::IfElse {
                cond,
                body,
                else_body,
            } => {
                // TODO: forbid if expressions that return a value without an `else`
                // fix `if false { "hello" }` leaveing false on the stack
                // fix blocks returning values as a whole, potentially
                // implement mutability and desugar the following:
                // 1 + { let x = 2; x + 2 }
                // into:
                // let <gensym>; { let x = 2; <gensym> = x + 2 }; 1 + <gensym>

                self.compile_expr(*cond, pool)?;
                let then_jmp = self.emit_jump(OpCode::JumpIfFalse);
                self.chunk_mut().push(OpCode::Discard as u8);

                // TODO: dont clone
                self.compile(body.clone(), pool)?;

                let else_jump = self.emit_jump(OpCode::Jump);
                self.patch_jump(then_jmp);

                self.chunk_mut().push(OpCode::Discard as u8);

                if let Some(e_body) = else_body {
                    self.compile(e_body.clone(), pool)?;
                }

                self.patch_jump(else_jump);
            }
        }

        Ok(())
    }

    pub fn compile(&mut self, ast: Ast, pool: &ExprPool) -> Result<()> {
        match ast {
            Ast::Expression(e) => self.compile_expr(e, pool)?,
            Ast::Statement(s) => {
                let top = self.chunk().len();
                self.compile_expr(s, pool)?;

                debug_assert_eq!(VM::calc_stack_effect(&self.chunk()[top..]), 1);

                self.chunk_mut().push(OpCode::Discard as u8);
            }
            Ast::LetDecl { name, ty: _, body } => {
                self.compile_expr(body, pool)?;

                if self.scope_depth > 0 {
                    if self.locals.iter().any(|local| local.name == name) {
                        return Err(VmError::GlobalAlreadyDefined(name).into());
                    }

                    self.locals.push(Local {
                        name,
                        depth: self.scope_depth,
                    });
                } else {
                    self.data.push(Value::UTF8(name));

                    let dfg_idx = self.data.len() as u8 - 1;
                    self.chunk_mut()
                        .extend_from_slice(&[OpCode::DefineGlobal as u8, dfg_idx]);
                }
            }
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
