use std::collections::HashMap;

use crate::{
    error::KoanError,
    lexer::{self, Operator},
    parser::{self, Ast},
    pool::{Expr, ExprPool, ExprRef},
};

use codegen::ir;
use cranelift::{
    jit::{JITBuilder, JITModule},
    module::{default_libcall_names, Linkage, Module},
    native::builder,
    prelude::*,
};

const COMPARISON_TOLERANCE: f64 = 1e-14;

extern "C" fn pow_std(a: f64, b: f64) -> f64 {
    a.powf(b)
}

extern "C" fn cmp_tol_std(l: f64, r: f64) -> f64 {
    let max = l.abs().max(r.abs());
    let ct = COMPARISON_TOLERANCE * max;

    From::from((l - r).abs() <= ct)
}

extern "C" fn not_std(a: f64) -> f64 {
    if a == 0.0 {
        1.0
    } else {
        0.0
    }
}

extern "C" fn and_std(l: f64, r: f64) -> f64 {
    let res = (l == 0.0 && r == 0.0) || (l == 1.0 && r == 1.0);

    From::from(res)
}

extern "C" fn or_std(l: f64, r: f64) -> f64 {
    let res = l == 1.0 || r == 1.0;

    From::from(res)
}

/// The basic JIT class.
pub struct Jit {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: JITModule,
}

impl Default for Jit {
    fn default() -> Self {
        let mut flag_builder = settings::builder();

        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();

        let isa_builder = builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });

        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let mut builder = JITBuilder::with_isa(isa, default_libcall_names());

        let pow_addr: *const u8 = pow_std as *const u8;
        builder.symbol("pow", pow_addr);

        let cmp_tol_addr: *const u8 = cmp_tol_std as *const u8;
        builder.symbol("cmp_tol", cmp_tol_addr);

        let not_addr: *const u8 = not_std as *const u8;
        builder.symbol("not_std", not_addr);

        let and_addr: *const u8 = and_std as *const u8;
        builder.symbol("and_std", and_addr);

        let or_addr: *const u8 = or_std as *const u8;
        builder.symbol("or_std", or_addr);

        let module = JITModule::new(builder);

        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
        }
    }
}

impl Jit {
    pub fn compile(&mut self, input: &str) -> Result<*const u8, KoanError> {
        let tokens = lexer::lex(input)?;
        let (mut stmts, pool) = parser::parse(tokens)?;

        // TODO: actually handle translating multiple statements
        self.translate(stmts.pop().unwrap(), pool)?;

        let id = self
            .module
            .declare_function("main", Linkage::Export, &self.ctx.func.signature)
            .unwrap();

        self.module.define_function(id, &mut self.ctx).unwrap();

        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions().unwrap();

        let code = self.module.get_finalized_function(id);

        Ok(code)
    }

    fn translate(&mut self, ast: Ast, pool: ExprPool) -> Result<(), KoanError> {
        let f64_ty = types::F64;

        match ast {
            Ast::Expression(eref) | Ast::Statement(eref) => {
                let is_expr = matches!(ast, Ast::Expression(_));

                if is_expr {
                    // Expressions are just IIFEs that take nothing and return a float (for now)
                    self.ctx.func.signature.returns.push(AbiParam::new(f64_ty));
                }

                let mut builder =
                    FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

                let entry_bl = builder.create_block();

                builder.append_block_params_for_function_params(entry_bl);
                builder.switch_to_block(entry_bl);
                builder.seal_block(entry_bl);

                let mut translator = FunctionTranslator {
                    builder,
                    variables: HashMap::new(),
                    module: &mut self.module,
                };

                let rval = translator.translate_expr(eref, &pool);

                if is_expr {
                    translator.builder.ins().return_(&[rval]);
                }

                translator.builder.finalize();
            }
            Ast::Block(_) => todo!(),
            Ast::LetDecl { name, ty, body } => todo!(),
            Ast::FunDecl { name, params, body } => todo!(),
        }

        Ok(())
    }
}

struct FunctionTranslator<'a> {
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, Variable>,
    module: &'a mut JITModule,
}

impl<'a> FunctionTranslator<'a> {
    fn translate_assign(&mut self, name: &str, eref: ExprRef, pool: &ExprPool) -> Value {
        let new_value = self.translate_expr(eref, pool);
        let variable = self.variables.get(name).unwrap();
        self.builder.def_var(*variable, new_value);
        new_value
    }

    fn call_stdlib(&mut self, name: &str, args: &[Value]) -> Value {
        let mut sig = self.module.make_signature();

        for _arg in args {
            sig.params.push(AbiParam::new(types::F64));
        }

        sig.returns.push(AbiParam::new(types::F64));

        let callee = self
            .module
            .declare_function(name, Linkage::Import, &sig)
            .expect("problem declaring function");

        let local_callee: ir::FuncRef =
            self.module.declare_func_in_func(callee, self.builder.func);

        let call = self.builder.ins().call(local_callee, args);

        *self.builder.inst_results(call).first().unwrap()
    }

    fn translate_expr(&mut self, eref: ExprRef, pool: &ExprPool) -> Value {
        match pool.get(eref) {
            Expr::NumLit(num) => self.builder.ins().f64const(*num),
            Expr::BinOp { lhs, op, rhs } => {
                let lhs = self.translate_expr(*lhs, pool);
                let rhs = self.translate_expr(*rhs, pool);

                match op {
                    Operator::Power => self.call_stdlib("pow", &[lhs, rhs]),
                    Operator::DoubleEqual => self.call_stdlib("cmp_tol", &[lhs, rhs]),
                    Operator::Plus => self.builder.ins().fadd(lhs, rhs),
                    Operator::Minus => self.builder.ins().fsub(lhs, rhs),
                    Operator::Times => self.builder.ins().fmul(lhs, rhs),
                    Operator::Slash => self.builder.ins().fdiv(lhs, rhs),
                    Operator::Greater => {
                        self.builder.ins().fcmp(FloatCC::GreaterThan, lhs, rhs)
                    }
                    Operator::GreaterEqual => {
                        self.builder
                            .ins()
                            .fcmp(FloatCC::GreaterThanOrEqual, lhs, rhs)
                    }
                    Operator::Lesser => {
                        self.builder.ins().fcmp(FloatCC::LessThan, lhs, rhs)
                    }
                    Operator::LesserEqual => {
                        self.builder.ins().fcmp(FloatCC::LessThanOrEqual, lhs, rhs)
                    }
                    Operator::NotEqual => {
                        self.builder.ins().fcmp(FloatCC::NotEqual, lhs, rhs)
                    }
                    Operator::DoublePipe => self.call_stdlib("or_std", &[lhs, rhs]),
                    Operator::DoubleAnd => self.call_stdlib("and_std", &[lhs, rhs]),

                    _ => unreachable!(),
                }
            }
            Expr::PreOp { op, rhs } => {
                let rhs = self.translate_expr(*rhs, pool);

                match op {
                    Operator::PiTimes => {
                        let pi = self.builder.ins().f64const(std::f64::consts::PI);

                        self.builder.ins().fmul(pi, rhs)
                    }
                    Operator::Sqrt => self.builder.ins().sqrt(rhs),
                    Operator::Minus => self.builder.ins().fneg(rhs),
                    Operator::Abs => self.builder.ins().fabs(rhs),

                    Operator::Not => self.call_stdlib("not_std", &[rhs]),

                    _ => unreachable!(),
                }
            }
            Expr::Ident(_) => todo!(),
            Expr::StrLit(_) => todo!(),
            Expr::FunCall(_, _) => todo!(),
            Expr::Array(_) => todo!(),
            Expr::IfElse {
                cond,
                body,
                else_body,
            } => todo!(),
        }
    }
}
