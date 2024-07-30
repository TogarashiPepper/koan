use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    execution_engine::JitFunction,
    module::Module,
    types::{FloatType, IntType},
    values::{FloatValue, IntValue},
    OptimizationLevel,
};

use crate::{
    lexer::Operator,
    parser::Ast,
    pool::{Expr, ExprPool, ExprRef},
};

struct RecursiveBuilder<'a> {
    f64_t: FloatType<'a>,
    module: Module<'a>,
    context: &'a Context,
    builder: &'a Builder<'a>,
}

impl<'a> RecursiveBuilder<'a> {
    pub fn new(
        f64_t: FloatType<'a>,
        module: Module<'a>,
        builder: &'a Builder<'a>,
        context: &'a Context,
    ) -> Self {
        let pow_fn_type = f64_t.fn_type(&[f64_t.into(), f64_t.into()], false);
        module.add_function("llvm.pow.f64", pow_fn_type, None);

        Self {
            f64_t,
            module,
            builder,
            context,
        }
    }

    pub fn exec(self) {
        llvm_jit_exec(self.module);
    }

    pub fn emit_add_func(&self) {
        let fn_type = self
            .f64_t
            .fn_type(&[self.f64_t.into(), self.f64_t.into()], false);
        let intrinsic = self.module.get_function("llvm.pow.f64").unwrap();
        let function = self.module.add_function("power", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        let x = function.get_nth_param(0).unwrap().into_float_value();
        let y = function.get_nth_param(1).unwrap().into_float_value();

        // let sum = self.builder.build_float_add(x, y, "sum").unwrap();
        let sum = self
            .builder
            .build_direct_call(intrinsic, &[x.into(), y.into()], "ret")
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_float_value();

        self.builder.build_return(Some(&sum)).unwrap();

        self.module.verify().unwrap();
        self.module.print_to_stderr();
    }

    pub fn build(
        &self,
        ast: ExprRef,
        pool: &ExprPool,
    ) -> Result<FloatValue, BuilderError> {
        let exp_ref = pool.get(ast);

        match exp_ref {
            Expr::NumLit(n) => Ok(self.f64_t.const_float(*n)),
            Expr::PreOp { op, rhs } => {
                let op = *op;
                let child = self.build(*rhs, pool)?;

                match op {
                    Operator::PiTimes => todo!(),
                    Operator::Sqrt => todo!(),
                    Operator::Minus => self.builder.build_float_neg(child, "negation"),
                    Operator::Not => todo!(),

                    _ => unreachable!(),
                }
            }

            Expr::BinOp { lhs, op, rhs } => {
                let op = *op;
                let lhs = self.build(*lhs, pool)?;
                let rhs = self.build(*rhs, pool)?;

                match op {
                    Operator::Power => {
                        let res = self
                            .builder
                            .build_direct_call(
                                self.module.get_function("llvm.pow.f64").unwrap(),
                                &[lhs.into(), rhs.into()],
                                "ret",
                            )?
                            .try_as_basic_value()
                            .left()
                            .unwrap()
                            .into_float_value();

                        Ok(res)
                    }
                    Operator::Plus => todo!(),
                    Operator::Minus => todo!(),
                    Operator::Times => todo!(),
                    Operator::Slash => todo!(),
                    Operator::DoubleEqual => todo!(),
                    Operator::Greater => todo!(),
                    Operator::GreaterEqual => todo!(),
                    Operator::Lesser => todo!(),
                    Operator::LesserEqual => todo!(),
                    Operator::NotEqual => todo!(),
                    Operator::DoublePipe => todo!(),
                    Operator::DoubleAnd => todo!(),

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

pub fn compile(_ast: Ast, _pool: ExprPool) {
    let context = Context::create();
    let module = context.create_module("main");
    let f64_ty = context.f64_type();
    let builder = context.create_builder();

    let codegen = RecursiveBuilder::new(f64_ty, module, &builder, &context);
    codegen.emit_add_func();
    codegen.exec();
}

fn llvm_jit_exec(module: Module) {
    let exec_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    unsafe {
        type FloaPow = unsafe extern "C" fn() -> f64;

        let add: JitFunction<FloaPow> = exec_engine.get_function("power").unwrap();
        println!("{}", add.call());
    }
}
