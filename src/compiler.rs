use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    execution_engine::JitFunction,
    module::Module,
    types::FloatType,
    values::{BasicMetadataValueEnum, BasicValue, FloatValue},
    FloatPredicate, OptimizationLevel,
};

use crate::{
    lexer::{lex, Operator},
    parser::{parse, Ast},
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
        let bin_intrinsic_type = f64_t.fn_type(&[f64_t.into(), f64_t.into()], false);
        let un_intrinsic_type = f64_t.fn_type(&[f64_t.into()], false);
        module.add_function("llvm.pow.f64", bin_intrinsic_type, None);
        module.add_function("llvm.sqrt.f64", un_intrinsic_type, None);

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

    pub fn emit_main_func(&self, ast: Ast, pool: &ExprPool) {
        let fn_type = self.f64_t.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        let Ast::Expression(ast) = ast else {
            unreachable!()
        };
        let sum = self.build(ast, pool).unwrap();

        self.builder.build_return(Some(&sum)).unwrap();

        self.module.verify().unwrap();
    }

    // Calls an intrinsic that takes two args
    fn emit_intrinsic_call<'b, 'c: 'b>(
        &'b self,
        args: &[BasicMetadataValueEnum<'c>],
        name: &'static str,
    ) -> Result<FloatValue, BuilderError> {
        let intrinsic = self.module.get_function(name).unwrap();
        let res = self
            .builder
            .build_direct_call(intrinsic, args, "ret")?
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_float_value();

        Ok(res)
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
                    Operator::PiTimes => self.builder.build_float_mul(
                        self.f64_t.const_float(std::f64::consts::PI),
                        child,
                        "ret",
                    ),
                    Operator::Sqrt => {
                        self.emit_intrinsic_call(&[child.into()], "llvm.sqrt.f64")
                    }
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
                    Operator::Power => self
                        .emit_intrinsic_call(&[lhs.into(), rhs.into()], "llvm.pow.f64"),
                    Operator::Plus => self.builder.build_float_add(lhs, rhs, "ret"),
                    Operator::Minus => self.builder.build_float_sub(lhs, rhs, "ret"),
                    Operator::Times => self.builder.build_float_mul(lhs, rhs, "ret"),
                    Operator::Slash => self.builder.build_float_div(lhs, rhs, "ret"),
                    Operator::DoubleEqual
                    | Operator::Greater
                    | Operator::GreaterEqual
                    | Operator::Lesser
                    | Operator::LesserEqual
                    | Operator::NotEqual => {
                        // TODO: implement tolerant comparision in llvm ir
                        let cmp_var = match op {
                            Operator::DoubleEqual => FloatPredicate::OEQ,
                            Operator::Greater => FloatPredicate::OGT,
                            Operator::GreaterEqual => FloatPredicate::OGE,
                            Operator::Lesser => FloatPredicate::OLT,
                            Operator::LesserEqual => FloatPredicate::OLE,
                            Operator::NotEqual => FloatPredicate::ONE,

                            _ => unreachable!(),
                        };

                        let ret_i = self
                            .builder
                            .build_float_compare(cmp_var, lhs, rhs, "ret")?
                            .as_basic_value_enum()
                            .into_int_value();

                        let ret = self
                            .builder
                            .build_signed_int_to_float(ret_i, self.f64_t, "ret")?;
                        Ok(ret)
                    }
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
    let (ast, pool) = lex("√100").and_then(parse).unwrap();
    codegen.emit_main_func(ast[0].clone(), &pool);
    codegen.exec();
}

fn llvm_jit_exec(module: Module) {
    let exec_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    module.print_to_stderr();

    unsafe {
        type FloaPow = unsafe extern "C" fn() -> f64;

        let add: JitFunction<FloaPow> = exec_engine.get_function("main").unwrap();
        println!("{}", add.call());
    }
}
