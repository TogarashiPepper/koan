use inkwell::{
    builder::{Builder, BuilderError}, context::Context, execution_engine::JitFunction, memory_buffer::MemoryBuffer, module::Module, types::{BasicTypeEnum, FloatType}, values::{
        BasicMetadataValueEnum, BasicValue, FloatValue, FunctionValue, GlobalValue,
    }, AddressSpace, FloatPredicate, OptimizationLevel
};

use crate::{
    lexer::{lex, Operator},
    parser::{parse, Ast},
    pool::{Expr, ExprPool, ExprRef},
};

fn create_printf<'a>(
    context: &'a Context,
    module: &Module<'a>,
) -> (FunctionValue<'a>, GlobalValue<'a>) {
    let printf_format = "%f\n";
    let printf_format_type = context
        .i8_type()
        .array_type((printf_format.len() + 1) as u32);
    let printf_format_global =
        module.add_global(printf_format_type, None, "write_format");

    printf_format_global
        .set_initializer(&context.const_string(printf_format.as_bytes(), true));

    let printf_args = [context.ptr_type(AddressSpace::default()).into()];

    let printf_type = context.f32_type().fn_type(&printf_args, true);
    let printf_fn = module.add_function("printf", printf_type, None);

    (printf_fn, printf_format_global)
}

struct RecursiveBuilder<'a> {
    f64_t: FloatType<'a>,
    module: Module<'a>,
    context: &'a Context,
    builder: Builder<'a>,
    pool: ExprPool,
    printf: (FunctionValue<'a>, GlobalValue<'a>),
}

impl<'a> RecursiveBuilder<'a> {
    pub fn new(context: &'a Context, pool: ExprPool) -> Self {
        let module = context.create_module("main");
        let f64_t = context.f64_type();
        let builder = context.create_builder();
        let printf = create_printf(context, &module);

        let bin_intrinsic_type = f64_t.fn_type(&[f64_t.into(), f64_t.into()], false);
        let un_intrinsic_type = f64_t.fn_type(&[f64_t.into()], false);
        module.add_function("llvm.pow.f64", bin_intrinsic_type, None);
        module.add_function("llvm.sqrt.f64", un_intrinsic_type, None);
        module.add_function("llvm.fabs.f64", un_intrinsic_type, None);

        // TODO: Handle error
        let mut bytes = std::fs::read("./stdlib/stdlib.ll").unwrap();

        // Append null byte for llvm since it expects null terminated strings
        bytes.push(0);

        let mem_buffer = MemoryBuffer::create_from_memory_range_copy(&bytes, "stdmembuffer");
        let std_module = context.create_module_from_ir(mem_buffer).unwrap();

        module.link_in_module(std_module).unwrap();

        Self {
            f64_t,
            module,
            builder,
            context,
            pool,
            printf,
        }
    }

    pub fn exec(self) {
        llvm_jit_exec(self.module);
    }

    fn emit_print<'c, 'b: 'c>(&'b self, value: ExprRef) -> Result<(), BuilderError> {
        let value = self.build(value)?;

        let args: &[BasicMetadataValueEnum<'c>] =
            &[self.printf.1.as_pointer_value().into(), value.into()];

        self.builder
            .build_call(self.printf.0, args, "write_call")
            .unwrap();

        Ok(())
    }

    pub fn emit_main_func(&self, ast: Ast) {
        let fn_type = self.f64_t.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        let Ast::Expression(ast) = ast else {
            unreachable!()
        };
        let sum = self.build(ast).unwrap();

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

    pub fn build(&self, ast: ExprRef) -> Result<FloatValue, BuilderError> {
        let exp_ref = self.pool.get(ast);

        match exp_ref {
            Expr::NumLit(n) => Ok(self.f64_t.const_float(*n)),
            Expr::PreOp { op, rhs } => {
                let op = *op;
                let child = self.build(*rhs)?;

                match op {
                    Operator::PiTimes => self.builder.build_float_mul(
                        self.f64_t.const_float(std::f64::consts::PI),
                        child,
                        "ret",
                    ),
                    Operator::Sqrt => {
                        self.emit_intrinsic_call(&[child.into()], "llvm.sqrt.f64")
                    }
                    Operator::Abs => {
                        self.emit_intrinsic_call(&[child.into()], "llvm.fabs.f64")
                    }
                    Operator::Minus => self.builder.build_float_neg(child, "negation"),
                    Operator::Not => todo!(),

                    _ => unreachable!(),
                }
            }

            Expr::BinOp { lhs, op, rhs } => {
                let op = *op;
                let lhs = self.build(*lhs)?;
                let rhs = self.build(*rhs)?;

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
                        // TODO: implement tolerant comparison in llvm ir
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
            Expr::FunCall(name, args) => match name.as_str() {
                "print" => {
                    for arg in args {
                        self.emit_print(*arg)?;
                    }

                    Ok(self.f64_t.const_zero())
                }

                _ => todo!(),
            },
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
    let (ast, pool) = lex("|-1|").and_then(parse).unwrap();
    let context = Context::create();
    let codegen = RecursiveBuilder::new(&context, pool);
    codegen.emit_main_func(ast[0].clone());
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
