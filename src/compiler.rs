use inkwell::{
    builder::Builder, context::Context, execution_engine::JitFunction, module::Module, types::{FloatType, IntType}, values::{FloatValue, IntValue}, OptimizationLevel
};

use crate::{lexer::Operator, parser::Ast, pool::{Expr, ExprPool, ExprRef}};

struct RecursiveBuilder<'a> {
    f64_t: FloatType<'a>,
    builder: &'a Builder<'a>,
}

impl<'a> RecursiveBuilder<'a> {
    pub fn new(f64_t: FloatType<'a>, builder: &'a Builder<'a>) -> Self {
        Self { f64_t, builder }
    }

    pub fn build(&self, ast: ExprRef, pool: ExprPool) -> FloatValue {
        let exp_ref = pool.get(ast);

        match exp_ref {
            Expr::NumLit(n) => self.f64_t.const_float(*n),
            Expr::PreOp { op, rhs } => {
                let op = *op;
                let child = self.build(*rhs, pool);

                match op {
                    Operator::PiTimes => todo!(),
                    Operator::Sqrt => todo!(),
                    Operator::Minus => todo!(),
                    Operator::Not => todo!(),

                    _ => unreachable!()
                }
            },

            Expr::BinOp { lhs, op, rhs } => todo!(),
            Expr::Ident(_) => todo!(),
            Expr::StrLit(_) => todo!(),
            Expr::FunCall(_, _) => todo!(),
            Expr::Array(_) => todo!(),
            Expr::IfElse { cond, body, else_body } => todo!(),
        }
    }
}

// fn compile(ast: Ast, pool: ExprPool) {
//     let context = Context::create();
//     let module = context.create_module("addition");
//     let i32_ty = context.i32_type();
//
//     let fn_ty = i32_ty.fn_type(&[i32_ty.into(), i32_ty.into()], false);
//     let fn_val = module.add_function("add", fn_ty, None);
//     let entry_basic_block = context.append_basic_block(fn_val, "entry");
//
//     let builder = context.create_builder();
//     builder.position_at_end(entry_basic_block);
//
//     let x = fn_val.get_nth_param(0).unwrap().into_int_value();
//     let y = fn_val.get_nth_param(1).unwrap().into_int_value();
//
//     let ret = builder.build_int_add(x, y, "add").unwrap();
//     let return_instruction = builder.build_return(Some(&ret)).unwrap();
// }
//
// fn llvm_jit_exec(module: Module) {
//     let exec_engine = module
//         .create_jit_execution_engine(OptimizationLevel::Aggressive)
//         .unwrap();
//
//     unsafe {
//         type Addition = unsafe extern "C" fn(i32, i32) -> i32;
//
//         let add: JitFunction<Addition> =
//             exec_engine.get_function("add").unwrap();
//         let x = 1;
//         let y = 2;
//         assert_eq!(add.call(x, y), x + y);
//     }
// }
