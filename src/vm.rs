use crate::{
    error::{KoanError, Result, VmError},
    value::Value,
};

use std::ops::{Add, Div, Mul, Neg, Not, Sub};

#[repr(u8)]
pub enum OpCode {
    Add,
    Sub,
    Mul,
    Div,
    Sqrt,
    Pow,
    Floor,
    Abs,
    // TODO: figure out how that works
    Load,
}

impl TryFrom<u8> for OpCode {
    type Error = KoanError;

    fn try_from(value: u8) -> Result<Self> {
        if value <= 8 {
            unsafe {
                // SAFETY: OpCode only has 9 elements so we ensure `value` is in the 0..9 range
                Ok(std::mem::transmute::<u8, OpCode>(value))
            }
        } else {
            Err(VmError::InvalidOpCode(value).into())
        }
    }
}

pub struct VM {
    pub chunk: Vec<u8>,
    pub pc: usize,
    pub stack: Vec<Value>,
}

impl VM {
    // TODO: ctx for in between runs? i.e for use in repl
    pub fn run(mut self) -> Result<()> {
        loop {
            let Some(byte) = self.chunk.get(self.pc).copied() else {
                break;
            };

            let op_code: OpCode = byte.try_into()?;

            match op_code {
                OpCode::Add => self.bin_op(Value::add)?,
                OpCode::Sub => self.bin_op(Value::sub)?,
                OpCode::Mul => self.bin_op(Value::mul)?,
                OpCode::Div => self.bin_op(Value::div)?,
                OpCode::Pow => self.bin_op(Value::pow)?,
                OpCode::Abs => self.un_op(Value::abs)?,
                OpCode::Sqrt => self.un_op(Value::sqrt)?,
                OpCode::Floor => todo!(),
                OpCode::Load => todo!(),
            }

            self.pc += 1;
        }

        Ok(())
    }

    fn pop(&mut self) -> Result<Value> {
        self.stack.pop().ok_or_else(|| VmError::StackEmpty.into())
    }

    fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    fn bin_op<F>(&mut self, op: F) -> Result<()>
    where
        F: FnOnce(Value, Value) -> Result<Value>,
    {
        let b = self.pop()?;
        let a = self.pop()?;

        let res = op(a, b)?;
        self.push(res);

        Ok(())
    }

    fn un_op<F>(&mut self, op: F) -> Result<()>
    where
        F: FnOnce(&Value) -> Result<Value>,
    {
        let a = self.pop()?;
        let res = op(&a)?;

        self.push(res);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    // TODO: write tests for the vm
}
