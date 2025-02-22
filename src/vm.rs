use crate::{
    error::{InterpError, KoanError, Result, VmError},
    lexer::Operator,
    value::Value,
};

use std::ops::{Add, Div, Mul, Neg, Not, Sub};

#[repr(u8)]
#[derive(Debug, PartialEq, Eq)]
pub enum OpCode {
    Add,
    Sub,
    Mul,
    Div,
    Sqrt,
    Pow,
    Floor,
    Abs,
    Load,
    Print,
    Eq,
    Neq,
    Greater,
    GreaterEq,
    Lesser,
    LesserEq,
    Or,
    And,
    Not,
    PiTimes,
}

impl TryFrom<u8> for OpCode {
    type Error = KoanError;

    fn try_from(value: u8) -> Result<Self> {
        if value < 20 {
            unsafe {
                // SAFETY: OpCode only has 20 elements so we ensure `value` is in the 0..9 range
                Ok(std::mem::transmute::<u8, OpCode>(value))
            }
        } else {
            Err(VmError::InvalidOpCode(value).into())
        }
    }
}

pub struct VM {
    pub chunk: Vec<u8>,
    // TODO: limit to 255 so a `load` can have a 1byte param
    // Maybe use an array(?)
    pub data: Vec<Value>,
    pub pc: usize,
    pub stack: Vec<Value>,
}

impl VM {
    pub fn new() -> Self {
        VM {
            chunk: vec![],
            data: vec![],
            pc: 0,
            stack: vec![],
        }
    }

    pub fn calc_stack_effect(&self) -> i64 {
        let mut skip = false;
        let mut effect = 0;

        for ins in &self.chunk {
            if skip {
                skip = false;
                continue;
            }

            effect += match OpCode::try_from(*ins).unwrap() {
                OpCode::Add
                | OpCode::Sub
                | OpCode::Mul
                | OpCode::Div
                | OpCode::Pow
                | OpCode::Eq
                | OpCode::Neq
                | OpCode::Greater
                | OpCode::GreaterEq
                | OpCode::Lesser
                | OpCode::LesserEq
                | OpCode::Or
                | OpCode::And => -1,
                OpCode::Not
                | OpCode::Sqrt
                | OpCode::Floor
                | OpCode::Print
                | OpCode::PiTimes
                | OpCode::Abs => 0,
                OpCode::Load => {
                    skip = true;
                    1
                }
            }
        }

        effect
    }

    // TODO: ctx for in between runs? i.e for use in repl
    pub fn run(&mut self) -> Result<()> {
        loop {
            let Some(byte) = self.read_byte() else {
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
                OpCode::Load => {
                    let cnst_idx = self
                        .read_byte()
                        .ok_or(VmError::MissingParameter(OpCode::Load))?;

                    self.push(self.data.get(cnst_idx as usize).unwrap().clone());
                }
                OpCode::Print => println!("{:?}", self.pop()?),
                OpCode::Eq => self.bin_op(|l, r| Ok(Value::Num(f64::from(l == r))))?,
                OpCode::Neq => self.bin_op(|l, r| Ok(Value::Num(f64::from(l != r))))?,
                OpCode::Greater => {
                    self.bin_op(|l, r| Ok(Value::Num(f64::from(l > r))))?
                }
                OpCode::GreaterEq => {
                    self.bin_op(|l, r| Ok(Value::Num(f64::from(l >= r))))?
                }
                OpCode::Lesser => self.bin_op(|l, r| Ok(Value::Num(f64::from(l < r))))?,
                OpCode::LesserEq => {
                    self.bin_op(|l, r| Ok(Value::Num(f64::from(l <= r))))?
                }
                OpCode::Or | OpCode::And => {
                    let b = self.pop()?;
                    let a = self.pop()?;

                    match (&b, &a) {
                        (Value::Num(l), Value::Num(r)) => {
                            todo!()
                        }
                        _ => {
                            return Err(InterpError::MismatchedTypes(
                                if op_code == OpCode::Or {
                                    Operator::DoublePipe
                                } else {
                                    Operator::DoubleAnd
                                },
                                a.ty_str(),
                                b.ty_str(),
                            )
                            .into())
                        }
                    }
                }
                OpCode::Not => todo!(),
                OpCode::PiTimes => todo!(),
            }
        }

        Ok(())
    }

    fn read_byte(&mut self) -> Option<u8> {
        let byte = self.chunk.get(self.pc).copied();
        self.pc += 1;

        byte
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

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::value::Value;

    use super::{OpCode, VM};

    #[test]
    fn load() {
        let mut vm = VM {
            chunk: vec![OpCode::Load as u8, 0],
            data: vec![Value::Num(42.0)],
            pc: 0,
            stack: vec![],
        };

        vm.run().unwrap();

        assert_eq!(vm.stack, vec![Value::Num(42.0)]);
    }
}
