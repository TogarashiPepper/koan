use crate::{
    error::{InterpError, KoanError, Result, VmError},
    lexer::Operator,
    value::Value,
};

use std::{
    collections::HashMap,
    f64::consts::PI,
    ops::{Add, Div, Mul, Sub},
    rc::Rc,
};

#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum OpCode {
    Add,
    Sub,
    Mul,
    Div,
    Sqrt,
    Pow,
    Floor,
    Abs,
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
    /// pops a value off the stack
    Discard,
    Load,
    DefineGlobal,
    GetGlobal,
    DebugStack,
    GetLocal,
    DiscardUnder,
    CreateArray,
}

impl TryFrom<u8> for OpCode {
    type Error = KoanError;

    fn try_from(value: u8) -> Result<Self> {
        if value < 27 {
            unsafe {
                // SAFETY: OpCode only has 20 elements so we ensure `value` is in the 0..9 range
                Ok(std::mem::transmute::<u8, OpCode>(value))
            }
        } else {
            Err(VmError::InvalidOpCode(value).into())
        }
    }
}

#[derive(Debug)]
pub struct VM {
    pub chunk: Vec<u8>,
    // TODO: limit to 255 so a `load` can have a 1byte param
    // Maybe use an array(?)
    pub data: Vec<Value>,
    pub pc: usize,
    pub stack: Vec<Value>,
    pub globals: HashMap<String, Value>,
}

impl VM {
    pub fn new() -> Self {
        VM {
            chunk: vec![],
            data: vec![],
            pc: 0,
            stack: vec![],
            globals: HashMap::new(),
        }
    }

    pub fn with_globals(globals: HashMap<String, Value>) -> Self {
        Self {
            globals,
            ..Default::default()
        }
    }

    pub fn dbg_chunk(chunk: &[u8]) {
        let mut skip_conv = false;

        println!("[");
        for ins in chunk {
            if !skip_conv {
                let op = OpCode::try_from(*ins).unwrap();
                if matches!(
                    op,
                    OpCode::DefineGlobal
                        | OpCode::GetGlobal
                        | OpCode::GetLocal
                        | OpCode::Load
                ) {
                    skip_conv = true;
                }

                println!("\t{op:?},");
            } else {
                println!("\t{ins},");
                skip_conv = false;
            }
        }

        println!("]");
    }

    pub fn calc_stack_effect(chunk: &[u8]) -> i64 {
        let mut effect = 0;

        let mut idx = 0;
        loop {
            let Some(ins) = chunk.get(idx).copied() else {
                break;
            };

            idx += 1;

            effect += match OpCode::try_from(ins).unwrap() {
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
                | OpCode::And
                | OpCode::Discard
                | OpCode::DiscardUnder
                | OpCode::Print => -1,
                OpCode::Not
                | OpCode::Sqrt
                | OpCode::Floor
                | OpCode::PiTimes
                | OpCode::Abs
                | OpCode::DebugStack => 0,
                OpCode::Load | OpCode::GetGlobal | OpCode::GetLocal => {
                    idx += 1;
                    1
                }
                OpCode::DefineGlobal => {
                    idx += 1;
                    -1
                }
                OpCode::CreateArray => {
                    let len = chunk[idx] as i64;
                    idx += 1;

                    1 - len
                }
            }
        }

        effect
    }

    fn run_instruction(&mut self, ins: u8) -> Result<()> {
        let op_code: OpCode = ins.try_into()?;

        match op_code {
            OpCode::Add => self.bin_op(Value::add)?,
            OpCode::Sub => self.bin_op(Value::sub)?,
            OpCode::Mul => self.bin_op(Value::mul)?,
            OpCode::Div => self.bin_op(Value::div)?,
            OpCode::Pow => self.bin_op(Value::pow)?,
            OpCode::Abs => self.un_op(Value::abs)?,
            OpCode::Sqrt => self.un_op(Value::sqrt)?,
            OpCode::Floor => {
                let v = self.stack.pop().ok_or(VmError::StackEmpty)?;
                let floored = v.in_num("floor", |f| f.floor())?;

                self.stack.push(floored);
            }
            OpCode::Load => {
                let cnst_idx = self
                    .read_byte()
                    .ok_or(VmError::MissingParameter(OpCode::Load))?;

                self.push(self.data.get(cnst_idx as usize).unwrap().clone());
            }
            OpCode::Print => match self.pop()? {
                Value::Num(x) => println!("{}", x),
                Value::UTF8(x) => println!("{}", x),
                Value::Array(x) => println!("{:?}", x),
                Value::Nothing => println!("nothing"),
            },
            OpCode::Eq => self.bin_op(|l, r| Ok(Value::Num(f64::from(l == r))))?,
            OpCode::Neq => self.bin_op(|l, r| Ok(Value::Num(f64::from(l != r))))?,
            OpCode::Greater => self.bin_op(|l, r| Ok(Value::Num(f64::from(l > r))))?,
            OpCode::GreaterEq => self.bin_op(|l, r| Ok(Value::Num(f64::from(l >= r))))?,
            OpCode::Lesser => self.bin_op(|l, r| Ok(Value::Num(f64::from(l < r))))?,
            OpCode::LesserEq => self.bin_op(|l, r| Ok(Value::Num(f64::from(l <= r))))?,
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
            OpCode::Discard => {
                if self.stack.pop().is_none() {
                    return Err(VmError::StackEmpty.into());
                }
            }
            OpCode::PiTimes => self.un_op(|l| l * Value::Num(PI))?,
            OpCode::DefineGlobal => {
                let idx = self.read_byte().ok_or(VmError::MissingParameter(op_code))?;

                let Value::UTF8(name) = self.data[idx as usize].clone() else {
                    panic!("DefineGlobal data idx wasn't a str value");
                };

                let val = self.stack.pop().ok_or(VmError::StackEmpty)?;

                #[allow(clippy::map_entry)]
                // Clippy suggestion forces us to move name, which makes the else case fail
                if !self.globals.contains_key(&name) {
                    self.globals.insert(name, val);
                } else {
                    return Err(VmError::GlobalAlreadyDefined(name).into());
                }
            }
            OpCode::GetGlobal => {
                let idx = self.read_byte().ok_or(VmError::MissingParameter(op_code))?;

                let Value::UTF8(name) = &self.data[idx as usize] else {
                    panic!("GetGlobal data idx wasn't a str value");
                };

                let val = self
                    .globals
                    .get(name)
                    .cloned()
                    .ok_or_else(|| InterpError::UndefVar(name.to_owned()))?;

                self.stack.push(val);
            }
            OpCode::DebugStack => {
                println!("{:?}", self.stack);
            }
            OpCode::GetLocal => {
                let slot = self.read_byte().ok_or(VmError::MissingParameter(op_code))?;

                let x = self.stack.get(slot as usize).unwrap();
                self.stack.push(x.clone());
            }
            OpCode::DiscardUnder => {
                self.stack.remove(self.stack.len() - 2);
            }
            OpCode::CreateArray => {
                let len = self.read_byte().ok_or(VmError::MissingParameter(op_code))?;

                let new = self.stack.split_off(self.stack.len() - len as usize);

                self.stack.push(Value::Array(Rc::new(new)));
            }
            OpCode::Not => todo!(),
        }

        Ok(())
    }

    pub fn run(&mut self) -> Result<()> {
        loop {
            let Some(byte) = self.read_byte() else {
                break;
            };

            self.run_instruction(byte)?;
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
        F: FnOnce(Value) -> Result<Value>,
    {
        let a = self.pop()?;
        let res = op(a)?;

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
            ..Default::default()
        };

        vm.run().unwrap();

        assert_eq!(vm.stack, vec![Value::Num(42.0)]);
    }
}
