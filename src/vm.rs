use crate::{
    compiler::Compiler, error::{InterpError, KoanError, Result, VmError}, lexer::Operator, value::{Function, Value}
};

use std::{
    collections::HashMap,
    f64::consts::PI,
    io::Write,
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
    JumpIfFalse,
    Jump,
}

impl TryFrom<u8> for OpCode {
    type Error = KoanError;

    fn try_from(value: u8) -> Result<Self> {
        if value < 29 {
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
    // TODO: limit to 255 so a `load` can have a 1byte param
    // Maybe use an array(?)
    pub data: Vec<Value>,
    pub pc: usize,
    pub stack: Vec<Value>,
    pub globals: HashMap<String, Value>,

    pub functions: Vec<Function>,
    pub current_fn: usize,
}

impl VM {
    pub fn with_globals(globals: HashMap<String, Value>) -> Self {
        let mut def = Compiler::new().finish();

        def.globals.extend(globals);

        def
    }

    fn chunk(&self) -> &[u8] {
        &self.functions[self.current_fn].chunk
    }

    pub fn dbg_chunk(&self) {
        let mut idx = 0;
        let chunk = self.chunk();
        eprintln!("chunk.len: {}", chunk.len());
        eprintln!("chunk: {chunk:?}");
        eprintln!("self.data: {:?}", self.data);

        loop {
            if idx >= chunk.len() {
                break;
            }

            let byte = match OpCode::try_from(chunk[idx]) {
                Ok(b) => b,
                Err(e) => {
                    println!("idx: {idx}");
                    println!("{chunk:?}");
                    println!("{e:?}");

                    std::process::exit(1);
                }
            };

            eprint!("idx: {idx}; ");
            eprint!("{byte:?}\t");

            match byte {
                OpCode::Add
                | OpCode::Sub
                | OpCode::Mul
                | OpCode::Div
                | OpCode::Sqrt
                | OpCode::Pow
                | OpCode::Floor
                | OpCode::Abs
                | OpCode::Print
                | OpCode::Eq
                | OpCode::Neq
                | OpCode::Greater
                | OpCode::GreaterEq
                | OpCode::Lesser
                | OpCode::LesserEq
                | OpCode::Or
                | OpCode::And
                | OpCode::Not
                | OpCode::PiTimes
                | OpCode::Discard
                | OpCode::DiscardUnder => {
                    eprintln!();
                }
                OpCode::Load => {
                    idx += 1;

                    eprintln!("{:?}", self.data[chunk[idx] as usize]);
                }
                OpCode::GetGlobal | OpCode::DefineGlobal => {
                    idx += 1;

                    eprintln!("name: {}", self.data[chunk[idx] as usize]);
                }
                OpCode::GetLocal => {
                    idx += 1;

                    eprintln!("stack_idx: {}", chunk[idx]);
                }
                OpCode::JumpIfFalse | OpCode::Jump => {
                    let offset = u16::from_le_bytes([chunk[idx + 1], chunk[idx + 2]]);

                    eprintln!("offset: {offset}");

                    idx += 2;
                }
                OpCode::DebugStack => todo!(),
                OpCode::CreateArray => todo!(),
            }

            idx += 1;
        }

        eprintln!();
    }

    pub fn calc_stack_effect(chunk: &[u8]) -> i64 {
        let mut effect: i64 = 0;

        let mut idx = 0;
        loop {
            let Some(ins) = chunk.get(idx).copied() else {
                break;
            };

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
                OpCode::JumpIfFalse | OpCode::Jump => {
                    idx += 2;
                    0
                }
                OpCode::CreateArray => {
                    let len = u16::from_le_bytes([chunk[idx], chunk[idx + 1]]);
                    idx += 2;

                    1 - len as i64
                }
            };

            idx += 1;
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
            OpCode::Print => println!("{}", self.pop()?),
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
                        .into());
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
                // TODO: file/check if bug in clippy
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
                let len = self.read_u16().ok_or(VmError::MissingParameter(op_code))?;

                let new = self.stack.split_off(self.stack.len() - len as usize);

                self.stack.push(Value::Array(Rc::new(new)));
            }
            OpCode::JumpIfFalse => {
                let offset = self.read_u16().ok_or(VmError::MissingParameter(op_code))?;
                let last = self.stack.last();

                match last {
                    Some(Value::Num(0.0)) => self.pc += offset as usize,
                    Some(Value::Num(1.0)) => {}
                    Some(_) | None => return Err(InterpError::InvalidIfNum.into()),
                }

                if let Some(Value::Num(0.0)) = self.stack.last() {
                    self.pc += offset as usize;
                }
            }
            OpCode::Jump => {
                let offset = self.read_u16().ok_or(VmError::MissingParameter(op_code))?;

                self.pc += offset as usize;
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
        let byte = self.chunk().get(self.pc).copied();
        // TODO: make pc for the current function
        self.pc += 1;

        byte
    }

    fn read_u16(&mut self) -> Option<u16> {
        let b1 = self.chunk().get(self.pc).copied();
        let b2 = self.chunk().get(self.pc + 1).copied();

        // TODO: make pc for the current function
        self.pc += 2;

        b1.zip(b2).map(|(l, r)| u16::from_le_bytes([l, r]))
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
        Compiler::new().finish()
    }
}

