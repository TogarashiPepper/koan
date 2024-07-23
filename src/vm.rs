use crate::value::Value;

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

pub struct VM {
    pub chunk: Vec<u64>,
    pub pc: usize,
    pub stack: Vec<Value>,
}

