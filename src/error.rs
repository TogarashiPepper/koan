use std::backtrace::Backtrace;
use thiserror::Error;

use crate::{
    lexer::{Operator, TokenType},
    value::ValTy,
};

pub type Result<T> = std::result::Result<T, KoanError>;

#[derive(Error, Debug, PartialEq)]
pub enum LexError {
    #[error("Expected token `{1}` after `{0}`")]
    PartialMultiCharToken(char, char),
    #[error("Character `{0}` is not a valid token")]
    IllegalCharInIdent(char),
    #[error("Character `{0}` is not allowed in user-defined identifiers")]
    InvalidToken(String),
    #[error("Unterminated string literal")]
    UntermStringLit,
}

#[derive(Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("Expected literal of type `{0}`")]
    ExpectedLiteral(String),
    #[error("Expected infix operator")]
    ExpectedInfixOp,
    #[error("Expected `{0:?}`, found EOF")]
    ExpectedFoundEof(TokenType),
    #[error("Expected `{0:?}`, found `{1:?}`")]
    ExpectedFound(TokenType, TokenType),
    #[error("Unexpected `{0:?}` token found")]
    Unexpected(TokenType),
}

#[derive(Error, Debug, PartialEq)]
pub enum InterpError {
    /// 1st parameter is the lhs, 2nd is the rhs.
    #[error("Cannot apply operator `{0:?}` to types `{1}` and `{2}`")]
    MismatchedTypes(Operator, ValTy, ValTy),
    #[error("Attempted to divide by zero")]
    DivByZero,
    #[error("Variable `{0}` is undefined")]
    UndefVar(String),
    #[error("Cannot apply unary operator `{0:?}` for type `{1}`")]
    MismatchedUnOp(Operator, ValTy),
    #[error("Function `{0}` is undefined")]
    UndefFunc(String),
    /// Fields are: name, received arity, expected arity
    #[error("Function `{0}` got {1} arguments but expected {2}")]
    MismatchedArity(String, usize, usize),
    #[error("Binary Operations on arrays must be on two arrays of equal length")]
    BinOpArrInvalidLength,
    #[error("Cannot shadow or reassign to `π`")]
    AssignmentToPi,
    #[error("Function `{0}` did not expect argument of type {1}")]
    InvalidParamTy(String, ValTy),
    #[error("Array of {0} elements would be too large")]
    RangeTooLarge(u64),
    #[error("Cannot define functions outside of the top level")]
    NonTopLevelFnDef,
    #[error("The conditional in an if expression must be of type `number`")]
    InvalidIfTy,
    #[error("The conditional in an if expression must be `1` or `0`")]
    InvalidIfNum,
}

#[derive(Error, Debug, PartialEq)]
pub enum CliError {
    #[error("Got error kind `{0}` when trying to read file")]
    FileError(std::io::ErrorKind),
}

/// For errors that are caused by malformed input or some kind of internal error in the VM.
/// Errors that are the fault of the user should be in `InterprError`
#[derive(Error, Debug, PartialEq)]
pub enum VmError {
    #[error("`{0}` is not a valid OpCode")]
    InvalidOpCode(u8),
    #[error("Tried to pop a value off the stack but it was empty")]
    StackEmpty,
    #[error("The program counter was out of bounds")]
    PcOutOfBounds,
}

#[derive(Debug, PartialEq)]
pub enum KoanErrorType {
    LexError(LexError),
    ParseError(ParseError),
    InterpError(InterpError),
    CliError(CliError),
    VmError(VmError),
}

macro_rules! gen_from {
    ($tyname:ident) => {
        impl From<$tyname> for KoanError {
            fn from(value: $tyname) -> KoanError {
                KoanError(KoanErrorType::$tyname(value), Backtrace::capture())
            }
        }
    };
}

gen_from!(ParseError);
gen_from!(LexError);
gen_from!(InterpError);
gen_from!(CliError);
gen_from!(VmError);

#[derive(Debug)]
pub struct KoanError(pub KoanErrorType, pub std::backtrace::Backtrace);

impl PartialEq for KoanError {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

pub fn handle_err(err: KoanError) -> String {
    match err.0 {
        KoanErrorType::LexError(e) => format!("{e}"),
        KoanErrorType::ParseError(e) => format!("{e}"),
        KoanErrorType::InterpError(e) => {
            format!("value: {e}\nbacktrace: {}", err.1)
        }
        KoanErrorType::CliError(e) => format!("{e}"),
        KoanErrorType::VmError(e) => {
            format!("{e}, if you're seeing this it's likely a bug in the compiler")
        }
    }
}
