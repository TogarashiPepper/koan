use std::backtrace::Backtrace;
use thiserror::Error;

use crate::lexer::{Operator, TokenType};

pub type Result<T> = std::result::Result<T, KoanError>;

#[derive(Error, Debug, PartialEq)]
pub enum LexError {
    #[error("Expected token `{1}` after `{0}`")]
    PartialMultiCharToken(char, char),
    #[error("Character `{0}` is not a valid token")]
    IllegalCharInIdent(char),
    #[error("Character `{0}` is not allowed in user-defined identifiers")]
    InvalidToken(String),
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
pub enum InterpreterError {
    /// 1st parameter is the lhs, 2nd is the rhs.
    #[error("Cannot apply operator `{0:?}` to types `{1}` and `{2}`")]
    MismatchedTypes(Operator, &'static str, &'static str),
    #[error("Attempted to divide by zero")]
    DivByZero,
    #[error("Variable `{0}` is undefined")]
    UndefVar(String),
    #[error("Cannot apply unary operator `{0:?}` for type `{1}`")]
    MismatchedUnOp(Operator, &'static str),
    #[error("Function `{0}` is undefined")]
    UndefFunc(String),
    /// Fields are: name, received arity, expected arity
    #[error("Function `{0}` got {1} arguments but expected {2}")]
    MismatchedArity(String, usize, usize),
    #[error(
        "Binary Operations on arrays must be on two arrays of equal length"
    )]
    BinOpArrInvalidLength,
    #[error("Cannot shadow or reassign to `π`")]
    AssignmentToPi,
    #[error("Function `{0}` did not expect argument of type {1}")]
    InvalidParamTy(String, &'static str),
    #[error("Array of {0} elements would be too large")]
    RangeTooLarge(u64),
}

#[derive(Error, Debug, PartialEq)]
pub enum CliError {
    #[error("Got error kind `{0}` when trying to read file")]
    FileError(std::io::ErrorKind),
}

#[derive(Debug, PartialEq)]
pub enum KoanErrorType {
    LexErr(LexError),
    ParseErr(ParseError),
    InterpErr(InterpreterError),
    CliErr(CliError),
}

impl From<ParseError> for KoanError {
    fn from(value: ParseError) -> Self {
        KoanError(KoanErrorType::ParseErr(value), Backtrace::capture())
    }
}

impl From<LexError> for KoanError {
    fn from(value: LexError) -> Self {
        KoanError(KoanErrorType::LexErr(value), Backtrace::capture())
    }
}

impl From<InterpreterError> for KoanError {
    fn from(value: InterpreterError) -> Self {
        KoanError(KoanErrorType::InterpErr(value), Backtrace::capture())
    }
}

impl From<CliError> for KoanError {
    fn from(value: CliError) -> Self {
        KoanError(KoanErrorType::CliErr(value), Backtrace::capture())
    }
}

#[derive(Debug)]
pub struct KoanError(pub KoanErrorType, pub std::backtrace::Backtrace);

impl PartialEq for KoanError {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

pub fn handle_err(err: KoanError) -> String {
    match err.0 {
        KoanErrorType::LexErr(e) => format!("{e}"),
        KoanErrorType::ParseErr(e) => format!("{e}"),
        KoanErrorType::InterpErr(e) => format!("{e}"),
        KoanErrorType::CliErr(e) => format!("{e}"),
    }
}
