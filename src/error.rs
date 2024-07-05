use std::backtrace::Backtrace;

use crate::lexer::{Operator, TokenType};

pub type Result<T> = std::result::Result<T, KoanError>;

#[derive(Debug, PartialEq)]
pub enum LexError {
    PartialMultiCharToken(char, char),
    InvalidToken(String),
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    ExpectedLiteral(String),
    ExpectedInfixOp,
    ExpectedFoundEof(TokenType),
    ExpectedFound(TokenType, TokenType),
    Unexpected(TokenType),
}

#[derive(Debug, PartialEq)]
pub enum InterpreterError {
    /// 1st parameter is the lhs, 2nd is the rhs.
    MismatchedTypes(Operator, &'static str, &'static str),
    DivByZero,
    UndefVar(String),
    MismatchedUnOp(Operator, &'static str),
    UndefFunc(String),
    /// Fields are: name, received arity, expected arity
    MismatchedArity(String, usize, usize),
}

#[derive(Debug, PartialEq)]
pub enum CliError {
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
        KoanErrorType::LexErr(lerr) => match lerr {
            LexError::PartialMultiCharToken(f, s) => {
                format!("Expected token `{s}` after `{f}`")
            }
            LexError::InvalidToken(tok) => format!("Character `{tok}` is not a valid token"),
        },
        KoanErrorType::ParseErr(perr) => match perr {
            ParseError::ExpectedLiteral(ty) => format!("Expected literal of type `{ty}`"),
            ParseError::ExpectedInfixOp => "Expected infix operator".to_owned(),
            ParseError::ExpectedFoundEof(expected) => {
                format!("Expected `{expected:?}`, found EOF")
            }
            ParseError::ExpectedFound(e, f) => format!("Expected `{e:?}`, found `{f:?}`"),
            ParseError::Unexpected(unexpected) => {
                format!("Unexpected `{unexpected:?}` token found")
            }
        },
        KoanErrorType::InterpErr(ierr) => match ierr {
            InterpreterError::MismatchedTypes(op, l, r) => {
                format!("Cannot apply operator `{op:?}` to types `{l}` and `{r}`")
            }
            InterpreterError::DivByZero => "Attempted to divide by zero".to_owned(),
            InterpreterError::UndefVar(varname) => {
                format!("Variable `{varname}` is undefined")
            }
            InterpreterError::MismatchedUnOp(op, ty) => {
                format!("Cannot apply unary operator `{op:?}` for type `{ty}`")
            }
            InterpreterError::UndefFunc(fnname) => format!("Function `{fnname}` is undefined"),
            InterpreterError::MismatchedArity(name, got, expected) => {
                format!("Function `{name}` got {got} arguments but expected {expected}")
            }
        },
        KoanErrorType::CliErr(cerr) => match cerr {
            CliError::FileError(ioerr) => {
                format!("Got error kind `{ioerr}` when trying to read file")
            }
        },
    }
}

