use std::backtrace::Backtrace;

use crate::lexer::{Operator, TokenType};

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
    FileError(std::io::ErrorKind)
}

#[derive(Debug, PartialEq)]
pub enum KoanErrorType {
    LexErr(LexError),
    ParseErr(ParseError),
    InterpErr(InterpreterError),
    CliErr(CliError)
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
