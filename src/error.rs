use std::backtrace::Backtrace;

#[derive(Debug, PartialEq)]
pub enum LexError {
    PartialMultiCharToken(char, char),
    InvalidToken(String),
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    ExpectedLiteral(String),
    ExpectedInfixOp,
}

#[derive(Debug)]
pub struct KoanError(pub KoanErrorType, pub std::backtrace::Backtrace);

impl PartialEq for KoanError {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

#[derive(Debug, PartialEq)]
pub enum KoanErrorType {
    LexErr(LexError),
    ParseErr(ParseError),
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
