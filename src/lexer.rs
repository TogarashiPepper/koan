use std::ops::RangeInclusive;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    PiTimes,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token<'a> {
    variant: TokenType,
    location: RangeInclusive<usize>,
    lexeme: &'a str,
}


fn lex<'a>(input: &'a str) -> Vec<Token<'a>> {
    todo!()
}
