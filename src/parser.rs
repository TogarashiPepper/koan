use crate::lexer::{TokenType, Token};

enum Operator {
    PiTimes,
    Plus,
    Minus,
    Times,
    Slash,
    Assign,
    Equality,
    Greater,
    GreaterEqual,
    Lesser,
    LesserEqual,
}

impl TryFrom<TokenType> for Operator {
    type Error = &'static str;

    fn try_from(value: TokenType) -> Result<Self, Self::Error> {
        Ok(match value {
            TokenType::PiTimes => Self::PiTimes,
            TokenType::Plus => Self::Plus,
            TokenType::Minus => Self::Minus,
            TokenType::Times => Self::Times,
            TokenType::Slash => Self::Slash,
            TokenType::Equal => Self::Assign,
            TokenType::DoubleEqual => Self::Equality,
            TokenType::Greater => Self::Greater,
            TokenType::GreaterEqual => Self::GreaterEqual,
            TokenType::Lesser => Self::Lesser,
            TokenType::LesserEqual => Self::LesserEqual,
            _ => return Err("The provided TokenType is not an operator")
        })
    }
}

enum Ast {

}

enum Expr {
    // Convert this to BinOp(BinOp) for impls on it?
    BinOp {
        lhs: Box<Expr>,
        op: Operator,
        rhs: Box<Expr>
    },
    Ident(String),
    // TODO: Add integer literals to avoid JS-esque strangeness
    NumLit(f64),
}

pub fn parse<'a>(tokens: Vec<Token<'a>>) -> Ast {
    todo!()
}
