use crate::parser::Expr;

pub enum Value {
    Num(f64),
    UTF8(String),
}

impl Expr {
    pub fn eval(self) -> Value {
        todo!()
    }
}
