use crate::{
    error::{ParseError, Result},
    lexer::{Token, TokenType},
    parser::TokenStream,
};

impl<'a, T: Iterator<Item = Token<'a>>> TokenStream<'a, T> {
    /// Consume the next token, error-ing if the type doesn't match what is expected
    pub fn expect(&mut self, ty: TokenType) -> Result<Token<'a>> {
        Err(match self.it.next() {
            Some(t) if t.variant == ty => return Ok(t),
            Some(t) => ParseError::ExpectedFound(ty, t.variant).into(),
            None => ParseError::ExpectedFoundEof(ty).into(),
        })
    }

    pub fn check(&mut self, ty: TokenType) -> bool {
        match self.it.peek() {
            Some(t) => t.variant == ty,
            None => false,
        }
    }

    pub fn multi_expect<const N: usize>(
        &mut self,
        tys: &[TokenType; N],
    ) -> Result<[Token<'a>; N]> {
        let mut res = Vec::with_capacity(N);

        for expected in tys {
            res.push(self.expect(*expected)?);
        }

        Ok(res.try_into().unwrap())
    }

    pub fn list<F, R>(
        &mut self,
        delim: (Option<TokenType>, TokenType),
        sep: TokenType,
        func: F,
    ) -> Result<Vec<R>>
    where
        F: FnMut(&mut TokenStream<'a, T>) -> Result<R>,
    {
        let mut func = func;
        if let Some(d) = delim.0 {
            let _ = self.expect(d)?;
        }

        let mut xs = vec![];

        loop {
            match self.it.peek() {
                Some(tok) if tok.variant == delim.1 => {
                    self.it.next();
                    break;
                }
                Some(_) => {
                    let x = func(self)?;
                    xs.push(x);

                    if self.check(delim.1) {
                        self.it.next();
                        break;
                    }

                    let _ = self.expect(sep)?;
                }
                None => {
                    return Err(ParseError::ExpectedFoundEof(delim.1).into())
                }
            }
        }

        Ok(xs)
    }
}
