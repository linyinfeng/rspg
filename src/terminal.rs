pub trait Terminal<Token> {
    fn contains(&self, t: &Token) -> bool;
}

impl<T, Token> Terminal<&Token> for T
where
    T: Terminal<Token>,
{
    fn contains(&self, t: &&Token) -> bool {
        self.contains(*t)
    }
}
