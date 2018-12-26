/// map Token to terminal Terminal
pub trait Token<Terminal> {
    fn terminal(&self) -> Terminal;
}

impl<Terminal, T> Token<Terminal> for &T
where
    T: Token<Terminal>,
{
    fn terminal(&self) -> Terminal {
        (*self).terminal()
    }
}
