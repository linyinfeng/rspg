use std::ops::Deref;
use std::ops::DerefMut;
use std::fmt;

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


/// a token type just map itself to terminal
#[derive(Clone, Copy, Ord, PartialOrd, Eq, PartialEq)]
pub struct TerminalToken<T>(T);

impl<T> Deref for TerminalToken<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for TerminalToken<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> Token<T> for TerminalToken<T>
where T: Copy,
{
    fn terminal(&self) -> T {
        self.0
    }
}

impl<T> fmt::Display for TerminalToken<T>
where T: fmt::Display, {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T> fmt::Debug for TerminalToken<T>
where T: fmt::Debug, {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl<T> TerminalToken<T> {
    pub fn new(t: T) -> Self {
        TerminalToken(t)
    }
}