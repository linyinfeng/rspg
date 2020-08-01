use crate::grammar::Grammar;
use crate::grammar::TerminalIndex;
use serde::Deserialize;
use serde::Serialize;
use std::fmt;
use std::ops::Deref;
use std::ops::DerefMut;

/// `Token<Terminal>` is a trait. Types which implement `Token<Terminal>`
/// implement function `Token<Terminal>::terminal` which return the associated
/// terminal of the token.
pub trait Token<Terminal>
where
    Terminal: Ord,
{
    fn terminal(&self) -> Terminal;

    fn terminal_index<N>(&self, grammar: &Grammar<N, Terminal>) -> TerminalIndex
    where
        N: Ord,
    {
        grammar.terminal_index(&self.terminal())
    }
}

/// Implements all reference to a `Token<Terminal>` as `Token<Terminal>`.
impl<Terminal, T> Token<Terminal> for &T
where
    Terminal: Ord,
    T: Token<Terminal>,
{
    /// Deref and call `Token<Terminal>::terminal`.
    fn terminal(&self) -> Terminal {
        (*self).terminal()
    }
}

/// A token type whose terminal type is itself.
#[derive(Serialize, Deserialize, Clone, Copy, Ord, PartialOrd, Eq, PartialEq)]
pub struct TerminalToken<T>(pub T);

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
where
    T: Ord + Copy,
{
    fn terminal(&self) -> T {
        self.0
    }
}

impl<T> fmt::Display for TerminalToken<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T> fmt::Debug for TerminalToken<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl<T> TerminalToken<T> {
    /// Create a new `TerminalToken<T>` from `T`.
    pub fn new(t: T) -> Self {
        TerminalToken(t)
    }
}
