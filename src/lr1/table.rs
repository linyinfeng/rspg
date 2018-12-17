use crate::terminal::Terminal;
use std::collections::BTreeMap;

type State = usize;

#[derive(Debug)]
pub struct Table<N, T> {
    pub action: Vec<(T, Vec<Action>)>,
    pub end_action: Vec<Action>,
    pub goto: BTreeMap<N, Vec<Goto>>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Action {
    Reduce(usize),
    Shift(usize),
    Accept,
    Error,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Goto {
    Goto(usize),
    Error,
}

impl<N, T> Table<N, T>
where
    N: Ord,
{
    pub fn terminals(&self) -> impl Iterator<Item = &T> {
        self.action.iter().map(|t| &t.0)
    }

    pub fn goto(&self, state: State, nonterminal: &N) -> Option<&Goto> {
        self.goto.get(nonterminal).and_then(|v| v.get(state))
    }

    pub fn action<Token>(&self, state: State, token: &Token) -> Option<&Action>
    where
        T: Terminal<Token>,
    {
        for t in &self.action {
            if t.0.contains(token) {
                return t.1.get(state)
            }
        }
        None
    }

    pub fn end_action(&self, state: State) -> Option<&Action> {
        self.end_action.get(state)
    }
}
