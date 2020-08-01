use crate::display::DisplayWith;
use crate::grammar::Grammar;
use crate::grammar::NonterminalIndex;
use crate::grammar::RuleIndex;
use crate::grammar::TerminalIndex;
use crate::utility::vec_with_size;
use serde::Deserialize;
use serde::Serialize;
use std::fmt;
use std::iter;
use std::ops;

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Copy)]
pub struct State(pub(self) usize);

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Table {
    action: Vec<Vec<Action>>,   // indexed with TerminalIndex and State
    end_action: Vec<EndAction>, // indexed with State
    goto: Vec<Vec<Goto>>,       // indexed with NonterminalIndex and State
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, Eq, PartialEq)]
pub enum Action {
    Reduce(RuleIndex),
    // reduce using rule
    Shift(State),
    // shift to state
    Error,
}

impl fmt::Display for Action {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Action::Reduce(index) => {
                write!(f, "r{}", index)?;
            },
            Action::Shift(state) => write!(f, "s{}", state)?,
            Action::Error => write!(f, "")?,
        }
        Ok(())
    }
}

impl<N, T> DisplayWith<Grammar<N, T>> for Action
where
    N: fmt::Display + Ord,
    T: fmt::Debug + Ord,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> fmt::Result {
        match self {
            Action::Reduce(index) => {
                write!(f, "r{}", index.display_with(grammar))?;
            },
            Action::Shift(state) => write!(f, "s{}", state)?,
            Action::Error => write!(f, "")?,
        }
        Ok(())
    }
}

impl Default for Action {
    fn default() -> Self {
        Action::Error
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, Eq, PartialEq)]
pub enum EndAction {
    Reduce(RuleIndex),
    // reduce using rule
    Accept,
    Error,
}

impl fmt::Display for EndAction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EndAction::Reduce(index) => {
                write!(f, "r{}", index)?;
            },
            EndAction::Accept => write!(f, "acc")?,
            EndAction::Error => write!(f, "")?,
        }
        Ok(())
    }
}

impl<N, T> DisplayWith<Grammar<N, T>> for EndAction
where
    N: fmt::Display + Ord,
    T: fmt::Debug + Ord,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> fmt::Result {
        match self {
            EndAction::Reduce(index) => {
                write!(f, "r{}", index.display_with(grammar))?;
            },
            EndAction::Accept => write!(f, "acc")?,
            EndAction::Error => write!(f, "")?,
        }
        Ok(())
    }
}

impl Default for EndAction {
    fn default() -> Self {
        EndAction::Error
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, Eq, PartialEq)]
pub enum Goto {
    Goto(State),
    Error,
}

impl fmt::Display for Goto {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Goto::Goto(state) => write!(f, "{}", state),
            Goto::Error => write!(f, ""),
        }
    }
}

impl Default for Goto {
    fn default() -> Self {
        Goto::Error
    }
}

impl Table {
    /// Create an empty(no state) table.
    pub fn new<N, T>(grammar: &Grammar<N, T>) -> Table
    where
        N: Ord,
        T: Ord,
    {
        Table {
            action: vec_with_size(grammar.terminals_len(), Vec::new()),
            end_action: Vec::new(),
            goto: vec_with_size(grammar.nonterminals_len(), Vec::new()),
        }
    }

    pub fn push_state(&mut self) -> State {
        let state = State(self.state_len());
        self.end_action.push(Default::default());
        for states in &mut self.action {
            states.push(Default::default());
        }
        for states in &mut self.goto {
            states.push(Default::default());
        }
        state
    }

    pub fn state_len(&self) -> usize {
        self.end_action.len()
    }

    pub fn states(&self) -> iter::Map<ops::Range<usize>, fn(usize) -> State> {
        (0..self.state_len()).map(State)
    }

    pub fn start_state() -> State {
        State(0)
    }

    pub fn goto(&self, state: State, nonterminal: NonterminalIndex) -> Goto {
        self.goto[nonterminal.value()][state.0]
    }

    pub fn goto_mut(&mut self, state: State, nonterminal: NonterminalIndex) -> &mut Goto {
        &mut self.goto[nonterminal.value()][state.0]
    }

    pub fn action(&self, state: State, terminal: TerminalIndex) -> Action {
        self.action[terminal.value()][state.0]
    }

    pub fn action_mut(&mut self, state: State, terminal: TerminalIndex) -> &mut Action {
        &mut self.action[terminal.value()][state.0]
    }

    pub fn end_action(&self, state: State) -> EndAction {
        self.end_action[state.0]
    }

    pub fn end_action_mut(&mut self, state: State) -> &mut EndAction {
        &mut self.end_action[state.0]
    }

    pub fn pretty_table<N, T>(&self, grammar: &Grammar<N, T>, detailed: bool) -> prettytable::Table
    where
        N: fmt::Display + Ord,
        T: fmt::Debug + Ord,
    {
        use prettytable::{Cell, Row};
        let mut table = prettytable::Table::new();
        table.add_row({
            let mut cells = Vec::new();
            cells.push(Cell::new(""));
            cells.push(Cell::new("action").with_hspan(grammar.terminals_len() + 1));
            cells.push(Cell::new("goto").with_hspan(grammar.nonterminals_len()));
            Row::new(cells)
        });
        table.add_row({
            let mut cells = Vec::new();
            cells.push(Cell::new(""));
            for terminal in grammar.terminal_indices() {
                cells.push(Cell::new(&format!("{}", terminal.display_with(grammar))));
            }
            cells.push(Cell::new("$"));
            for nonterminal in grammar.nonterminal_indices() {
                cells.push(Cell::new(&format!("{}", nonterminal.display_with(grammar))));
            }
            Row::new(cells)
        });
        for state in self.states() {
            table.add_row({
                let mut cells = Vec::new();
                cells.push(Cell::new(&format!("{}", state)));
                for terminal in grammar.terminal_indices() {
                    if detailed {
                        cells.push(Cell::new(&format!(
                            "{}",
                            self.action(state, terminal).display_with(grammar)
                        )));
                    } else {
                        cells.push(Cell::new(&format!("{}", self.action(state, terminal))));
                    }
                }
                if detailed {
                    cells.push(Cell::new(&format!(
                        "{}",
                        self.end_action(state).display_with(grammar)
                    )));
                } else {
                    cells.push(Cell::new(&format!("{}", self.end_action(state))));
                }
                for nonterminal in grammar.nonterminal_indices() {
                    cells.push(Cell::new(&format!("{}", self.goto(state, nonterminal))));
                }
                Row::new(cells)
            });
        }
        table
    }
}
