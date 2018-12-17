use crate::grammar::Grammar;
use crate::grammar::Symbol;
use crate::grammar::SymbolString;
use crate::lr1::table::Goto;
use crate::lr1::table::{Action, Table};
use crate::terminal::Terminal;
use std::collections::VecDeque;
use std::fmt;
use std::marker::PhantomData;

#[derive(Debug)]
enum SymbolStackItem<N, T> {
    Symbol(Symbol<N, T>),
    Initial,
}

#[derive(Debug)]
struct StackItem<N, T> {
    pub state: usize,
    pub symbol: SymbolStackItem<N, T>,
}

// term for terminal, token for token
#[derive(Debug)]
pub struct Parser<'t, 'g, N, Term, Token, InputIter>
where
    InputIter: Iterator<Item = Token>,
    Term: Terminal<Token>,
{
    grammar: &'g Grammar<N, Term>,
    table: &'t Table<N, Term>,
    stack: Vec<StackItem<N, Token>>,
    input: InputIter,
    top: Top<Token>,
    // limit input type
    _phantom: PhantomData<Token>,
}

#[derive(Debug)]
enum Top<T> {
    Begin,
    Token(T),
    End,
}

#[derive(Debug)]
pub enum Event<N, T> {
    Reduce(Reduce<N, T>),
    Accept,
    Error(Error),
}

#[derive(Debug)]
pub struct Reduce<N, T> {
    pub from: SymbolString<N, T>,
    pub to: N,
}

impl<T: fmt::Debug> fmt::Display for Reduce<&'static str, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} <- ", self.to)?;
        if self.from.is_empty() {
            write!(f, "ε;")?;
        } else {
            for index in 0..self.from.len() {
                if index != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", self.from[index])?;
                if index == self.from.len() - 1 {
                    write!(f, ";")?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum Error {
    NoAction,
    NoGoto,
}

impl<'t, 'g, N, Term, Token, InputIter> Parser<'t, 'g, N, Term, Token, InputIter>
where
    N: Ord + Copy,
    InputIter: Iterator<Item = Token>,
    Term: Terminal<Token>,
{
    pub fn new(grammar: &'g Grammar<N, Term>, table: &'t Table<N, Term>, input: InputIter) -> Self {
        Self {
            grammar,
            table,
            stack: vec![StackItem {
                state: 0,
                symbol: SymbolStackItem::Initial,
            }],
            input,
            top: Top::Begin,
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn next_event(&mut self) -> Event<N, Token> {
        loop {
            if let Top::Begin = self.top {
                self.fetch_top();
            }

            let &StackItem { state, .. } = self.stack.last().unwrap();

            let action = match self.look_ahead() {
                Some(token) => self.table.action(state, token),
                None => self.table.end_action(state),
            };

            match action.unwrap() {
                Action::Accept => return Event::Accept,
                Action::Error => return Event::Error(Error::NoAction),
                Action::Shift(state) => {
                    let token = self.next_input().unwrap();
                    self.stack.push(StackItem {
                        state: *state,
                        symbol: SymbolStackItem::Symbol(Symbol::Terminal(token)),
                    });
                },
                Action::Reduce(rule_index) => {
                    let rule = &self.grammar.rules[*rule_index];
                    let length = rule.right.len();
                    let reduce = Reduce {
                        to: rule.left,
                        from: {
                            let mut queue = VecDeque::new();
                            for _ in 0..length {
                                match self.stack.pop().unwrap().symbol {
                                    SymbolStackItem::Symbol(symbol) => queue.push_front(symbol),
                                    SymbolStackItem::Initial => panic!("pop initial stack item"),
                                }
                            }
                            queue.into_iter().collect()
                        },
                    };
                    let current_top_state = self.stack.last().unwrap().state;
                    match self.table.goto(current_top_state, &rule.left).unwrap() {
                        Goto::Goto(goto) => self.stack.push(StackItem {
                            state: *goto,
                            symbol: SymbolStackItem::Symbol(Symbol::Nonterminal(rule.left)),
                        }),
                        Goto::Error => return Event::Error(Error::NoGoto),
                    };
                    return Event::Reduce(reduce)
                },
            }
        }
    }

    fn next_input(&mut self) -> Option<Token> {
        match &mut self.top {
            Top::Begin => match self.input.next() {
                None => {
                    self.top = Top::End;
                    None
                },
                Some(token) => {
                    match self.input.next() {
                        None => self.top = Top::End,
                        Some(new_top) => self.top = Top::Token(new_top),
                    };
                    Some(token)
                },
            },
            Top::End => None,
            mut top => {
                let mut new_top = match self.input.next() {
                    None => Top::End,
                    Some(t) => Top::Token(t),
                };
                std::mem::swap(&mut new_top, &mut top);
                match new_top {
                    Top::Token(t) => Some(t),
                    _ => panic!(),
                }
            },
        }
    }

    fn look_ahead(&self) -> Option<&Token> {
        match &self.top {
            Top::Token(token) => Some(&token),
            Top::End => None,
            _ => panic!("look ahead before fetch top"),
        }
    }

    fn fetch_top(&mut self) {
        match self.input.next() {
            None => self.top = Top::End,
            Some(token) => self.top = Top::Token(token),
        }
    }
}
