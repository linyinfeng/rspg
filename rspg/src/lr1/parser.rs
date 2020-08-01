use crate::display::DisplayWith;
use crate::grammar::Grammar;
use crate::grammar::NonterminalIndex;
use crate::grammar::RuleIndex;
use crate::lr1::table::EndAction;
use crate::lr1::table::Goto;
use crate::lr1::table::State;
use crate::lr1::table::{Action, Table};
use crate::token;
use std::collections::VecDeque;
use std::fmt;
use std::marker::PhantomData;
use std::ops::Deref;
use std::ops::DerefMut;

#[derive(Debug, Clone)]
pub enum SymbolStackItem<Parsed, Token> {
    Nonterminal(NonterminalIndex, Parsed),
    Token(Token),
}

impl<N, T, Parsed, Token> DisplayWith<Grammar<N, T>> for SymbolStackItem<Parsed, Token>
where
    N: fmt::Display + Ord,
    T: Ord,
    Token: fmt::Debug,
    Parsed: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> fmt::Result {
        match self {
            SymbolStackItem::Nonterminal(n, p) => {
                write!(f, "{}{{{:?}}}", n.display_with(grammar), p)
            },
            SymbolStackItem::Token(t) => write!(f, "{:?}", t),
        }
    }
}

impl<Parsed, Token> SymbolStackItem<Parsed, Token> {
    pub fn nonterminal(&self) -> Option<NonterminalIndex> {
        match self {
            SymbolStackItem::Nonterminal(i, _) => Some(*i),
            _ => None,
        }
    }

    pub fn parsed(self) -> Option<Parsed> {
        match self {
            SymbolStackItem::Nonterminal(_, p) => Some(p),
            _ => None,
        }
    }

    pub fn token(self) -> Option<Token> {
        match self {
            SymbolStackItem::Token(t) => Some(t),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
struct StackItem<Parsed, Token> {
    pub state: State,
    pub symbol: Option<SymbolStackItem<Parsed, Token>>,
}

// term for terminal, token for token
#[derive(Debug, Clone)]
pub struct Parser<'t, 'g, N, Term, Token, Parsed, F, E>
where
    N: Ord,
    Term: Ord,
    Token: token::Token<Term>,
    F: Fn(Reduce<Parsed, Token>) -> Result<Parsed, E>,
{
    pub grammar: &'g Grammar<N, Term>,
    pub table: &'t Table,
    pub reducer: F,
    pub phantom: PhantomData<(Token, Parsed, E)>,
}

#[derive(Debug, Clone)]
pub struct SymbolStackItemString<Parsed, Token>(pub VecDeque<SymbolStackItem<Parsed, Token>>);

impl<Parsed, Token> From<VecDeque<SymbolStackItem<Parsed, Token>>>
    for SymbolStackItemString<Parsed, Token>
{
    fn from(v: VecDeque<SymbolStackItem<Parsed, Token>>) -> Self {
        SymbolStackItemString(v)
    }
}

impl<Parsed, Token> Deref for SymbolStackItemString<Parsed, Token> {
    type Target = VecDeque<SymbolStackItem<Parsed, Token>>;

    fn deref(&self) -> &<Self as Deref>::Target {
        &self.0
    }
}

impl<Parsed, Token> DerefMut for SymbolStackItemString<Parsed, Token> {
    fn deref_mut(&mut self) -> &mut <Self as Deref>::Target {
        &mut self.0
    }
}

impl<N, T, Parsed, Token> DisplayWith<Grammar<N, T>> for SymbolStackItemString<Parsed, Token>
where
    N: fmt::Display + Ord,
    T: Ord,
    Token: fmt::Debug,
    Parsed: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> fmt::Result {
        if self.is_empty() {
            write!(f, "ε")?;
        } else {
            for (i, item) in self.iter().enumerate() {
                if i != 0 {
                    write!(f, " ")?;
                }
                write!(f, "{}", item.display_with(grammar))?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Reduce<Parsed, Token> {
    pub rule: RuleIndex,
    pub from: SymbolStackItemString<Parsed, Token>,
    pub to: NonterminalIndex,
}

impl<N, T, Parsed, Token> DisplayWith<Grammar<N, T>> for Reduce<Parsed, Token>
where
    N: fmt::Display + Ord,
    T: Ord,
    Token: fmt::Debug,
    Parsed: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> fmt::Result {
        write!(f, "{}", self.to.display_with(grammar))?;
        write!(f, " <- ")?;
        write!(f, "{}", self.from.display_with(grammar))?;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Error<E> {
    NoAction,
    NoEndAction,
    NoGoto,
    IncompleteAccept,
    NotAcceptOnStart,
    Reduce(E),
}

impl<'t, 'g, N, Term, Token, Parsed, F, E> Parser<'t, 'g, N, Term, Token, Parsed, F, E>
where
    N: Ord + Copy,
    Term: Ord,
    Token: token::Token<Term>,
    F: Fn(Reduce<Parsed, Token>) -> Result<Parsed, E>,
{
    pub fn parse<InputIter>(&self, input: InputIter) -> Result<Parsed, Error<E>>
    where
        InputIter: Iterator<Item = Token>,
    {
        let mut stack = vec![StackItem {
            state: Table::start_state(),
            symbol: None,
        }];
        let mut input = input.peekable();
        loop {
            let &StackItem { state, .. } = stack.last().unwrap();

            match input.peek() {
                Some(token) => {
                    let terminal = self.grammar.terminal_index(&token.terminal());
                    let action = Some(self.table.action(state, terminal));
                    match action.expect("token can not match any terminals") {
                        Action::Error => return Err(Error::NoAction),
                        Action::Shift(state) => {
                            let token = input.next().unwrap();
                            stack.push(StackItem {
                                state,
                                symbol: Some(SymbolStackItem::Token(token)),
                            });
                        },
                        Action::Reduce(rule) => match self.reduce(&mut stack, rule) {
                            Ok(()) => (),
                            Err(e) => return Err(e),
                        },
                    }
                },
                None => match self.table.end_action(state) {
                    EndAction::Error => return Err(Error::NoEndAction),
                    EndAction::Accept => {
                        if stack.len() == 2 {
                            match stack.pop().unwrap().symbol.unwrap() {
                                SymbolStackItem::Nonterminal(nonterminal, parsed) => {
                                    if nonterminal == self.grammar.start_index() {
                                        return Ok(parsed)
                                    } else {
                                        return Err(Error::NotAcceptOnStart)
                                    }
                                },
                                SymbolStackItem::Token(_) => return Err(Error::IncompleteAccept),
                            }
                        } else {
                            return Err(Error::IncompleteAccept)
                        }
                    },
                    EndAction::Reduce(rule) => match self.reduce(&mut stack, rule) {
                        Ok(()) => (),
                        Err(e) => return Err(e),
                    },
                },
            }
        }
    }

    fn reduce(
        &self,
        stack: &mut Vec<StackItem<Parsed, Token>>,
        rule_index: RuleIndex,
    ) -> Result<(), Error<E>> {
        let rule = self.grammar.rule(rule_index);
        let length = rule.right.len();
        let reduce = Reduce {
            rule: rule_index,
            from: {
                let mut queue = VecDeque::new();
                for _ in 0..length {
                    match stack.pop().unwrap().symbol {
                        Some(item) => queue.push_front(item),
                        None => panic!("pop initial stack item"),
                    }
                }
                SymbolStackItemString(queue)
            },
            to: rule.left,
        };
        match (self.reducer)(reduce) {
            Ok(reduced) => {
                let current_top_state = stack.last().unwrap().state;
                match self.table.goto(current_top_state, rule.left) {
                    Goto::Goto(goto) => {
                        stack.push(StackItem {
                            state: goto,
                            symbol: Some(SymbolStackItem::Nonterminal(rule.left, reduced)),
                        });
                        Ok(())
                    },
                    Goto::Error => Err(Error::NoGoto),
                }
            },
            Err(e) => Err(Error::Reduce(e)),
        }
    }
}
