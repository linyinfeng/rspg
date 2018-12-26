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
use std::iter::Peekable;
use std::ops::Deref;

#[derive(Debug, Clone)]
pub enum NonterminalToken<Token> {
    Nonterminal(NonterminalIndex),
    Token(Token),
}

impl<N, T, Token> DisplayWith<Grammar<N, T>> for NonterminalToken<Token>
where
    N: fmt::Display,
    Token: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> fmt::Result {
        match self {
            NonterminalToken::Nonterminal(n) => write!(f, "{}", n.display_with(grammar)),
            NonterminalToken::Token(t) => write!(f, "{:?}", t),
        }
    }
}

#[derive(Debug, Clone)]
enum SymbolStackItem<Token> {
    NonterminalToken(NonterminalToken<Token>),
    Initial,
}

#[derive(Debug, Clone)]
struct StackItem<Token> {
    pub state: State,
    pub symbol: SymbolStackItem<Token>,
}

// term for terminal, token for token
#[derive(Debug, Clone)]
pub struct Parser<'t, 'g, N, Term, Token, InputIter>
where
    InputIter: Iterator<Item = Token>,
    Token: token::Token<Term>,
{
    grammar: &'g Grammar<N, Term>,
    table: &'t Table,
    stack: Vec<StackItem<Token>>,
    input: Peekable<InputIter>,
}

#[derive(Debug, Clone)]
pub enum Event<Token> {
    Reduce(Reduce<Token>),
    Accept,
    Error(Error),
}

#[derive(Debug, Clone)]
pub struct NonterminalTokenString<Token>(pub Vec<NonterminalToken<Token>>);

impl<Token> From<Vec<NonterminalToken<Token>>> for NonterminalTokenString<Token> {
    fn from(v: Vec<NonterminalToken<Token>>) -> Self {
        NonterminalTokenString(v)
    }
}

impl<Token> Deref for NonterminalTokenString<Token> {
    type Target = Vec<NonterminalToken<Token>>;

    fn deref(&self) -> &<Self as Deref>::Target {
        &self.0
    }
}

impl<N, T, Token> DisplayWith<Grammar<N, T>> for NonterminalTokenString<Token>
where
    N: fmt::Display,
    Token: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> fmt::Result {
        if self.is_empty() {
            write!(f, "ε")?;
        } else {
            for (i, symbol) in self.iter().enumerate() {
                if i != 0 {
                    write!(f, " ")?;
                }
                write!(f, "{}", symbol.display_with(grammar))?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Reduce<Token> {
    pub rule: RuleIndex,
    pub from: NonterminalTokenString<Token>,
    pub to: NonterminalIndex,
}

impl<N, T, Token> DisplayWith<Grammar<N, T>> for Reduce<Token>
where
    N: fmt::Display,
    Token: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> fmt::Result {
        write!(f, "{}", self.to.display_with(grammar))?;
        write!(f, " <- ")?;
        write!(f, "{}", self.from.display_with(grammar))?;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Error {
    ErrorAction,
    ErrorEndAction,
    ErrorGoto,
}

impl<'t, 'g, N, Term, Token, InputIter> Parser<'t, 'g, N, Term, Token, InputIter>
where
    N: Ord + Copy,
    Term: Ord,
    InputIter: Iterator<Item = Token>,
    Token: token::Token<Term>,
{
    pub fn new(grammar: &'g Grammar<N, Term>, table: &'t Table, input: InputIter) -> Self {
        Self {
            grammar,
            table,
            stack: vec![StackItem {
                state: Table::start_state(),
                symbol: SymbolStackItem::Initial,
            }],
            input: input.peekable(),
        }
    }

    pub fn next_event(&mut self) -> Event<Token> {
        loop {
            let &StackItem { state, .. } = self.stack.last().unwrap();

            match self.input.peek() {
                Some(token) => {
                    let terminal = self.grammar.terminal_index(&token.terminal());
                    let action = Some(self.table.action(state, terminal));
                    match action.expect("token can not match any terminals") {
                        Action::Error => return Event::Error(Error::ErrorAction),
                        Action::Shift(state) => {
                            let token = self.input.next().unwrap();
                            self.stack.push(StackItem {
                                state,
                                symbol: SymbolStackItem::NonterminalToken(NonterminalToken::Token(
                                    token,
                                )),
                            });
                        },
                        Action::Reduce(rule) => match self.reduce(rule) {
                            Ok(reduce) => return Event::Reduce(reduce),
                            Err(e) => return Event::Error(e),
                        },
                    }
                },
                None => match self.table.end_action(state) {
                    EndAction::Error => return Event::Error(Error::ErrorEndAction),
                    EndAction::Accept => return Event::Accept,
                    EndAction::Reduce(rule) => match self.reduce(rule) {
                        Ok(reduce) => return Event::Reduce(reduce),
                        Err(e) => return Event::Error(e),
                    },
                },
            }
        }
    }

    fn reduce(&mut self, rule_index: RuleIndex) -> Result<Reduce<Token>, Error> {
        let rule = self.grammar.rule(rule_index);
        let length = rule.right.len();
        let reduce = Reduce {
            rule: rule_index,
            from: {
                let mut queue = VecDeque::new();
                for _ in 0..length {
                    match self.stack.pop().unwrap().symbol {
                        SymbolStackItem::NonterminalToken(symbol) => queue.push_front(symbol),
                        SymbolStackItem::Initial => panic!("pop initial stack item"),
                    }
                }
                NonterminalTokenString(queue.into_iter().collect())
            },
            to: rule.left,
        };
        let current_top_state = self.stack.last().unwrap().state;
        match self.table.goto(current_top_state, rule.left) {
            Goto::Goto(goto) => {
                self.stack.push(StackItem {
                    state: goto,
                    symbol: SymbolStackItem::NonterminalToken(NonterminalToken::Nonterminal(
                        rule.left,
                    )),
                });
                Ok(reduce)
            },
            Goto::Error => Err(Error::ErrorGoto),
        }
    }
}
