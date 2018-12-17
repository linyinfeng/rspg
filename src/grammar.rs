use std::collections::BTreeMap;
use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
pub enum Symbol<N, T> {
    Nonterminal(N),
    Terminal(T),
}

impl<T: fmt::Debug> fmt::Display for Symbol<&'static str, T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        match self {
            Symbol::Nonterminal(n) => write!(f, "{}", n),
            Symbol::Terminal(t) => write!(f, "{:?}", t),
        }
    }
}

pub type SymbolString<N, T> = Vec<Symbol<N, T>>;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Rule<N, T> {
    pub left: N,
    pub right: SymbolString<N, T>,
}

impl<T: fmt::Debug> fmt::Display for Rule<&'static str, T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} -> ", self.left)?;
        if self.right.is_empty() {
            write!(f, "ε;")?;
        } else {
            for index in 0..self.right.len() {
                if index != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", self.right[index])?;
                if index == self.right.len() - 1 {
                    write!(f, ";")?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Grammar<N, T> {
    pub index: BTreeMap<N, Vec<usize>>,
    pub rules: Vec<Rule<N, T>>,
    pub start: N,
}

impl<T: fmt::Debug> fmt::Display for Grammar<&'static str, T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        writeln!(f, "grammar! {{\n    start {};", self.start)?;
        for rule in &self.rules {
            writeln!(f, "    rule {}", rule)?;
        }
        write!(f, "}}")
    }
}

#[derive(Debug, Default)]
pub struct GrammarBuilder<N, T>
where
    N: Ord,
{
    index: BTreeMap<N, Vec<usize>>,
    rules: Vec<Rule<N, T>>,
    start: Option<N>,
}

impl<N, T> GrammarBuilder<N, T>
where
    N: Ord + Copy,
{
    pub fn new() -> Self {
        Self {
            index: BTreeMap::new(),
            rules: Vec::new(),
            start: None,
        }
    }

    pub fn from(grammar: Grammar<N, T>) -> Self {
        GrammarBuilder {
            index: grammar.index,
            rules: grammar.rules,
            start: Some(grammar.start),
        }
    }

    pub fn start(mut self, start: N) -> Self {
        self.start = Some(start);
        self
    }

    pub fn push_rule(mut self, left: N) -> Self {
        if self.start.is_none() {
            self.start = Some(left);
        }
        let index = self.rules.len();
        match self.index.get_mut(&left) {
            None => {
                self.index.insert(left, vec![index]);
            },
            Some(entry) => {
                entry.push(index);
            },
        }
        self.rules.push(Rule {
            left,
            right: SymbolString::new(),
        });
        self
    }

    pub fn push_right(mut self, s: Symbol<N, T>) -> Self {
        self.rules.last_mut().unwrap().right.push(s);
        self
    }

    pub fn rule_number(&self) -> usize {
        self.rules.len()
    }

    pub fn build(self) -> Option<Grammar<N, T>> {
        match self.start {
            Some(start) => {
                if !self.index.contains_key(&start) {
                    panic!()
                }
                Some(Grammar {
                    index: self.index,
                    rules: self.rules,
                    start,
                })
            },
            None => None,
        }
    }
}
