use crate::display::DisplayWith;
use crate::grammar::Grammar;
use crate::grammar::RuleIndex;
use crate::grammar::Symbol;
use std::fmt;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Copy)]
pub struct Item {
    pub rule: RuleIndex,
    pub location: usize,
}

impl Item {
    pub fn next_symbol<N, T>(&self, grammar: &Grammar<N, T>) -> Option<Symbol> {
        grammar.rule(self.rule).right.get(self.location).cloned()
    }

    pub fn finished<N, T>(&self, grammar: &Grammar<N, T>) -> bool {
        grammar.rule(self.rule).right.len() == self.location
    }
}

impl<N, T> DisplayWith<Grammar<N, T>> for Item
where
    N: fmt::Display,
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> fmt::Result {
        write!(f, "[")?;
        let rule = grammar.rule(self.rule);
        write!(f, "{}", rule.left.display_with(grammar))?;
        write!(f, " -> ",)?;
        for i in 0..=rule.right.len() {
            if i != 0 {
                write!(f, " ")?;
            }
            if i == self.location {
                write!(f, "·")?;
            }
            if i < rule.right.len() {
                write!(f, "{}", rule.right[i].display_with(grammar))?;
            }
        }
        write!(f, "]")?;
        Ok(())
    }
}
