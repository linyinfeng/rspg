use crate::display::DisplayWith;
use crate::grammar::Grammar;
use crate::grammar::RuleIndex;
use crate::grammar::Symbol;
use serde::Deserialize;
use serde::Serialize;
use std::fmt;

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Copy)]
pub struct Item {
    pub rule: RuleIndex,
    pub location: usize,
}

impl Item {
    pub fn next_symbol<N, T>(&self, grammar: &Grammar<N, T>) -> Option<Symbol>
    where
        N: Ord,
        T: Ord,
    {
        grammar.rule(self.rule).right.get(self.location).cloned()
    }

    pub fn finished<N, T>(&self, grammar: &Grammar<N, T>) -> bool
    where
        N: Ord,
        T: Ord,
    {
        grammar.rule(self.rule).right.len() == self.location
    }
}

impl<N, T> DisplayWith<Grammar<N, T>> for Item
where
    N: fmt::Display + Ord,
    T: fmt::Debug + Ord,
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
            if i < self.location {
                write!(f, "{}", rule.right[i].display_with(grammar))?;
            } else if i == self.location {
                write!(f, "·")?;
            } else {
                // if i > self.location
                write!(f, "{}", rule.right[i - 1].display_with(grammar))?;
            }
        }
        write!(f, "]")?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::Item;
    use crate::display::DisplayWith;
    use crate::grammar;
    use crate::grammar::Grammar;
    use crate::grammar::Symbol;

    fn example_grammar() -> Grammar<&'static str, char> {
        grammar! {
            start E;
            rule E -> A, 'b', 'c', E;
            rule E -> epsilon;
        }
    }

    #[test]
    fn finished() {
        let grammar = example_grammar();
        let mut rule_indices = grammar.rule_indices();
        let first_rule = rule_indices.next().unwrap();
        let second_rule = rule_indices.next().unwrap();
        assert_eq!(
            Item {
                rule: first_rule,
                location: 0,
            }
            .finished(&grammar),
            false
        );
        assert_eq!(
            Item {
                rule: first_rule,
                location: 1,
            }
            .finished(&grammar),
            false
        );
        assert_eq!(
            Item {
                rule: first_rule,
                location: 2,
            }
            .finished(&grammar),
            false
        );
        assert_eq!(
            Item {
                rule: first_rule,
                location: 3,
            }
            .finished(&grammar),
            false
        );
        assert_eq!(
            Item {
                rule: first_rule,
                location: 4,
            }
            .finished(&grammar),
            true
        );
        assert_eq!(
            Item {
                rule: second_rule,
                location: 0,
            }
            .finished(&grammar),
            true
        );
    }

    #[test]
    fn next_symbol() {
        let grammar = example_grammar();
        let mut rule_indices = grammar.rule_indices();
        let first_rule = rule_indices.next().unwrap();
        let second_rule = rule_indices.next().unwrap();
        assert_eq!(
            Item {
                rule: first_rule,
                location: 0,
            }
            .next_symbol(&grammar),
            Some(Symbol::Nonterminal(grammar.nonterminal_index(&"A")))
        );
        assert_eq!(
            Item {
                rule: first_rule,
                location: 1,
            }
            .next_symbol(&grammar),
            Some(Symbol::Terminal(grammar.terminal_index(&'b')))
        );
        assert_eq!(
            Item {
                rule: first_rule,
                location: 2,
            }
            .next_symbol(&grammar),
            Some(Symbol::Terminal(grammar.terminal_index(&'c')))
        );
        assert_eq!(
            Item {
                rule: first_rule,
                location: 3,
            }
            .next_symbol(&grammar),
            Some(Symbol::Nonterminal(grammar.nonterminal_index(&"E")))
        );
        assert_eq!(
            Item {
                rule: first_rule,
                location: 4,
            }
            .next_symbol(&grammar),
            None
        );
        assert_eq!(
            Item {
                rule: second_rule,
                location: 0,
            }
            .next_symbol(&grammar),
            None
        );
    }

    #[test]
    fn display() {
        let grammar = example_grammar();
        let mut rule_indices = grammar.rule_indices();
        let first_rule = rule_indices.next().unwrap();
        let second_rule = rule_indices.next().unwrap();
        assert_eq!(
            Item {
                rule: first_rule,
                location: 0,
            }
            .display_with(&grammar)
            .to_string(),
            "[E -> · A 'b' 'c' E]"
        );
        assert_eq!(
            Item {
                rule: first_rule,
                location: 1,
            }
            .display_with(&grammar)
            .to_string(),
            "[E -> A · 'b' 'c' E]"
        );
        assert_eq!(
            Item {
                rule: first_rule,
                location: 2,
            }
            .display_with(&grammar)
            .to_string(),
            "[E -> A 'b' · 'c' E]"
        );
        assert_eq!(
            Item {
                rule: first_rule,
                location: 3,
            }
            .display_with(&grammar)
            .to_string(),
            "[E -> A 'b' 'c' · E]"
        );
        assert_eq!(
            Item {
                rule: first_rule,
                location: 4,
            }
            .display_with(&grammar)
            .to_string(),
            "[E -> A 'b' 'c' E ·]"
        );
        assert_eq!(
            Item {
                rule: second_rule,
                location: 0,
            }
            .display_with(&grammar)
            .to_string(),
            "[E -> ·]"
        );
    }
}
