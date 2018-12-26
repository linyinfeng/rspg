use crate::display::DisplayWith;
use crate::grammar::NonterminalIndex;
use crate::grammar::TerminalIndex;
use crate::grammar::{Grammar, Rule, Symbol};
use crate::utility::vec_with_size;
use std::collections::BTreeSet;
use std::fmt;

#[derive(Debug, Clone)]
pub struct FirstSet {
    pub terminals: BTreeSet<TerminalIndex>,
    pub can_be_empty: bool,
}

impl fmt::Display for FirstSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{")?;
        let mut i = 0usize;
        let mut iter = self.terminals.iter();
        while i < self.terminals.len() {
            if i != 0usize {
                write!(f, ", ")?;
            }
            write!(f, "{}", iter.next().unwrap())?;
            i += 1;
        }
        if self.can_be_empty {
            if i != 0usize {
                write!(f, ", ")?;
            }
            write!(f, "ε")?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl<N, T> DisplayWith<Grammar<N, T>> for FirstSet
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> fmt::Result {
        write!(f, "{{")?;
        let mut i = 0usize;
        let mut iter = self.terminals.iter();
        while i < self.terminals.len() {
            if i != 0usize {
                write!(f, ", ")?;
            }
            write!(f, "{}", iter.next().unwrap().display_with(grammar))?;
            i += 1;
        }
        if self.can_be_empty {
            if i != 0usize {
                write!(f, ", ")?;
            }
            write!(f, "ε")?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct FirstSets(Vec<FirstSet>);

impl FirstSets {
    pub fn first_set_of_nonterminal(&self, nonterminal: NonterminalIndex) -> &FirstSet {
        &self.0[nonterminal.value()]
    }

    fn first_set_of_nonterminal_mut(&mut self, nonterminal: NonterminalIndex) -> &mut FirstSet {
        &mut self.0[nonterminal.value()]
    }

    pub fn first_set_of_symbol_string(&self, symbol_string: &[Symbol]) -> FirstSet {
        let mut first_set = FirstSet {
            terminals: BTreeSet::new(),
            can_be_empty: false,
        };
        let mut empty_count = 0;
        for symbol in symbol_string {
            match symbol {
                Symbol::Nonterminal(n) => {
                    let set = self.first_set_of_nonterminal(*n);
                    first_set.terminals.extend(&set.terminals);
                    if set.can_be_empty {
                        empty_count += 1;
                    } else {
                        break
                    }
                },
                Symbol::Terminal(t) => {
                    first_set.terminals.insert(*t);
                    break
                },
            }
        }
        if empty_count == symbol_string.len() {
            first_set.can_be_empty = true;
        }
        first_set
    }

    pub fn of_grammar<N, T>(grammar: &Grammar<N, T>) -> FirstSets {
        let mut sets = FirstSets(vec_with_size(
            grammar.nonterminals_len(),
            FirstSet {
                terminals: BTreeSet::new(),
                can_be_empty: false,
            },
        ));
        loop {
            if !sets.iteration(&grammar) {
                // if not changed
                break
            }
        }
        sets
    }

    fn iteration<N, T>(&mut self, grammar: &Grammar<N, T>) -> bool {
        let mut changed = false;
        for Rule { left, right } in grammar.rules() {
            let mut empty_count = 0;
            for symbol in right.iter() {
                match symbol {
                    Symbol::Nonterminal(n) => {
                        changed |= self.merge_first_set_to_first_set(*left, *n);
                        if self.first_set_of_nonterminal(*n).can_be_empty {
                            empty_count += 1;
                        } else {
                            break
                        }
                    },
                    Symbol::Terminal(t) => {
                        changed |= self.merge_nonterminal_to_first_set(*left, *t);
                        break
                    },
                }
            }
            if empty_count == right.len() {
                let can_be_empty = &mut self.first_set_of_nonterminal_mut(*left).can_be_empty;
                if !(*can_be_empty) {
                    changed = true;
                }
                *can_be_empty = true;
            }
        }
        changed
    }

    fn merge_nonterminal_to_first_set(
        &mut self,
        to: NonterminalIndex,
        with: TerminalIndex,
    ) -> bool {
        let first_set = self.first_set_of_nonterminal_mut(to);
        if !first_set.terminals.contains(&with) {
            first_set.terminals.insert(with);
            true
        } else {
            false
        }
    }

    fn merge_first_set_to_first_set(
        &mut self,
        to: NonterminalIndex,
        from: NonterminalIndex,
    ) -> bool {
        let mut from_set = self.first_set_of_nonterminal(from).terminals.clone();
        let to_set = &mut self.first_set_of_nonterminal_mut(to).terminals;
        let length_before = to_set.len();
        to_set.append(&mut from_set);
        let length_after = to_set.len();
        length_after != length_before
    }
}

impl<N, T> DisplayWith<Grammar<N, T>> for FirstSets
where
    N: fmt::Display,
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> Result<(), fmt::Error> {
        for i in grammar.nonterminal_indices() {
            if i.value() != 0 {
                writeln!(f)?;
            }
            write!(f, "{}", i.display_with(grammar))?;
            write!(f, ": ")?;
            write!(
                f,
                "{}",
                self.first_set_of_nonterminal(i).display_with(grammar)
            )?;
        }
        Ok(())
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Debug, Clone)]
pub struct FollowSet {
    pub terminals: BTreeSet<TerminalIndex>,
    pub can_be_end: bool,
}

impl fmt::Display for FollowSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{")?;
        let mut i = 0usize;
        let mut iter = self.terminals.iter();
        while i < self.terminals.len() {
            if i != 0usize {
                write!(f, ", ")?;
            }
            write!(f, "{}", iter.next().unwrap())?;
            i += 1;
        }
        if self.can_be_end {
            if i != 0usize {
                write!(f, ", ")?;
            }
            write!(f, "$")?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl<N, T> DisplayWith<Grammar<N, T>> for FollowSet
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> fmt::Result {
        write!(f, "{{")?;
        let mut i = 0usize;
        let mut iter = self.terminals.iter();
        while i < self.terminals.len() {
            if i != 0usize {
                write!(f, ", ")?;
            }
            write!(f, "{}", iter.next().unwrap().display_with(grammar))?;
            i += 1;
        }
        if self.can_be_end {
            if i != 0usize {
                write!(f, ", ")?;
            }
            write!(f, "$")?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct FollowSets(Vec<FollowSet>);

impl FollowSets {
    pub fn follow_set_of_nonterminal(&self, nonterminal: NonterminalIndex) -> &FollowSet {
        &self.0[nonterminal.value()]
    }

    fn follow_set_of_nonterminal_mut(&mut self, nonterminal: NonterminalIndex) -> &mut FollowSet {
        &mut self.0[nonterminal.value()]
    }

    pub fn of_grammar<N, T>(grammar: &Grammar<N, T>, first_sets: &FirstSets) -> FollowSets
    where
        N: Ord + Copy,
        T: Ord + Copy,
    {
        let mut sets = FollowSets(vec_with_size(
            grammar.nonterminals_len(),
            FollowSet {
                terminals: BTreeSet::new(),
                can_be_end: false,
            },
        ));
        sets.follow_set_of_nonterminal_mut(grammar.start_index())
            .can_be_end = true;
        loop {
            if !sets.iteration(grammar, first_sets) {
                // if not changed
                break
            }
        }
        sets
    }

    fn iteration<N, T>(&mut self, grammar: &Grammar<N, T>, first_sets: &FirstSets) -> bool {
        let mut changed = false;
        for Rule { left, right } in grammar.rules() {
            for (index, symbol) in right.iter().enumerate() {
                if let Symbol::Nonterminal(n) = symbol {
                    let right_first = first_sets.first_set_of_symbol_string(&right[index + 1..]);
                    // merge first
                    changed |= self.merge_first_set_to_follow_set(*n, &right_first);
                    if right_first.can_be_empty {
                        changed |= self.merge_follow_set_to_follow_set(*n, *left);
                    }
                }
            }
        }
        changed
    }

    fn merge_first_set_to_follow_set(&mut self, to: NonterminalIndex, from: &FirstSet) -> bool {
        let mut from_set = from.terminals.clone();
        let to_set = &mut self.follow_set_of_nonterminal_mut(to).terminals;
        let length_before = to_set.len();
        to_set.append(&mut from_set);
        let length_after = to_set.len();
        length_after != length_before
    }

    fn merge_follow_set_to_follow_set(
        &mut self,
        to: NonterminalIndex,
        from: NonterminalIndex,
    ) -> bool {
        let mut from_set = self.follow_set_of_nonterminal(from).clone();
        let to_set = self.follow_set_of_nonterminal_mut(to);
        let length_before = to_set.terminals.len();
        to_set.terminals.append(&mut from_set.terminals);
        let length_after = to_set.terminals.len();

        let mut changed = length_after != length_before;
        if from_set.can_be_end {
            if !to_set.can_be_end {
                changed = true;
            }
            to_set.can_be_end = true;
        }
        changed
    }
}

impl<N, T> DisplayWith<Grammar<N, T>> for FollowSets
where
    N: fmt::Display,
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> Result<(), fmt::Error> {
        for i in grammar.nonterminal_indices() {
            if i.value() != 0 {
                writeln!(f)?;
            }
            write!(f, "{}", i.display_with(grammar))?;
            write!(f, ": ")?;
            write!(
                f,
                "{}",
                self.follow_set_of_nonterminal(i).display_with(grammar)
            )?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::FirstSets;
    use super::FollowSets;
    use crate::grammar;
    #[test]
    fn simple() {
        let grammar = grammar! {
            start A;
            rule A -> '(', A, ')', A;
            rule A -> ε;
        };
        let first_sets = FirstSets::of_grammar(&grammar);
        let follow_sets = FollowSets::of_grammar(&grammar, &first_sets);

        assert_eq!(
            first_sets
                .first_set_of_nonterminal(grammar.nonterminal_index(&"A"))
                .terminals,
            vec![grammar.terminal_index(&'(')].into_iter().collect()
        );
        assert_eq!(
            first_sets
                .first_set_of_nonterminal(grammar.nonterminal_index(&"A"))
                .can_be_empty,
            true
        );
        assert_eq!(
            follow_sets
                .follow_set_of_nonterminal(grammar.nonterminal_index(&"A"))
                .terminals,
            vec![grammar.terminal_index(&')')].into_iter().collect()
        );
        assert_eq!(
            follow_sets
                .follow_set_of_nonterminal(grammar.nonterminal_index(&"A"))
                .can_be_end,
            true
        );
    }
}
