use crate::display::DisplayWith;
use crate::grammar::NonterminalIndex;
use crate::grammar::TerminalIndex;
use crate::grammar::{Grammar, Rule, Symbol};
use crate::utility::vec_with_size;
use serde::Deserialize;
use serde::Serialize;
use std::collections::BTreeSet;
use std::fmt;

/// A struct describing a FIRST set of something.
///
/// A FIRST set contains a set of terminals and epsilon.
#[derive(Serialize, Deserialize, Debug, Clone)]
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
            write!(f, "epsilon")?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl<N, T> DisplayWith<Grammar<N, T>> for FirstSet
where
    N: Ord,
    T: fmt::Debug + Ord,
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
            write!(f, "epsilon")?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

/// A struct describing the FIRST sets of every non-terminals of a grammar.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct FirstSets(Vec<FirstSet>);

impl FirstSets {
    /// Given `NonterminalIndex`, return a reference of the particular FIRST
    /// set.
    pub fn first_set_of_nonterminal(&self, nonterminal: NonterminalIndex) -> &FirstSet {
        &self.0[nonterminal.value()]
    }

    /// Given `NonterminalIndex`, return a mutable reference of the particular
    /// FIRST set.
    pub fn first_set_of_nonterminal_mut(&mut self, nonterminal: NonterminalIndex) -> &mut FirstSet {
        &mut self.0[nonterminal.value()]
    }

    /// Given a symbol string, return its FIRST set.
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
                        break;
                    }
                }
                Symbol::Terminal(t) => {
                    first_set.terminals.insert(*t);
                    break;
                }
            }
        }
        if empty_count == symbol_string.len() {
            first_set.can_be_empty = true;
        }
        first_set
    }
}

impl FirstSets {
    /// Calculate the FIRST sets of every non-terminals of the `grammar`.
    pub fn of_grammar<N, T>(grammar: &Grammar<N, T>) -> FirstSets
    where
        N: Ord,
        T: Ord,
    {
        let mut sets = FirstSets(vec_with_size(
            grammar.nonterminals_len(),
            FirstSet {
                terminals: BTreeSet::new(),
                can_be_empty: false,
            },
        ));
        loop {
            if !sets.iteration(grammar) {
                // if not changed
                break;
            }
        }
        sets
    }

    fn iteration<N, T>(&mut self, grammar: &Grammar<N, T>) -> bool
    where
        N: Ord,
        T: Ord,
    {
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
                            break;
                        }
                    }
                    Symbol::Terminal(t) => {
                        changed |= self.merge_nonterminal_to_first_set(*left, *t);
                        break;
                    }
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
    N: fmt::Display + Ord,
    T: fmt::Debug + Ord,
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

/// A struct describing a FOLLOW set of something.
///
/// A FOLLOW set contains a set of terminals and $.
#[derive(Serialize, Deserialize, Ord, PartialOrd, Eq, PartialEq, Debug, Clone)]
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
    N: Ord,
    T: fmt::Debug + Ord,
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

/// A struct describing the FOLLOW sets of every non-terminals of a grammar.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct FollowSets(Vec<FollowSet>);

impl FollowSets {
    /// Given `NonterminalIndex`, return a reference of the particular FOLLOW
    /// set.
    pub fn follow_set_of_nonterminal(&self, nonterminal: NonterminalIndex) -> &FollowSet {
        &self.0[nonterminal.value()]
    }

    /// Given `NonterminalIndex`, return a mutable reference of the particular
    /// FOLLOW set.
    pub fn follow_set_of_nonterminal_mut(
        &mut self,
        nonterminal: NonterminalIndex,
    ) -> &mut FollowSet {
        &mut self.0[nonterminal.value()]
    }
}

impl FollowSets {
    /// Calculate the FOLLOW sets of every non-terminals of the `grammar`.
    pub fn of_grammar<N, T>(grammar: &Grammar<N, T>, first_sets: &FirstSets) -> FollowSets
    where
        N: Ord,
        T: Ord,
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
                break;
            }
        }
        sets
    }

    fn iteration<N, T>(&mut self, grammar: &Grammar<N, T>, first_sets: &FirstSets) -> bool
    where
        N: Ord,
        T: Ord,
    {
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
    N: fmt::Display + Ord,
    T: fmt::Debug + Ord,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> Result<(), fmt::Error>
    where
        N: Ord,
        T: Ord,
    {
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
    use crate::display::DisplayWith;
    use crate::grammar;
    use crate::grammar::Grammar;

    fn assert_first_set<N, T>(
        first_sets: &FirstSets,
        grammar: &Grammar<N, T>,
        nonterminal: &N,
        terminals: &[T],
        can_be_empty: bool,
    ) where
        N: std::cmp::Ord,
        T: std::cmp::Ord,
    {
        let set = first_sets.first_set_of_nonterminal(grammar.nonterminal_index(nonterminal));
        assert_eq!(
            set.terminals,
            terminals
                .iter()
                .map(|t| grammar.terminal_index(t))
                .collect()
        );
        assert_eq!(set.can_be_empty, can_be_empty);
    }

    fn assert_follow_set<N, T>(
        follow_set: &FollowSets,
        grammar: &Grammar<N, T>,
        nonterminal: &N,
        terminals: &[T],
        can_be_end: bool,
    ) where
        N: std::cmp::Ord,
        T: std::cmp::Ord,
    {
        let set = follow_set.follow_set_of_nonterminal(grammar.nonterminal_index(nonterminal));
        assert_eq!(
            set.terminals,
            terminals
                .iter()
                .map(|t| grammar.terminal_index(t))
                .collect()
        );
        assert_eq!(set.can_be_end, can_be_end);
    }

    fn example_grammar() -> Grammar<&'static str, &'static str> {
        grammar! {
            start E;
            rule E -> T, E1;
            rule E1 -> "+", T, E1;
            rule E1 -> epsilon;
            rule T -> F, T1;
            rule T1 -> "*", F, T1;
            rule T1 -> epsilon;
            rule F -> "(", E, ")";
            rule F -> "id";
        }
    }

    #[test]
    fn sets_of_empty_grammar() {
        let grammar = grammar! {
            start A;
            rule A -> 'a';
        };
        let first_sets = FirstSets::of_grammar(&grammar);
        let follow_sets = FollowSets::of_grammar(&grammar, &first_sets);

        assert_first_set(&first_sets, &grammar, &"A", &['a'], false);
        assert_follow_set(&follow_sets, &grammar, &"A", &[], true);
    }

    #[test]
    fn sets_of_simple_grammar() {
        let grammar = grammar! {
            start A;
            rule A -> '(', A, ')', A;
            rule A -> epsilon;
        };
        let first_sets = FirstSets::of_grammar(&grammar);
        let follow_sets = FollowSets::of_grammar(&grammar, &first_sets);

        assert_first_set(&first_sets, &grammar, &"A", &['('], true);
        assert_follow_set(&follow_sets, &grammar, &"A", &[')'], true);
    }

    #[test]
    fn sets_of_example_grammar() {
        let grammar = example_grammar();
        let first_sets = FirstSets::of_grammar(&grammar);
        let follow_sets = FollowSets::of_grammar(&grammar, &first_sets);

        assert_first_set(&first_sets, &grammar, &"E", &["(", "id"], false);
        assert_first_set(&first_sets, &grammar, &"E1", &["+"], true);
        assert_first_set(&first_sets, &grammar, &"T", &["(", "id"], false);
        assert_first_set(&first_sets, &grammar, &"T1", &["*"], true);
        assert_first_set(&first_sets, &grammar, &"F", &["(", "id"], false);

        assert_follow_set(&follow_sets, &grammar, &"E", &[")"], true);
        assert_follow_set(&follow_sets, &grammar, &"E1", &[")"], true);
        assert_follow_set(&follow_sets, &grammar, &"T", &["+", ")"], true);
        assert_follow_set(&follow_sets, &grammar, &"T1", &["+", ")"], true);
        assert_follow_set(&follow_sets, &grammar, &"F", &["+", "*", ")"], true);
    }

    #[test]
    fn first_sets_before_can_be_empty() {
        let grammar = grammar! {
            start S;
            rule S -> A, B;
            rule A -> 'a';
            rule A -> epsilon;
            rule B -> 'b';
            rule B -> epsilon;
        };
        let first_sets = FirstSets::of_grammar(&grammar);
        assert_first_set(&first_sets, &grammar, &"S", &['a', 'b'], true);
        assert_first_set(&first_sets, &grammar, &"A", &['a'], true);
        assert_first_set(&first_sets, &grammar, &"B", &['b'], true);
    }

    #[test]
    fn display_first_sets_with_grammar() {
        let grammar = example_grammar();
        let first_sets = FirstSets::of_grammar(&grammar);
        assert_eq!(
            format!("\n{}\n", first_sets.display_with(&grammar)),
            r#"
E: {"(", "id"}
T: {"(", "id"}
E1: {"+", epsilon}
F: {"(", "id"}
T1: {"*", epsilon}
"#
        );
    }

    #[test]
    fn display_follow_sets_with_grammar() {
        let grammar = example_grammar();
        let first_sets = FirstSets::of_grammar(&grammar);
        let follow_sets = FollowSets::of_grammar(&grammar, &first_sets);
        assert_eq!(
            format!("\n{}\n", follow_sets.display_with(&grammar)),
            r#"
E: {")", $}
T: {"+", ")", $}
E1: {")", $}
F: {"+", "*", ")", $}
T1: {"+", ")", $}
"#
        );
    }

    #[test]
    fn display_first_set() {
        let grammar = example_grammar();
        let first_sets = FirstSets::of_grammar(&grammar);
        assert_eq!(
            format!(
                "{}",
                first_sets.first_set_of_nonterminal(grammar.nonterminal_index(&"E"))
            ),
            "{t2, t4}"
        );
        assert_eq!(
            format!(
                "{}",
                first_sets.first_set_of_nonterminal(grammar.nonterminal_index(&"T"))
            ),
            "{t2, t4}"
        );
        assert_eq!(
            format!(
                "{}",
                first_sets.first_set_of_nonterminal(grammar.nonterminal_index(&"E1"))
            ),
            "{t0, epsilon}"
        );
        assert_eq!(
            format!(
                "{}",
                first_sets.first_set_of_nonterminal(grammar.nonterminal_index(&"F"))
            ),
            "{t2, t4}"
        );
        assert_eq!(
            format!(
                "{}",
                first_sets.first_set_of_nonterminal(grammar.nonterminal_index(&"T1"))
            ),
            "{t1, epsilon}"
        );
    }

    #[test]
    fn display_follow_set() {
        let grammar = example_grammar();
        let first_sets = FirstSets::of_grammar(&grammar);
        let follow_sets = FollowSets::of_grammar(&grammar, &first_sets);
        assert_eq!(
            format!(
                "{}",
                follow_sets.follow_set_of_nonterminal(grammar.nonterminal_index(&"E"))
            ),
            "{t3, $}"
        );
        assert_eq!(
            format!(
                "{}",
                follow_sets.follow_set_of_nonterminal(grammar.nonterminal_index(&"T"))
            ),
            "{t0, t3, $}"
        );
        assert_eq!(
            format!(
                "{}",
                follow_sets.follow_set_of_nonterminal(grammar.nonterminal_index(&"E1"))
            ),
            "{t3, $}"
        );
        assert_eq!(
            format!(
                "{}",
                follow_sets.follow_set_of_nonterminal(grammar.nonterminal_index(&"F"))
            ),
            "{t0, t1, t3, $}"
        );
        assert_eq!(
            format!(
                "{}",
                follow_sets.follow_set_of_nonterminal(grammar.nonterminal_index(&"T1"))
            ),
            "{t0, t3, $}"
        );
    }
}
