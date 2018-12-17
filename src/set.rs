use crate::grammar::{Grammar, Rule, Symbol};
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fmt;

#[derive(Debug, Clone)]
pub struct FirstSet<T> {
    pub terminals: BTreeSet<T>,
    pub can_be_empty: bool,
}

impl<T: fmt::Debug> fmt::Display for FirstSet<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{{")?;
        let mut index = 0usize;
        let mut iter = self.terminals.iter();
        while index < self.terminals.len() {
            if index != 0usize {
                write!(f, ", ")?;
            }
            write!(f, "{:?}", iter.next().unwrap())?;
            index += 1;
        }
        if self.can_be_empty {
            if index != 0usize {
                write!(f, ", ")?;
            }
            write!(f, "ε")?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct FirstSets<N, T> {
    pub sets: BTreeMap<N, FirstSet<T>>,
}

impl<T: fmt::Debug> fmt::Display for FirstSets<&'static str, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for (nonterminal, first_set) in &self.sets {
            writeln!(f, "{}: {}", nonterminal, first_set)?;
        }
        Ok(())
    }
}

pub fn first_sets_of_grammar<N, T>(grammar: &Grammar<N, T>) -> FirstSets<N, T>
where
    N: Ord + Copy,
    T: Ord + Copy,
{
    let mut sets = FirstSets {
        sets: BTreeMap::new(),
    };
    for nonterminal in grammar.index.keys() {
        sets.sets.insert(
            nonterminal.clone(),
            FirstSet {
                terminals: BTreeSet::new(),
                can_be_empty: false,
            },
        );
    }
    loop {
        if !first_sets_iteration(&mut sets, grammar) {
            // if not changed
            break
        }
    }
    sets
}

fn first_sets_iteration<N, T>(first_sets: &mut FirstSets<N, T>, grammar: &Grammar<N, T>) -> bool
where
    N: Ord + Copy,
    T: Ord + Copy,
{
    let mut changed = false;
    for Rule { left, right } in &grammar.rules {
        let mut empty_count = 0;
        for symbol in right {
            match symbol {
                Symbol::Nonterminal(n) => {
                    changed |= merge_to_first_sets(first_sets, left, n);
                    if first_sets.sets[n].can_be_empty {
                        empty_count += 1;
                    } else {
                        break
                    }
                },
                Symbol::Terminal(t) => {
                    changed |= add_to_first_sets(first_sets, left, *t);
                    break
                },
            }
        }
        if empty_count == right.len() {
            let can_be_empty = &mut first_sets.sets.get_mut(left).unwrap().can_be_empty;
            if !(*can_be_empty) {
                changed = true;
            }
            *can_be_empty = true;
        }
    }
    changed
}

fn add_to_first_sets<N, T>(first_sets: &mut FirstSets<N, T>, to: &N, with: T) -> bool
where
    N: Ord,
    T: Ord,
{
    let first_set = first_sets.sets.get_mut(&to).unwrap();
    if !first_set.terminals.contains(&with) {
        first_set.terminals.insert(with);
        true
    } else {
        false
    }
}

fn merge_to_first_sets<N, T>(first_sets: &mut FirstSets<N, T>, to: &N, from: &N) -> bool
where
    N: Ord,
    T: Ord + Copy,
{
    let mut from_set = first_sets.sets[from].terminals.clone();
    let to_set = &mut first_sets.sets.get_mut(to).unwrap().terminals;
    let length_before = to_set.len();
    to_set.append(&mut from_set);
    let length_after = to_set.len();
    length_after != length_before
}

pub fn first_set_of_symbol_string<N, T>(
    first_sets: &FirstSets<N, T>,
    symbol_string: &[Symbol<N, T>],
) -> FirstSet<T>
where
    N: Ord,
    T: Ord + Copy,
{
    let mut first_set = FirstSet {
        terminals: BTreeSet::new(),
        can_be_empty: false,
    };
    let mut empty_count = 0;
    for symbol in symbol_string {
        match symbol {
            Symbol::Nonterminal(n) => {
                let set = first_sets.sets.get(n).expect("unexpected nonterminal");
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

#[derive(Debug, Clone)]
pub struct FollowSet<T> {
    pub terminals: BTreeSet<T>,
    pub can_be_end: bool,
}

impl<T: fmt::Debug> fmt::Display for FollowSet<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{{")?;
        let mut index = 0usize;
        let mut iter = self.terminals.iter();
        while index < self.terminals.len() {
            if index != 0usize {
                write!(f, ", ")?;
            }
            write!(f, "{:?}", iter.next().unwrap())?;
            index += 1;
        }
        if self.can_be_end {
            if index != 0usize {
                write!(f, ", ")?;
            }
            write!(f, "$")?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct FollowSets<N, T> {
    pub sets: BTreeMap<N, FollowSet<T>>,
}

impl<T: fmt::Debug> fmt::Display for FollowSets<&'static str, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for (nonterminal, follow_set) in &self.sets {
            writeln!(f, "{}: {}", nonterminal, follow_set)?;
        }
        Ok(())
    }
}

pub fn follow_sets_of_grammar<N, T>(
    grammar: &Grammar<N, T>,
    first_sets: &FirstSets<N, T>,
) -> FollowSets<N, T>
where
    N: Ord + Copy,
    T: Ord + Copy,
{
    let mut sets = FollowSets {
        sets: BTreeMap::new(),
    };
    for nonterminal in grammar.index.keys() {
        sets.sets.insert(
            nonterminal.clone(),
            FollowSet {
                terminals: BTreeSet::new(),
                can_be_end: false,
            },
        );
    }
    sets.sets.get_mut(&grammar.start).unwrap().can_be_end = true;
    loop {
        if !follow_sets_iteration(&mut sets, grammar, first_sets) {
            // if not changed
            break
        }
    }
    sets
}

fn follow_sets_iteration<N, T>(
    follow_sets: &mut FollowSets<N, T>,
    grammar: &Grammar<N, T>,
    first_sets: &FirstSets<N, T>,
) -> bool
where
    N: Ord + Copy,
    T: Ord + Copy,
{
    let mut changed = false;
    for Rule { left, right } in &grammar.rules {
        for (index, symbol) in right.iter().enumerate() {
            if let Symbol::Nonterminal(n) = symbol {
                let right_first = first_set_of_symbol_string(first_sets, &right[index + 1..]);
                // merge first
                changed |= merge_first_to_follow_sets(follow_sets, n, &right_first);
                if right_first.can_be_empty {
                    changed |= merge_follow_to_follow_sets(follow_sets, n, left);
                }
            }
        }
    }
    changed
}

fn merge_first_to_follow_sets<N, T>(
    follow_sets: &mut FollowSets<N, T>,
    to: &N,
    from: &FirstSet<T>,
) -> bool
where
    N: Ord,
    T: Ord + Copy,
{
    let mut from_set = from.terminals.clone();
    let to_set = &mut follow_sets.sets.get_mut(to).unwrap().terminals;
    let length_before = to_set.len();
    to_set.append(&mut from_set);
    let length_after = to_set.len();
    length_after != length_before
}

fn merge_follow_to_follow_sets<N, T>(follow_sets: &mut FollowSets<N, T>, to: &N, from: &N) -> bool
where
    N: Ord,
    T: Ord + Copy,
{
    let mut from_set: FollowSet<T> = follow_sets.sets[from].clone();
    let to_set = follow_sets.sets.get_mut(to).unwrap();
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

#[cfg(test)]
mod test {
    use super::first_sets_of_grammar;
    use super::follow_sets_of_grammar;
    use crate::{grammar, grammar_impl};
    #[test]
    fn simple() {
        let grammar = grammar! {
            start A;
            rule A -> '(', A, ')', A;
            rule A -> ε;
        };
        let first = first_sets_of_grammar(&grammar);
        let follow = follow_sets_of_grammar(&grammar, &first);

        assert_eq!(first.sets["A"].terminals, vec!['('].into_iter().collect());
        assert_eq!(first.sets["A"].can_be_empty, true);
        assert_eq!(follow.sets["A"].terminals, vec![')'].into_iter().collect());
        assert_eq!(follow.sets["A"].can_be_end, true);
    }
}
