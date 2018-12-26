use crate::display::DisplayWith;
use crate::grammar::Grammar;
use crate::grammar::NonterminalIndex;
use crate::grammar::Symbol;
use crate::grammar::TerminalIndex;
use crate::lr0;
use crate::set::FirstSet;
use crate::set::FirstSets;
use crate::set::FollowSet;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fmt;

#[derive(Ord, PartialOrd, Eq, PartialEq, Debug, Clone, Default)]
pub struct ItemSet(pub BTreeMap<lr0::item::Item, FollowSet>);

impl<N, T> DisplayWith<Grammar<N, T>> for ItemSet
where
    N: fmt::Display,
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> fmt::Result {
        writeln!(f, "{{")?;
        for (lr0item, follow) in self.0.iter() {
            writeln!(f, "    {},", ItemRef { lr0item, follow }.display_with(grammar))?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Debug, Clone, Copy)]
pub struct ItemRef<'r> {
    pub lr0item: &'r lr0::item::Item,
    pub follow: &'r FollowSet,
}

impl<'r, N, T> DisplayWith<Grammar<N, T>> for ItemRef<'r>
where
    N: fmt::Display,
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> fmt::Result {
        write!(f, "[")?;
        let rule = grammar.rule(self.lr0item.rule);

        write!(f, "{}", rule.left.display_with(grammar))?;
        write!(f, " -> ")?;
        for i in 0..=rule.right.len() {
            if i == self.lr0item.location {
                if i != 0 {
                    write!(f, " ")?;
                }
                write!(f, "·")?;
            }
            if i < rule.right.len() {
                if i != 0 || i == self.lr0item.location {
                    write!(f, " ")?;
                }
                write!(f, "{}", rule.right[i].display_with(grammar))?;
            }
        }
        write!(f, ", ")?;
        if self.follow.terminals.is_empty() && !self.follow.can_be_end {
            panic!("invalid follow set for lr(1) item")
        } else {
            let mut i = 0usize;
            let mut iter = self.follow.terminals.iter();
            while i < self.follow.terminals.len() {
                if i != 0usize {
                    write!(f, "/")?;
                }
                write!(f, "{}", iter.next().unwrap().display_with(grammar))?;
                i += 1;
            }
            if self.follow.can_be_end {
                if i != 0usize {
                    write!(f, "/")?;
                }
                write!(f, "$")?;
            }
        }
        write!(f, "]")?;
        Ok(())
    }
}

impl ItemSet {
    pub fn new() -> Self {
        ItemSet(BTreeMap::new())
    }

    pub fn closure<N, T>(mut self, grammar: &Grammar<N, T>, first_sets: &FirstSets) -> Self {
        loop {
            if !self.closure_iteration(grammar, first_sets) {
                // if not changed
                break
            }
        }
        self
    }

    fn closure_iteration<N, T>(&mut self, grammar: &Grammar<N, T>, first_sets: &FirstSets) -> bool {
        let mut change = ItemSet::new();
        for (lr0item, follow) in &self.0 {
            if let Some(Symbol::Nonterminal(nonterminal)) = lr0item.next_symbol(grammar) {
                let first_set = first_sets.first_set_of_symbol_string(
                    &grammar.rule(lr0item.rule).right[lr0item.location + 1..],
                );
                let set = self.derive_from_nonterminal(grammar, first_set, follow, nonterminal);
                change.merge_item_set(set);
            }
        }
        self.merge_item_set(change)
    }

    fn derive_from_nonterminal<N, T>(
        &self,
        grammar: &Grammar<N, T>,
        after_first_set: FirstSet,
        origin_follow_set: &FollowSet,
        nonterminal: NonterminalIndex,
    ) -> ItemSet {
        let mut map = BTreeMap::new();
        let follow = {
            let mut set = FollowSet {
                terminals: after_first_set.terminals,
                can_be_end: false,
            };
            if after_first_set.can_be_empty {
                set.terminals
                    .append(&mut origin_follow_set.clone().terminals);
                set.can_be_end = origin_follow_set.can_be_end;
            }
            set
        };
        for rule in grammar.rules_with_left(nonterminal) {
            let lr0item = lr0::item::Item {
                rule: *rule,
                location: 0,
            };
            map.insert(lr0item, follow.clone());
        }
        ItemSet(map)
    }

    fn merge_item_set(&mut self, item_set: ItemSet) -> bool {
        let mut changed = false;
        for (lr0item, mut follow) in item_set.0 {
            match self.0.get_mut(&lr0item) {
                None => {
                    changed = true;
                    self.0.insert(lr0item, follow);
                },
                Some(origin_follow) => {
                    let origin_len = origin_follow.terminals.len();
                    origin_follow.terminals.append(&mut follow.terminals);
                    if origin_len != origin_follow.terminals.len() {
                        changed = true;
                    }
                    if follow.can_be_end && !origin_follow.can_be_end {
                        changed = true;
                        origin_follow.can_be_end = true;
                    }
                },
            }
        }
        changed
    }

    pub fn next_nonterminals<N, T>(&self, grammar: &Grammar<N, T>) -> BTreeSet<NonterminalIndex> {
        self.0
            .iter()
            .filter_map(|(item, _follow)| {
                if let Some(Symbol::Nonterminal(n)) = item.next_symbol(grammar) {
                    Some(n)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn next_terminals<N, T>(&self, grammar: &Grammar<N, T>) -> BTreeSet<TerminalIndex> {
        self.0
            .iter()
            .filter_map(|(item, _follow)| {
                if let Some(Symbol::Terminal(t)) = item.next_symbol(grammar) {
                    Some(t)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn finished<N, T>(&self, grammar: &Grammar<N, T>) -> BTreeSet<ItemRef> {
        self.0
            .iter()
            .filter_map(|(lr0item, follow)| {
                if lr0item.finished(grammar) {
                    Some(ItemRef { lr0item, follow })
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn go_nonterminal<N, T>(
        &self,
        grammar: &Grammar<N, T>,
        nonterminal: NonterminalIndex,
    ) -> ItemSet {
        ItemSet(
            self.0
                .iter()
                .filter_map(|(item, follow)| {
                    if let Some(Symbol::Nonterminal(n)) = item.next_symbol(grammar) {
                        if n == nonterminal {
                            Some((
                                lr0::item::Item {
                                    rule: item.rule,
                                    location: item.location + 1,
                                },
                                follow.clone(),
                            ))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
                .collect(),
        )
    }

    pub fn go_terminal<N, T>(&self, grammar: &Grammar<N, T>, terminal: TerminalIndex) -> ItemSet {
        ItemSet(
            self.0
                .iter()
                .filter_map(|(item, follow)| {
                    if let Some(Symbol::Terminal(t)) = item.next_symbol(grammar) {
                        if t == terminal {
                            Some((
                                lr0::item::Item {
                                    rule: item.rule,
                                    location: item.location + 1,
                                },
                                follow.clone(),
                            ))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
                .collect(),
        )
    }
}
