use crate::grammar::Grammar;
use crate::grammar::GrammarBuilder;
use crate::grammar::NonterminalIndex;
use crate::grammar::TerminalIndex;
use crate::lr0;
use crate::lr1::item::ItemRef;
use crate::lr1::item::ItemSet;
use crate::lr1::table::Action;
use crate::lr1::table::EndAction;
use crate::lr1::table::Goto;
use crate::lr1::table::Table;
use crate::set::FirstSets;
use crate::set::FollowSet;
use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::collections::BTreeSet;

type ItemSetIndex = usize;

#[derive(Debug, Clone)]
pub struct Generator<N, T>
where
    N: Ord,
    T: Ord,
{
    extended_grammar: Grammar<N, T>,
    canonical_collection_index: BTreeMap<ItemSet, ItemSetIndex>,
    canonical_collection: Vec<ItemSet>, // indexed with ItemSetIndex
    go_nonterminal: Vec<BTreeMap<NonterminalIndex, ItemSetIndex>>,
    go_terminal: Vec<BTreeMap<TerminalIndex, ItemSetIndex>>,
}

impl<N, T> Generator<N, T>
where
    N: Ord + Clone,
    T: Ord + Clone,
{
    pub fn construct(grammar: &Grammar<N, T>, first_sets: &FirstSets, new_start: N) -> Self {
        let mut generator = Self {
            extended_grammar: extend_grammar(grammar, new_start),
            canonical_collection_index: BTreeMap::new(),
            canonical_collection: Vec::new(),
            go_nonterminal: Vec::new(),
            go_terminal: Vec::new(),
        };
        generator.build(first_sets);
        generator
    }

    pub fn generate(&self, origin_grammar: &Grammar<N, T>) -> Option<Table> {
        let mut table = Table::new(origin_grammar); // use original grammar (not extended one) to build table
        let states = {
            // states is indexed by ItemSetIndex
            let mut states = Vec::new();
            for _ in 0..self.canonical_collection.len() {
                states.push(table.push_state());
            }
            states
        };
        for (item_set_index, state) in states.iter().enumerate() {
            let item_set = &self.canonical_collection[item_set_index];
            for (terminal, go_state) in &self.go_terminal[item_set_index] {
                if let Action::Error = table.action(*state, *terminal) {
                    *table.action_mut(*state, *terminal) = Action::Shift(states[*go_state]);
                } else {
                    return None;
                }
            }
            for ItemRef { lr0item, follow } in item_set.finished(&self.extended_grammar) {
                for terminal in &follow.terminals {
                    if let Action::Error = table.action(*state, *terminal) {
                        *table.action_mut(*state, *terminal) = Action::Reduce(lr0item.rule);
                    } else {
                        return None;
                    }
                }
                if follow.can_be_end {
                    if let EndAction::Error = table.end_action(*state) {
                        if self.extended_grammar.rule(lr0item.rule).left
                            == self.extended_grammar.start_index()
                        {
                            *table.end_action_mut(*state) = EndAction::Accept;
                        } else {
                            *table.end_action_mut(*state) = EndAction::Reduce(lr0item.rule);
                        }
                    } else {
                        return None;
                    }
                }
            }
            for (nonterminal, go_state) in &self.go_nonterminal[item_set_index] {
                if let Goto::Error = table.goto(*state, *nonterminal) {
                    *table.goto_mut(*state, *nonterminal) = Goto::Goto(states[*go_state]);
                } else {
                    return None;
                }
            }
        }
        Some(table)
    }

    pub fn canonical_collection(&self) -> &[ItemSet] {
        &self.canonical_collection
    }

    pub fn extended_grammar(&self) -> &Grammar<N, T> {
        &self.extended_grammar
    }

    fn build(&mut self, first_sets: &FirstSets) {
        let initial_item_set = ItemSet({
            let mut map = BTreeMap::new();
            let rule = *self
                .extended_grammar
                .rules_with_left(self.extended_grammar.start_index())
                .next()
                .unwrap();
            map.insert(
                lr0::item::Item { rule, location: 0 },
                FollowSet {
                    terminals: BTreeSet::new(),
                    can_be_end: true,
                },
            );
            map
        })
        .closure(&self.extended_grammar, first_sets);
        let initial_index = self.add_and_get_item_set(initial_item_set);

        let mut current_index = initial_index;
        loop {
            let next_nonterminals =
                self.canonical_collection[current_index].next_nonterminals(&self.extended_grammar);
            let next_terminals =
                self.canonical_collection[current_index].next_terminals(&self.extended_grammar);
            for nonterminal in next_nonterminals {
                let set = self.canonical_collection[current_index]
                    .go_nonterminal(&self.extended_grammar, nonterminal)
                    .closure(&self.extended_grammar, first_sets);
                let index = self.add_and_get_item_set(set);
                self.go_nonterminal[current_index].insert(nonterminal, index);
            }
            for terminal in next_terminals {
                let set = self.canonical_collection[current_index]
                    .go_terminal(&self.extended_grammar, terminal)
                    .closure(&self.extended_grammar, first_sets);
                let index = self.add_and_get_item_set(set);
                self.go_terminal[current_index].insert(terminal, index);
            }
            current_index += 1;
            if current_index == self.canonical_collection.len() {
                break;
            }
        }
    }

    fn add_and_get_item_set(&mut self, item_set: ItemSet) -> ItemSetIndex {
        match self.canonical_collection_index.entry(item_set.clone()) {
            Entry::Vacant(v) => {
                let index = self.canonical_collection.len();
                self.canonical_collection.push(item_set);
                self.go_terminal.push(BTreeMap::new());
                self.go_nonterminal.push(BTreeMap::new());
                *v.insert(index)
            }
            Entry::Occupied(o) => *o.get(),
        }
    }
}

pub fn extend_grammar<N, T>(grammar: &Grammar<N, T>, nonterminal: N) -> Grammar<N, T>
where
    N: Ord + Clone,
    T: Ord + Clone,
{
    assert!(
        !grammar.contains_nonterminal(&nonterminal),
        "new start is existed in grammar"
    );
    let start = grammar.start().clone();
    let builder = GrammarBuilder::from_grammar(grammar.clone())
        .push_rule_left(nonterminal.clone())
        .push_rule_right_nonterminal(start)
        .start(nonterminal);
    builder.build()
}
