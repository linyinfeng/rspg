use crate::display::DisplayWith;
use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::fmt;
use std::ops::Deref;
use std::ops::DerefMut;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Copy)]
pub struct NonterminalIndex(pub(self) usize);

impl NonterminalIndex {
    pub fn value(self) -> usize {
        self.0
    }
}

impl fmt::Display for NonterminalIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "N{}", self.value())
    }
}

impl<N, T> DisplayWith<Grammar<N, T>> for NonterminalIndex
where
    N: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> fmt::Result {
        write!(f, "{}", grammar.nonterminal(*self))
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Copy)]
pub struct TerminalIndex(pub(self) usize);

impl TerminalIndex {
    pub fn value(self) -> usize {
        self.0
    }
}

impl fmt::Display for TerminalIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "t{}", self.value())
    }
}

impl<N, T> DisplayWith<Grammar<N, T>> for TerminalIndex
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> fmt::Result {
        write!(f, "{:?}", grammar.terminal(*self))
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Copy)]
pub struct RuleIndex(pub(self) usize);

impl RuleIndex {
    pub fn value(self) -> usize {
        self.0
    }
}

impl fmt::Display for RuleIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value())
    }
}

impl<N, T> DisplayWith<Grammar<N, T>> for RuleIndex
where
    N: fmt::Display,
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> Result<(), fmt::Error> {
        write!(f, "{}", grammar.rule(*self).display_with(grammar))
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Copy)]
pub enum Symbol {
    Nonterminal(NonterminalIndex),
    Terminal(TerminalIndex),
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Symbol::Nonterminal(n) => write!(f, "{}", n),
            Symbol::Terminal(t) => write!(f, "{}", t),
        }
    }
}

impl<N, T> DisplayWith<Grammar<N, T>> for Symbol
where
    N: fmt::Display,
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> Result<(), fmt::Error> {
        match self {
            Symbol::Nonterminal(n) => write!(f, "{}", n.display_with(grammar)),
            Symbol::Terminal(t) => write!(f, "{}", t.display_with(grammar)),
        }
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Default, Hash, Clone)]
pub struct SymbolString(pub Vec<Symbol>);

impl From<Vec<Symbol>> for SymbolString {
    fn from(v: Vec<Symbol>) -> Self {
        SymbolString(v)
    }
}

impl Deref for SymbolString {
    type Target = Vec<Symbol>;

    fn deref(&self) -> &<Self as Deref>::Target {
        &self.0
    }
}

impl DerefMut for SymbolString {
    fn deref_mut(&mut self) -> &mut <Self as Deref>::Target {
        &mut self.0
    }
}

impl SymbolString {
    pub fn new() -> SymbolString {
        SymbolString(Vec::new())
    }
}

impl fmt::Display for SymbolString {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if self.is_empty() {
            write!(f, "ε")?;
        } else {
            for (i, symbol) in self.iter().enumerate() {
                if i != 0 {
                    write!(f, " ")?;
                }
                write!(f, "{}", symbol)?;
            }
        }
        Ok(())
    }
}

impl<N, T> DisplayWith<Grammar<N, T>> for SymbolString
where
    N: fmt::Display,
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> Result<(), fmt::Error> {
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

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone)]
pub struct Rule {
    pub left: NonterminalIndex,
    pub right: SymbolString,
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.left)?;
        write!(f, " -> ")?;
        write!(f, "{}", self.right)?;
        Ok(())
    }
}

impl<N, T> DisplayWith<Grammar<N, T>> for Rule
where
    N: fmt::Display,
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter, grammar: &Grammar<N, T>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.left.display_with(grammar))?;
        write!(f, " -> ")?;
        write!(f, "{}", self.right.display_with(grammar))?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Grammar<N, T> {
    pub(self) nonterminals: Vec<(N, Vec<RuleIndex>)>,
    pub(self) terminals: Vec<T>,
    pub(self) rules: Vec<Rule>,
    pub(self) start: NonterminalIndex,
    pub(self) nonterminal_index: BTreeMap<N, NonterminalIndex>,
    pub(self) terminal_index: BTreeMap<T, TerminalIndex>,
}

impl<N, T> Grammar<N, T> {
    pub fn start(&self) -> &N {
        self.nonterminal(self.start_index())
    }

    pub fn start_index(&self) -> NonterminalIndex {
        self.start
    }

    pub fn nonterminals_len(&self) -> usize {
        self.nonterminals.len()
    }

    pub fn terminals_len(&self) -> usize {
        self.terminals.len()
    }

    pub fn rules_len(&self) -> usize {
        self.rules.len()
    }

    pub fn nonterminals(&self) -> impl Iterator<Item = &N> {
        self.nonterminals.iter().map(|entry| &entry.0)
    }

    pub fn terminals(&self) -> impl Iterator<Item = &T> {
        self.terminals.iter()
    }

    pub fn rules(&self) -> impl Iterator<Item = &Rule> {
        self.rules.iter()
    }

    pub fn rules_with_left(
        &self,
        nonterminal: NonterminalIndex,
    ) -> impl Iterator<Item = &RuleIndex> {
        self.nonterminals[nonterminal.0].1.iter()
    }

    pub fn nonterminal(&self, index: NonterminalIndex) -> &N {
        &self.nonterminals[index.0].0
    }

    pub fn terminal(&self, index: TerminalIndex) -> &T {
        &self.terminals[index.0]
    }

    pub fn rule(&self, index: RuleIndex) -> &Rule {
        &self.rules[index.0]
    }

    pub fn nonterminal_indices(&self) -> impl Iterator<Item = NonterminalIndex> {
        (0..self.nonterminals.len()).map(NonterminalIndex)
    }

    pub fn terminal_indices(&self) -> impl Iterator<Item = TerminalIndex> {
        (0..self.terminals.len()).map(TerminalIndex)
    }

    pub fn rule_indices(&self) -> impl Iterator<Item = RuleIndex> {
        (0..self.rules.len()).map(RuleIndex)
    }
}

impl<N, T> Grammar<N, T>
where
    N: Ord,
{
    pub fn nonterminal_index(&self, nonterminal: &N) -> NonterminalIndex {
        self.nonterminal_index[nonterminal]
    }

    pub fn contains_nonterminal(&self, nonterminal: &N) -> bool {
        self.nonterminal_index.contains_key(nonterminal)
    }
}

impl<N, T> Grammar<N, T>
where
    T: Ord,
{
    pub fn terminal_index(&self, terminal: &T) -> TerminalIndex {
        self.terminal_index[terminal]
    }

    pub fn contains_terminal(&self, terminal: &T) -> bool {
        self.terminal_index.contains_key(terminal)
    }
}

impl<N, T> fmt::Display for Grammar<N, T>
where
    N: fmt::Display,
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        writeln!(f, "grammar {{")?;
        write!(f, "    start ")?;
        writeln!(f, "{}", self.start.display_with(self))?;
        for (i, rule) in self.rules.iter().enumerate() {
            write!(f, "    rule {}: ", i)?;
            writeln!(f, "{}", rule.display_with(self))?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct GrammarBuilder<N, T> {
    nonterminals: Vec<(N, Vec<RuleIndex>)>,
    terminals: Vec<T>,
    rules: Vec<Rule>,
    start: Option<NonterminalIndex>,
    nonterminal_index: BTreeMap<N, NonterminalIndex>,
    terminal_index: BTreeMap<T, TerminalIndex>,
}

impl<N, T> GrammarBuilder<N, T>
where
    N: Ord,
    T: Ord,
{
    pub fn new() -> Self {
        Self {
            nonterminals: Vec::new(),
            terminals: Vec::new(),
            rules: Vec::new(),
            start: None,
            nonterminal_index: BTreeMap::new(),
            terminal_index: BTreeMap::new(),
        }
    }
}

impl<N, T> Default for GrammarBuilder<N, T>
where
    N: Ord,
    T: Ord,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<N, T> GrammarBuilder<N, T>
where
    N: Ord + Clone,
    T: Ord + Clone,
{
    pub fn from_grammar(grammar: Grammar<N, T>) -> Self {
        Self {
            nonterminals: grammar.nonterminals,
            terminals: grammar.terminals,
            rules: grammar.rules,
            start: Some(grammar.start),
            nonterminal_index: grammar.nonterminal_index,
            terminal_index: grammar.terminal_index,
        }
    }

    pub fn start(mut self, start: N) -> Self {
        let index = self.add_and_get_nonterminal(start);
        self.start = Some(index);
        self
    }

    pub fn push_rule_left(mut self, left: N) -> Self {
        let nonterminal_index = self.add_and_get_nonterminal(left);
        let rule_index = RuleIndex(self.rules.len());
        self.rules.push(Rule {
            left: nonterminal_index,
            right: SymbolString::new(),
        });
        self.nonterminals[nonterminal_index.0].1.push(rule_index);
        self
    }

    pub fn push_rule_right_nonterminal(mut self, nonterminal: N) -> Self {
        let nonterminal_index = self.add_and_get_nonterminal(nonterminal);
        self.rules
            .last_mut()
            .expect("push rule first")
            .right
            .push(Symbol::Nonterminal(nonterminal_index));
        self
    }

    pub fn push_rule_right_terminal(mut self, terminal: T) -> Self {
        let terminal_index = self.add_and_get_terminal(terminal);
        self.rules
            .last_mut()
            .expect("push rule first")
            .right
            .push(Symbol::Terminal(terminal_index));
        self
    }

    fn add_and_get_nonterminal(&mut self, nonterminal: N) -> NonterminalIndex {
        match self.nonterminal_index.entry(nonterminal.clone()) {
            Entry::Vacant(v) => {
                let new_index = NonterminalIndex(self.nonterminals.len());
                self.nonterminals.push((nonterminal, Vec::new()));
                *v.insert(new_index)
            },
            Entry::Occupied(o) => *o.get(),
        }
    }

    fn add_and_get_terminal(&mut self, terminal: T) -> TerminalIndex {
        match self.terminal_index.entry(terminal.clone()) {
            Entry::Vacant(v) => {
                let new_index = TerminalIndex(self.terminals.len());
                self.terminals.push(terminal);
                *v.insert(new_index)
            },
            Entry::Occupied(o) => *o.get(),
        }
    }

    pub fn build(self) -> Grammar<N, T> {
        Grammar {
            nonterminals: self.nonterminals,
            terminals: self.terminals,
            rules: self.rules,
            start: self.start.expect("expect start"),
            nonterminal_index: self.nonterminal_index,
            terminal_index: self.terminal_index,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::display::DisplayWith;
    use crate::grammar;
    use crate::grammar::Grammar;

    fn example_grammar() -> Grammar<&'static str, &'static str> {
        grammar! {
            start E;
            rule E -> T, E1;
            rule E1 -> "+", T, E1;
            rule E1 -> ε;
            rule T -> F, T1;
            rule T1 -> "*", F, T1;
            rule T1 -> ε;
            rule F -> "(", E, ")";
            rule F -> "id";
        }
    }

    #[test]
    fn grammar_builder() {
        let grammar = example_grammar();
        assert_eq!(grammar.start(), &"E");
        assert_eq!(grammar.start_index(), grammar.nonterminal_index(&"E"));
        assert_eq!(grammar.nonterminals_len(), 5);
        assert_eq!(grammar.terminals_len(), 5);
        assert_eq!(grammar.rules_len(), 8);
        assert_eq!(
            grammar.nonterminals().collect::<Vec<_>>(),
            &[&"E", &"T", &"E1", &"F", &"T1"]
        );
        assert_eq!(
            grammar.terminals().collect::<Vec<_>>(),
            &[&"+", &"*", &"(", &")", &"id"]
        );
        assert_eq!(
            grammar
                .rules()
                .map(|r| r.display_with(&grammar))
                .map(|mix| mix.to_string())
                .collect::<Vec<_>>(),
            &[
                r#"E -> T E1"#,
                r#"E1 -> "+" T E1"#,
                r#"E1 -> ε"#,
                r#"T -> F T1"#,
                r#"T1 -> "*" F T1"#,
                r#"T1 -> ε"#,
                r#"F -> "(" E ")""#,
                r#"F -> "id""#,
            ]
        );
        assert_eq!(
            grammar
                .rules_with_left(grammar.nonterminal_index(&"E"))
                .map(|r| r.display_with(&grammar))
                .map(|mix| mix.to_string())
                .collect::<Vec<_>>(),
            &[r#"E -> T E1"#,]
        );
        assert_eq!(
            grammar
                .rules_with_left(grammar.nonterminal_index(&"E1"))
                .map(|r| r.display_with(&grammar))
                .map(|mix| mix.to_string())
                .collect::<Vec<_>>(),
            &[r#"E1 -> "+" T E1"#, r#"E1 -> ε"#,]
        );
        assert_eq!(
            grammar
                .rules_with_left(grammar.nonterminal_index(&"T"))
                .map(|r| r.display_with(&grammar))
                .map(|mix| mix.to_string())
                .collect::<Vec<_>>(),
            &[r#"T -> F T1"#,]
        );
        assert_eq!(
            grammar
                .rules_with_left(grammar.nonterminal_index(&"T1"))
                .map(|r| r.display_with(&grammar))
                .map(|mix| mix.to_string())
                .collect::<Vec<_>>(),
            &[r#"T1 -> "*" F T1"#, r#"T1 -> ε"#,]
        );
        assert_eq!(
            grammar
                .rules_with_left(grammar.nonterminal_index(&"F"))
                .map(|r| r.display_with(&grammar))
                .map(|mix| mix.to_string())
                .collect::<Vec<_>>(),
            &[r#"F -> "(" E ")""#, r#"F -> "id""#,]
        );
        assert!(grammar
            .nonterminals()
            .map(|n| (grammar.nonterminal_index(n), n))
            .all(|(i, n)| grammar.nonterminal(i) == n));
        assert!(grammar
            .terminals()
            .map(|t| (grammar.terminal_index(t), t))
            .all(|(i, t)| grammar.terminal(i) == t));
        assert!(grammar
            .rule_indices()
            .map(|i| grammar.rule(i))
            .zip(grammar.rules())
            .all(|(r1, r2)| r1 == r2));
        assert!(grammar
            .nonterminal_indices()
            .map(|i| grammar.nonterminal(i))
            .zip(grammar.nonterminals())
            .all(|(n1, n2)| n1 == n2));
        assert!(grammar
            .terminal_indices()
            .map(|i| grammar.terminal(i))
            .zip(grammar.terminals())
            .all(|(t1, t2)| t1 == t2));
    }

    #[test]
    fn display_grammar() {
        let grammar = example_grammar();
        assert_eq!(
            format!("\n{}\n", grammar),
            r#"
grammar {
    start E
    rule 0: E -> T E1
    rule 1: E1 -> "+" T E1
    rule 2: E1 -> ε
    rule 3: T -> F T1
    rule 4: T1 -> "*" F T1
    rule 5: T1 -> ε
    rule 6: F -> "(" E ")"
    rule 7: F -> "id"
}
"#
        );
    }

    #[test]
    fn display_rule() {
        let grammar = example_grammar();
        assert_eq!(
            grammar.rules().map(ToString::to_string).collect::<Vec<_>>(),
            &[
                "N0 -> N1 N2",
                "N2 -> t0 N1 N2",
                "N2 -> ε",
                "N1 -> N3 N4",
                "N4 -> t1 N3 N4",
                "N4 -> ε",
                "N3 -> t2 N0 t3",
                "N3 -> t4",
            ]
        );
    }
}
