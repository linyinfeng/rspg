# Simple Parser Generator

[![Build Status](https://travis-ci.org/linyinfeng/simple-parser-generator.svg?branch=master)](https://travis-ci.org/linyinfeng/simple-parser-generator)

A simple rust parser generator library for syntactic analysis learning.

## Status

- [x] Grammar
- [x] FIRST Set
- [x] FOLLOW Set
- [ ] LL(1) Parser
- [ ] LL(1) Table Generator
- [ ] SLR(1) Parser
- [ ] SLR(1) Table Generator
- [x] LR(1) Parser
- [x] LR(1) Table Generator
- [ ] ...

## Usage

Check the main.rs file.

```rust
// define terminal type
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
enum Terminal {
    Sign(char),
    Number,
}

// define token type
#[derive(Debug, Copy, Clone)]
enum Token {
    Sign(char),
    Number(f64),
}

// implement token::Token<Terminal> for token type defined
impl token::Token<Terminal> for Token {
    fn terminal(&self) -> Terminal {
        match self {
            Token::Sign(c) => Terminal::Sign(*c),
            Token::Number(_) => Terminal::Number,
        }
    }
}

// here comes the main function
fn main() {
    // define a Grammar<'static str, _> with macro grammar!
    let grammar = grammar! {
        start E;
        rule E -> E, Terminal::Sign('+'), T;
        rule E -> E, Terminal::Sign('-'), T;
        rule E -> T;
        rule T -> T, Terminal::Sign('*'), F;
        rule T -> T, Terminal::Sign('/'), F;
        rule T -> F;
        rule F -> Terminal::Sign('('), E, Terminal::Sign(')');
        rule F -> Terminal::Number;
    };
    println!("{}\n", grammar);

    // get the FIRST sets of every non-terminals for the grammar
    let first_sets = FirstSets::of_grammar(&grammar);
    println!("first sets:\n{}\n", first_sets.display_with(&grammar));

    // get the FOLLOR sets of every non-terminals for the grammar
    let follow_sets = FollowSets::of_grammar(&grammar, &first_sets);
    println!("follow sets:\n{}\n", follow_sets.display_with(&grammar));

    // define a new start non-terminal for extending the grammar
    let new_start = "E'";
    println!("new start for extending:\n{}\n", new_start);
    println!("LR(1) canonical collection:");
    // construct the parser generator, which create the LR(1) canonical collection of the grammar
    let generator = Generator::construct(&grammar, &first_sets, new_start);
    for (i, item_set) in generator.canonical_collection().iter().enumerate() {
        println!(
            "I_{} = {}",
            i,
            item_set.display_with(generator.extended_grammar())
        );
    }
    println!();

    // generate the LR(1) table
    let table = generator.generate(&grammar, &new_start).unwrap();
    println!("LR(1) table:");
    // get a pretty_table for debug
    let pretty_table = table.pretty_table(&grammar, false);
    pretty_table.printstd();
    println!();

    // get input tokens
    let input = vec![
        Token::Number(20.),
        Token::Sign('/'),
        Token::Number(10.),
        Token::Sign('-'),
        Token::Number(2.),
        Token::Sign('*'),
        Token::Sign('('),
        Token::Number(3.),
        Token::Sign('+'),
        Token::Number(6.),
        Token::Sign(')'),
    ];
    println!("input:\n{:?}\n", input);

    // crate a parser, call next_event to get next parse event
    println!("events:");
    let mut parser = Parser::new(&grammar, &table, input.iter());
    loop {
        match parser.next_event() {
            Event::Reduce(r) => {
                println!("use rule {} reduce: {}", r.rule, r.display_with(&grammar))
            },
            Event::Accept => {
                println!("accepted");
                break
            },
            Event::Error(e) => {
                println!("error: {:?}", e);
                break
            },
        }
    }
}
```
