use rspg::display::DisplayWith;
use rspg::grammar;
use rspg::lr1::generator::Generator;
use rspg::lr1::parser::Parser;
use rspg::set::FirstSets;
use rspg::set::FollowSets;
use rspg::token;
use std::marker::PhantomData;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
enum Terminal {
    Sign(char),
    Number,
}

#[derive(Debug, Copy, Clone)]
enum Token {
    Sign(char),
    Number(f64),
}

impl token::Token<Terminal> for Token {
    fn terminal(&self) -> Terminal {
        match self {
            Token::Sign(c) => Terminal::Sign(*c),
            Token::Number(_) => Terminal::Number,
        }
    }
}

fn main() {
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

    let first_sets = FirstSets::of_grammar(&grammar);
    println!("first sets:\n{}\n", first_sets.display_with(&grammar));

    let follow_sets = FollowSets::of_grammar(&grammar, &first_sets);
    println!("follow sets:\n{}\n", follow_sets.display_with(&grammar));

    println!("LR(1) canonical collection:");
    let generator = Generator::construct(&grammar, &first_sets, "E'");
    for (i, item_set) in generator.canonical_collection().iter().enumerate() {
        println!(
            "I_{} = {}",
            i,
            item_set.display_with(generator.extended_grammar())
        );
    }
    println!();

    let table = generator.generate(&grammar).unwrap();
    println!("LR(1) table:");
    let pretty_table = table.pretty_table(&grammar, false);
    pretty_table.printstd();
    println!();

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

    print!("input pretty printed:");
    for token in &input {
        print!(" ");
        match token {
            Token::Number(n) => print!("{}", n),
            Token::Sign(s) => print!("{}", s),
        }
    }
    println!("\n");

    let parser = {
        Parser {
            grammar: &grammar,
            table: &table,
            reducer: |mut r| {
                let mut rule_indices = grammar.rule_indices();
                let rule_plus = rule_indices.next().unwrap();
                let rule_sub = rule_indices.next().unwrap();
                let rule_et = rule_indices.next().unwrap();
                let rule_mul = rule_indices.next().unwrap();
                let rule_div = rule_indices.next().unwrap();
                let rule_tf = rule_indices.next().unwrap();
                let rule_bracket = rule_indices.next().unwrap();
                let rule_number = rule_indices.next().unwrap();
                println!("use rule {} reduce: {}", r.rule, r.display_with(&grammar));
                if r.rule == rule_plus {
                    let e = r.from.pop_front().unwrap();
                    let _ = r.from.pop_front().unwrap();
                    let t = r.from.pop_front().unwrap();
                    Ok(e.parsed().unwrap() + t.parsed().unwrap())
                } else if r.rule == rule_sub {
                    let e = r.from.pop_front().unwrap();
                    let _ = r.from.pop_front().unwrap();
                    let t = r.from.pop_front().unwrap();
                    Ok(e.parsed().unwrap() - t.parsed().unwrap())
                } else if r.rule == rule_et {
                    let t = r.from.pop_front().unwrap();
                    Ok(t.parsed().unwrap())
                } else if r.rule == rule_mul {
                    let t = r.from.pop_front().unwrap();
                    let _ = r.from.pop_front().unwrap();
                    let f = r.from.pop_front().unwrap();
                    Ok(t.parsed().unwrap() * f.parsed().unwrap())
                } else if r.rule == rule_div {
                    let t = r.from.pop_front().unwrap();
                    let _ = r.from.pop_front().unwrap();
                    let f = r.from.pop_front().unwrap();
                    Ok(t.parsed().unwrap() / f.parsed().unwrap())
                } else if r.rule == rule_tf {
                    let f = r.from.pop_front().unwrap();
                    Ok(f.parsed().unwrap())
                } else if r.rule == rule_bracket {
                    let _ = r.from.pop_front().unwrap();
                    let n = r.from.pop_front().unwrap();
                    let _ = r.from.pop_front().unwrap();
                    Ok(n.parsed().unwrap())
                } else if r.rule == rule_number {
                    let n = r.from.pop_front().unwrap();
                    match n.token().unwrap() {
                        Token::Number(n) => Ok(n),
                        _ => Err("not a number token"),
                    }
                } else {
                    unimplemented!()
                }
            },
            phantom: PhantomData,
        }
    };
    println!("events:");
    match parser.parse(input.into_iter()) {
        Ok(p) => println!("accepted: {:?}", p),
        Err(e) => println!("error: {:?}", e),
    }
}
