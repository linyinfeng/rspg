use rspg::display::DisplayWith;
use rspg::grammar;
use rspg::lr1::generator::Generator;
use rspg::lr1::parser::Parser;
use rspg::set::FirstSets;
use rspg::set::FollowSets;
use rspg::token::TerminalToken;
use std::marker::PhantomData;

fn main() {
    let grammar = grammar! {
        start S;
        rule S -> A, 'b';
        rule A -> A, 'a';
        rule A -> 'a';
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

    let input = "aab";
    println!("input:\n{:?}\n", input);

    let parser = Parser::<_, _, _, _, _, ()> {
        grammar: &grammar,
        table: &table,
        reducer: |r| {
            println!("use rule {} reduce: {}", r.rule, r.display_with(&grammar));
            Ok(())
        },
        phantom: PhantomData,
    };
    println!("events:");
    match parser.parse(input.chars().map(TerminalToken)) {
        Ok(p) => println!("accepted: {:?}", p),
        Err(e) => println!("error: {:?}", e),
    }
}
