use simple_parser_generator::grammar;
use simple_parser_generator::set::FirstSets;
use simple_parser_generator::set::FollowSets;
use simple_parser_generator::display::DisplayWith;
use simple_parser_generator::lr1::generator::Generator;
use simple_parser_generator::lr1::Parser;
use simple_parser_generator::lr1::parser::Event;
use simple_parser_generator::token::TerminalToken;

fn main() {
    let grammar = grammar!{
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

    println!("events:");
    let mut parser = Parser::new(&grammar, &table, input.chars().map(TerminalToken::new));
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