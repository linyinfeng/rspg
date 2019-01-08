use simple_parser_generator::display::DisplayWith;
use simple_parser_generator::grammar;
use simple_parser_generator::lr1::generator::Generator;
use simple_parser_generator::lr1::parser::Event;
use simple_parser_generator::lr1::Parser;
use simple_parser_generator::set::FirstSets;
use simple_parser_generator::set::FollowSets;
use simple_parser_generator::token;

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

    let new_start = "E'";
    println!("new start for extending:\n{}\n", new_start);
    println!("LR(1) canonical collection:");
    let generator = Generator::construct(&grammar, &first_sets, new_start);
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
