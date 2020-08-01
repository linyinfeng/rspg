use log::{debug, error, info, trace};
use rspg::display::DisplayWith;
use rustyline::error::ReadlineError;
use std::env;

mod lambda {
    use lazy_static::lazy_static;
    use log::trace;
    use rspg::display::DisplayWith;
    use rspg::grammar;
    use rspg::grammar::Grammar;
    use rspg::grammar::RuleIndex;
    use rspg::lr1::generator::Generator;
    use rspg::lr1::parser::Parser;
    use rspg::lr1::parser::Reduce;
    use rspg::lr1::table::Table;
    use rspg::set::FirstSets;
    use rspg::set::FollowSets;
    use rspg::token;
    use std::fmt;
    use std::iter::Peekable;
    use std::marker::PhantomData;

    #[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
    pub enum Terminal {
        Lambda,
        Variable,
        Point,
        LeftBracket,
        RightBracket,
    }

    #[derive(Debug, Clone)]
    pub enum Token {
        Lambda,
        Variable(String),
        Point,
        LeftBracket,
        RightBracket,
    }

    impl token::Token<Terminal> for Token {
        fn terminal(&self) -> Terminal {
            match self {
                Token::Lambda => Terminal::Lambda,
                Token::Variable(_) => Terminal::Variable,
                Token::Point => Terminal::Point,
                Token::LeftBracket => Terminal::LeftBracket,
                Token::RightBracket => Terminal::RightBracket,
            }
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub enum TokensError {
        InvalidChar(char),
    }

    impl fmt::Display for TokensError {
        fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
            match self {
                TokensError::InvalidChar(c) => write!(f, "ignored invalid char '{}'", c),
            }
        }
    }

    pub struct Tokens<I>
    where
        I: Iterator,
    {
        input: Peekable<I>,
    }

    pub fn tokens<I>(input: I) -> Tokens<I>
    where
        I: Iterator<Item = char>,
    {
        Tokens {
            input: input.peekable(),
        }
    }

    impl<I> Tokens<I>
    where
        I: Iterator<Item = char>,
    {
        fn match_variable(&mut self) -> String {
            let mut name = String::new();
            loop {
                match self.input.peek() {
                    Some(c) => {
                        let c = *c;
                        match c {
                            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                                self.input.next();
                                name.push(c);
                            },
                            _ => break,
                        }
                    },
                    None => break,
                }
            }
            if name.is_empty() {
                panic!()
            } else {
                name
            }
        }
    }

    impl<I> Iterator for Tokens<I>
    where
        I: Iterator<Item = char>,
    {
        type Item = Result<Token, TokensError>;

        fn next(&mut self) -> Option<Self::Item> {
            loop {
                match self.input.peek() {
                    Some(c) => {
                        let c = *c;
                        match c {
                            '\\' | 'λ' => {
                                self.input.next();
                                return Some(Ok(Token::Lambda))
                            },
                            '.' => {
                                self.input.next();
                                return Some(Ok(Token::Point))
                            },
                            '(' => {
                                self.input.next();
                                return Some(Ok(Token::LeftBracket))
                            },
                            ')' => {
                                self.input.next();
                                return Some(Ok(Token::RightBracket))
                            },
                            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                                let name = self.match_variable();
                                return Some(Ok(Token::Variable(name)))
                            },
                            ' ' | '\t' | '\n' => {
                                self.input.next();
                            },
                            other => {
                                self.input.next();
                                return Some(Err(TokensError::InvalidChar(other)))
                            },
                        }
                    },
                    None => return None,
                }
            }
        }
    }

    lazy_static! {
        pub static ref GRAMMAR: Grammar<&'static str, Terminal> = grammar! {
            start Term;
            rule Term -> Abstraction;
            rule Abstraction -> Terminal::Lambda, Variable, Terminal::Point, Abstraction;
            rule Abstraction -> Application;
            rule Application -> Application, Primary;
            rule Application -> Primary;
            rule Primary -> Terminal::LeftBracket, Term, Terminal::RightBracket;
            rule Primary -> Variable;
            rule Variable -> Terminal::Variable;
        };
        pub static ref RULES: Vec<RuleIndex> = GRAMMAR.rule_indices().collect();
        pub static ref FIRST_SETS: FirstSets = FirstSets::of_grammar(&GRAMMAR);
        pub static ref FOLLOW_SETS: FollowSets = FollowSets::of_grammar(&GRAMMAR, &FIRST_SETS);
        pub static ref TABLE: Table = Generator::construct(&GRAMMAR, &FIRST_SETS, "Term'")
            .generate(&GRAMMAR)
            .unwrap();
        pub static ref PARSER: Parser<
            'static,
            'static,
            &'static str,
            Terminal,
            Token,
            Term,
            fn(Reduce<Term, Token>) -> Result<Term, ()>,
            (),
        > = Parser {
            grammar: &GRAMMAR,
            table: &TABLE,
            reducer,
            phantom: PhantomData,
        };
    }

    #[derive(Debug, Clone)]
    pub enum Term {
        Variable(Variable),
        Abstraction(Variable, Box<Term>),
        Application(Box<Term>, Box<Term>),
    }

    #[derive(Debug, Clone)]
    pub struct Variable(pub String);

    impl fmt::Display for Term {
        fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
            match self {
                Term::Variable(v) => write!(f, "{}", v),
                Term::Abstraction(v, t) => write!(f, "(λ{}. {})", v, t),
                Term::Application(t1, t2) => write!(f, "({} {})", t1, t2),
            }
        }
    }

    impl fmt::Display for Variable {
        fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
            write!(f, "{}", self.0)
        }
    }

    macro_rules! destring {
        (let [$($p:pat,)*] = $e:expr) => (
            $(
                let $p = $e.pop_front().expect("no enough item in item string");
            )*
            assert_eq!($e.is_empty(), true, "item string is longer than pattern");
        );
        (let [$($p:pat),*] = $e:expr) => (
            destring!(let [$($p,)*] = $e);
        );
    }

    fn reducer(mut r: Reduce<Term, Token>) -> Result<Term, ()> {
        trace!("use rule {} reduce: {}", r.rule, r.display_with(&GRAMMAR));

        if r.rule == RULES[0] {
            // Term -> Abstraction;
            destring!(let [a] = r.from);
            Ok(a.parsed().unwrap())
        } else if r.rule == RULES[1] {
            // Abstraction -> Terminal::Lambda, Variable, Terminal::Point, Abstraction
            destring!(let [_, v, _, a] = r.from);
            match v.parsed().unwrap() {
                Term::Variable(v) => Ok(Term::Abstraction(v, Box::new(a.parsed().unwrap()))),
                _ => panic!(),
            }
        } else if r.rule == RULES[2] {
            // Abstraction -> Application
            destring!(let [a] = r.from);
            Ok(a.parsed().unwrap())
        } else if r.rule == RULES[3] {
            // Application -> Application, Primary
            destring!(let [a, p] = r.from);
            Ok(Term::Application(
                Box::new(a.parsed().unwrap()),
                Box::new(p.parsed().unwrap()),
            ))
        } else if r.rule == RULES[4] {
            // Application -> Primary
            destring!(let [p] = r.from);
            Ok(p.parsed().unwrap())
        } else if r.rule == RULES[5] {
            // Primary -> Terminal::LeftBracket, Term, Terminal::RightBracket
            destring!(let [_, t, _] = r.from);
            Ok(t.parsed().unwrap())
        } else if r.rule == RULES[6] {
            // Primary -> Variable
            destring!(let [v] = r.from);
            Ok(v.parsed().unwrap())
        } else if r.rule == RULES[7] {
            // Variable -> Terminal::Variable
            destring!(let [v] = r.from);
            match v.token().unwrap() {
                Token::Variable(name) => Ok(Term::Variable(Variable(name))),
                _ => panic!(),
            }
        } else {
            unimplemented!()
        }
    }
}

fn main() {
    let mut builder = env_logger::builder();
    builder.filter_module("lambda", log::LevelFilter::Trace);
    if env::var("RUST_LOG").is_ok() {
        builder.parse_filters(&env::var("RUST_LOG").unwrap());
    }
    builder.init();

    info!("{}", *lambda::GRAMMAR);
    info!(
        "first sets:\n{}",
        lambda::FIRST_SETS.display_with(&lambda::GRAMMAR)
    );
    info!(
        "follow sets:\n{}",
        lambda::FOLLOW_SETS.display_with(&lambda::GRAMMAR)
    );
    info!(
        "LR(1) table:\n{}",
        lambda::TABLE.pretty_table(&lambda::GRAMMAR, false)
    );

    let mut rl = rustyline::Editor::<()>::new();
    if rl.load_history("rspg-lambda-history.txt").is_err() {
        info!("no previous history.");
    }
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let tokens = lambda::tokens(line.chars()).filter_map(|r| match r {
                    Ok(t) => {
                        trace!("parsed token: {:?}", t);
                        Some(t)
                    },
                    Err(e) => {
                        error!("tokenize error: {}", e);
                        None
                    },
                });
                match lambda::PARSER.parse(tokens) {
                    Ok(p) => {
                        debug!("result: {:#?}", p);
                        println!("{}", p);
                    },
                    Err(e) => error!("parse error: {:?}", e),
                }
            },
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
            Err(err) => {
                error!("readline error: {:?}", err);
                break
            },
        }
    }
    rl.save_history("rspg-lambda-history.txt").unwrap();
}
