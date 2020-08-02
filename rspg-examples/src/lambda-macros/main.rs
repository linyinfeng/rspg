use log::debug;
use log::error;
use log::info;
use log::trace;
use rspg::display::DisplayWith;
use rspg_macros::rspg;
use rustyline::error::ReadlineError;
use std::fmt;
use std::iter::Peekable;

// Define token type.
#[derive(Debug, Clone)]
pub enum Token {
    Lambda,
    Variable(String),
    Point,
    LeftBracket,
    RightBracket,
}

// Define AST.
#[derive(Debug, Clone)]
pub struct Variable(pub String);

#[derive(Debug, Clone)]
pub enum Term {
    Variable(Variable),
    Abstraction(Variable, Box<Term>),
    Application(Box<Term>, Box<Term>),
}

// Define the parser.
rspg! {
    pub mod lambda {
        // Specify the token type.
        token Token;

        // Define patterns and values for terminal symbols.
        terminal "lambda" Token::Lambda => ();
        terminal "." Token::Point => ();
        terminal "(" Token::LeftBracket => ();
        terminal ")" Token::RightBracket => ();
        terminal "x" Token::Variable(x) => x;

        // Specify the error type.
        error ();

        // Specify the start nonterminal symbol.
        start Term;

        // Define type of nonterminal symbols.
        nonterminal Term: Term;
        // Define rules.
        // Use the syntax `(pattern: Symbol)` to capture the "value" of the symbol.
        // The type of the result expression should be `Result<NonterminalType, ErrorType>`.
        // In this case the expression `Ok(t)` should have type `Result<Term, ()>`.
        rule Term -> (t: Abstraction) => Ok(t);

        nonterminal Abstraction: Term;
        // Value of a bare `Symbol` will be ignored.
        rule Abstraction -> "lambda" (x: Variable) "." (t: Abstraction) => Ok(Term::Abstraction(x, Box::new(t)));
        rule Abstraction -> (t: Application) => Ok(t);

        nonterminal Application: Term;
        rule Application -> (t1: Application) (t2: Primary) => Ok(Term::Application(Box::new(t1), Box::new(t2)));
        rule Application -> (t: Primary) => Ok(t);

        nonterminal Primary: Term;
        rule Primary -> "(" (t: Term) ")" => Ok(t);
        rule Primary -> (x: Variable) => Ok(Term::Variable(x));

        nonterminal Variable: Variable;
        rule Variable -> (x: "x") => Ok(Variable(x));
    }
}

fn main() {
    env_logger::init();

    // rspg! { ... } defines some static variables, type `Parsed`, function `parse` and `reduce` in the mod `lambda`.
    // Use cargo doc or expand the macro to see the documents.
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
                // Tokenize.
                let tokens = tokens(line.chars()).filter_map(|r| match r {
                    Ok(t) => {
                        trace!("parsed token: {:?}", t);
                        Some(t)
                    }
                    Err(e) => {
                        error!("tokenize error: {}", e);
                        None
                    }
                });

                // Parse.
                match lambda::parse(tokens) {
                    Ok(t) => {
                        // `p` has type `Term`
                        debug!("result: {:#?}", t);
                        println!("{}", t);
                    }
                    Err(e) => error!("parse error: {:?}", e),
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
            Err(err) => {
                error!("readline error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("rspg-lambda-history.txt").unwrap();
}

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

pub fn tokens<I>(input: I) -> Tokens<I>
where
    I: Iterator<Item = char>,
{
    Tokens {
        input: input.peekable(),
    }
}

pub struct Tokens<I>
where
    I: Iterator,
{
    input: Peekable<I>,
}

impl<I> Tokens<I>
where
    I: Iterator<Item = char>,
{
    fn match_variable(&mut self) -> String {
        let mut name = String::new();
        while let Some(c) = self.input.peek() {
            let c = *c;
            match c {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                    self.input.next();
                    name.push(c);
                }
                _ => break,
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
                            return Some(Ok(Token::Lambda));
                        }
                        '.' => {
                            self.input.next();
                            return Some(Ok(Token::Point));
                        }
                        '(' => {
                            self.input.next();
                            return Some(Ok(Token::LeftBracket));
                        }
                        ')' => {
                            self.input.next();
                            return Some(Ok(Token::RightBracket));
                        }
                        'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                            let name = self.match_variable();
                            return Some(Ok(Token::Variable(name)));
                        }
                        ' ' | '\t' | '\n' => {
                            self.input.next();
                        }
                        other => {
                            self.input.next();
                            return Some(Err(TokensError::InvalidChar(other)));
                        }
                    }
                }
                None => return None,
            }
        }
    }
}
