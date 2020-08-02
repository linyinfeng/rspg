# rspg

![CI](https://github.com/linyinfeng/rspg/workflows/CI/badge.svg)
![crates.io](https://img.shields.io/crates/v/rspg.svg)
![docs.rs](https://docs.rs/rspg/badge.svg)

A simple **experimental** LR(1) rust parser generator library for syntactic analysis learning.

## Status

- [x] Grammar
- [x] FIRST Set
- [x] FOLLOW Set
- [x] LR(1) Parser
- [x] LR(1) Table Generator
- [ ] Tests
- [ ] Documentation
- [x] Basic ability to use LR(1) parse result
- [x] rspg-macros
- [ ] ...

## Usage

Full example: [rspg-examples/src/lambda-macros/main.rs](https://github.com/linyinfeng/rspg/blob/master/rspg-examples/src/lambda-macros/main.rs).

```rust
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
        rule Abstraction -> "lambda" (x: Variable) "." (t: Abstraction) =>
            Ok(Term::Abstraction(x, Box::new(t)));
        rule Abstraction -> (t: Application) => Ok(t);

        nonterminal Application: Term;
        rule Application -> (t1: Application) (t2: Primary) =>
            Ok(Term::Application(Box::new(t1), Box::new(t2)));
        rule Application -> (t: Primary) => Ok(t);

        nonterminal Primary: Term;
        rule Primary -> "(" (t: Term) ")" => Ok(t);
        rule Primary -> (x: Variable) => Ok(Term::Variable(x));

        nonterminal Variable: Variable;
        rule Variable -> (x: "x") => Ok(Variable(x));
    }
}

fn main() {
    ...

    // Tokenize.
    let tokens = ...;

    // Parse.
    match lambda::parse(tokens) {
        Ok(t) => {
            // `t` has type `Term`
            ...
        },
        Err(e) => {
            error!("parse error: {:?}", e);
            ...
        },
    }

    ...
}
```
