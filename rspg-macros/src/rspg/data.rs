use proc_macro2::Span;
use syn::braced;
use syn::parenthesized;
use syn::parse;
use syn::parse::Parse;
use syn::parse::ParseStream;
use syn::token;
use syn::Attribute;
use syn::Expr;
use syn::Ident;
use syn::LitStr;
use syn::Pat;
use syn::Token;
use syn::Type;
use syn::Visibility;

pub mod keyword {
    syn::custom_keyword!(start);
    syn::custom_keyword!(token);
    syn::custom_keyword!(terminal);
    syn::custom_keyword!(error);
    syn::custom_keyword!(nonterminal);
    syn::custom_keyword!(rule);
}

#[derive(Debug)]
pub struct Start {
    pub start: keyword::start,
    pub nonterminal: Ident,
    pub semicolon: Token![;],
}

impl Parse for Start {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        Ok(Start {
            start: input.parse()?,
            nonterminal: input.parse()?,
            semicolon: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct Rule {
    pub rule: keyword::rule,
    pub left: Ident,
    pub arrow: Token![->],
    pub right: SymbolString,
    pub double_arrow: Token![=>],
    pub body: Expr,
    pub semicolon: Token![;],
}

impl Parse for Rule {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        Ok(Rule {
            rule: input.parse()?,
            left: input.parse()?,
            arrow: input.parse()?,
            right: {
                let mut string = Vec::new();
                while !input.lookahead1().peek(Token![=>]) {
                    string.push(input.parse()?);
                }
                string
            },
            double_arrow: input.parse()?,
            body: input.parse()?,
            semicolon: input.parse()?,
        })
    }
}

type SymbolString = Vec<PatSymbol>;

#[derive(Debug)]
pub struct PatSymbol {
    pub pat: Option<(Pat, Token![:])>,
    pub symbol: Symbol,
}

impl Parse for PatSymbol {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Ident) || lookahead.peek(LitStr) {
            Ok(PatSymbol {
                pat: None,
                symbol: input.parse()?,
            })
        } else {
            let content;
            parenthesized!(content in input);
            Ok(PatSymbol {
                pat: Some((content.parse()?, content.parse()?)),
                symbol: content.parse()?,
            })
        }
    }
}

#[derive(Debug)]
pub enum Symbol {
    Nonterminal(Ident),
    Terminal(LitStr),
}

impl Symbol {
    pub fn span(&self) -> Span {
        match self {
            Self::Nonterminal(i) => i.span(),
            Self::Terminal(l) => l.span(),
        }
    }
}

impl Parse for Symbol {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Ident) {
            Ok(Symbol::Nonterminal(input.parse()?))
        } else {
            Ok(Symbol::Terminal(input.parse()?))
        }
    }
}

#[derive(Debug)]
pub struct RspgContent {
    pub start: Start,
    pub token: TokenDescription,
    pub terminals: Vec<TerminalDescription>,
    pub error: ErrorDescription,
    pub nonterminals: Vec<NonterminalDescription>,
    pub rules: Vec<Rule>,
}

impl Parse for RspgContent {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        let mut start = None;
        let mut token = None;
        let mut error = None;
        let mut terminals = Vec::new();
        let mut nonterminals = Vec::new();
        let mut rules = Vec::new();
        while !input.is_empty() {
            let lookahead = input.lookahead1();
            if lookahead.peek(keyword::start) {
                set_option(input, &mut start)?;
            } else if lookahead.peek(keyword::token) {
                set_option(input, &mut token)?;
            } else if lookahead.peek(keyword::error) {
                set_option(input, &mut error)?;
            } else if lookahead.peek(keyword::nonterminal) {
                nonterminals.push(input.parse()?);
            } else if lookahead.peek(keyword::terminal) {
                terminals.push(input.parse()?)
            } else {
                rules.push(input.parse()?);
            }
        }
        Ok(RspgContent {
            start: start.expect("start clause required"),
            token: token.expect("token clause required"),
            terminals,
            error: error.expect("error clause required"),
            nonterminals,
            rules,
        })
    }
}

fn set_option<T>(input: ParseStream, option: &mut Option<T>) -> parse::Result<()>
where
    T: Parse,
{
    if option.is_none() {
        *option = Some(input.parse()?);
        Ok(())
    } else {
        Err(syn::Error::new(input.span(), "clause already defined"))
    }
}

#[derive(Debug)]
pub struct TokenDescription {
    pub token: keyword::token,
    pub ty: Type,
    pub semicolon: Token![;],
}

impl Parse for TokenDescription {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        Ok(TokenDescription {
            token: input.parse()?,
            ty: input.parse()?,
            semicolon: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct ErrorDescription {
    pub error: keyword::error,
    pub ty: Type,
    pub semicolon: Token![;],
}

impl Parse for ErrorDescription {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        Ok(ErrorDescription {
            error: input.parse()?,
            ty: input.parse()?,
            semicolon: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct NonterminalDescription {
    pub nonterminal: keyword::nonterminal,
    pub ident: Ident,
    pub comma: Token![:],
    pub ty: Type,
    pub semicolon: Token![;],
}

impl Parse for NonterminalDescription {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        Ok(NonterminalDescription {
            nonterminal: input.parse()?,
            ident: input.parse()?,
            comma: input.parse()?,
            ty: input.parse()?,
            semicolon: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct TerminalDescription {
    pub terminal: keyword::terminal,
    pub lit: LitStr,
    pub pat: Pat,
    pub double_arrow: Token![=>],
    pub expr: Expr,
    pub semicolon: Token![;],
}

impl Parse for TerminalDescription {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        Ok(TerminalDescription {
            terminal: input.parse()?,
            lit: input.parse()?,
            pat: input.parse()?,
            double_arrow: input.parse()?,
            expr: input.parse()?,
            semicolon: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct RspgMod {
    pub outer_attrs: Vec<Attribute>,
    pub visibility: Visibility,
    pub mod_token: Token![mod],
    pub mod_name: Ident,
    pub brace: token::Brace,
    pub inner_attrs: Vec<Attribute>,
    pub content: RspgContent,
}

impl Parse for RspgMod {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        let content;
        #[allow(clippy::eval_order_dependence)]
        Ok(RspgMod {
            outer_attrs: input.call(Attribute::parse_outer)?,
            visibility: input.parse()?,
            mod_token: input.parse()?,
            mod_name: input.parse()?,
            brace: braced!(content in input),
            inner_attrs: input.call(Attribute::parse_inner)?,
            content: content.parse()?,
        })
    }
}
