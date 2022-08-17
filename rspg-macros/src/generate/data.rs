use proc_macro2::TokenStream;
use quote::ToTokens;
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
use token::Paren;

pub mod keyword {
    syn::custom_keyword!(start);
    syn::custom_keyword!(token);
    syn::custom_keyword!(terminal);
    syn::custom_keyword!(error);
    syn::custom_keyword!(nonterminal);
    syn::custom_keyword!(rule);
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

impl ToTokens for Rule {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.rule.to_tokens(tokens);
        self.left.to_tokens(tokens);
        self.arrow.to_tokens(tokens);
        for symbol in &self.right {
            symbol.to_tokens(tokens);
        }
        self.double_arrow.to_tokens(tokens);
        self.body.to_tokens(tokens);
        self.semicolon.to_tokens(tokens);
    }
}

type SymbolString = Vec<PatSymbol>;

#[derive(Debug, Clone)]
pub struct PatSymbol {
    pub pat: Option<(Paren, Pat, Token![:])>,
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
            #[allow(clippy::mixed_read_write_in_expression)]
            Ok(PatSymbol {
                pat: Some((
                    parenthesized!(content in input),
                    content.parse()?,
                    content.parse()?,
                )),
                symbol: content.parse()?,
            })
        }
    }
}

impl ToTokens for PatSymbol {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match &self.pat {
            Some((paren, pat, comma)) => paren.surround(tokens, |t| {
                pat.to_tokens(t);
                comma.to_tokens(t);
                self.symbol.to_tokens(t);
            }),
            None => self.symbol.to_tokens(tokens),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Nonterminal(Ident),
    Terminal(LitStr),
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

impl ToTokens for Symbol {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Nonterminal(i) => i.to_tokens(tokens),
            Self::Terminal(l) => l.to_tokens(tokens),
        }
    }
}

#[derive(Debug, Clone)]
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
            start: start.ok_or_else(|| input.error("start clause required"))?,
            token: token.ok_or_else(|| input.error("token clause required"))?,
            terminals,
            error: error.ok_or_else(|| input.error("error clause required"))?,
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

#[derive(Debug, Clone)]
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

impl ToTokens for TokenDescription {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.token.to_tokens(tokens);
        self.ty.to_tokens(tokens);
        self.semicolon.to_tokens(tokens);
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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
        #[allow(clippy::mixed_read_write_in_expression)]
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
