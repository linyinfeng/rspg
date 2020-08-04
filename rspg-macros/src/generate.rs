mod data;

use data::NonterminalDescription;
use data::RspgContent;
use data::RspgMod;
use data::TerminalDescription;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote;
use quote::quote_spanned;
use rspg::grammar::Grammar;
use std::cell::RefCell;
use std::collections::BTreeMap;
use syn::spanned::Spanned;
use syn::Ident;
use syn::LitStr;

macro_rules! use_names {
    () => ();
    ($name:ident, $($tail:tt)*) => (
        use_name!($name $name);
        use_names!($($tail)*)
    );
    ($span:expr => $name:ident, $($tail:tt)*) => (
        use_name!($span => $name $name);
        use_names!($($tail)*)
    );
}

// See <https://github.com/SergioBenitez/Rocket/blob/45b4436ed3a7ab913d96c2b69ee4df7fd8c0c618/core/codegen/src/lib.rs#L61>
macro_rules! define_names {
    ($($name:ident => $path:path,)*) => (
        macro_rules! use_name {
            $(
                ($i:ident $name) => (
                    #[allow(non_snake_case)] let $i = quote!{$path};
                );
            )*
            $(
                ($span:expr => $i:ident $name) => (
                    #[allow(non_snake_case)] let $i = quote_spanned!{$span => $path};
                );
            )*
        }
    );
}

define_names! {
    _ron_from_str => ::ron::de::from_str,
    _lazy_static => ::lazy_static::lazy_static,
    _Grammar => ::rspg::grammar::Grammar,
    _TerminalIndex => ::rspg::grammar::TerminalIndex,
    _RuleIndex => ::rspg::grammar::RuleIndex,
    _FirstSets => ::rspg::set::FirstSets,
    _FollowSets => ::rspg::set::FollowSets,
    _Table => ::rspg::lr1::table::Table,
    _Token => ::rspg::token::Token,
    _Reduce => ::rspg::lr1::parser::Reduce,
    _Parser => ::rspg::lr1::parser::Parser,
    _ParserError => ::rspg::lr1::parser::Error,
    _String => ::std::string::String,
    _Vec => ::std::vec::Vec,
    _Result => ::std::result::Result,
    _Iterator => ::std::iter::Iterator,
    _Ord => ::std::cmp::Ord,
    _PhantomData => ::std::marker::PhantomData,
}

pub fn generate(input: TokenStream) -> syn::Result<TokenStream> {
    let parsed: RspgMod = syn::parse2(input)?;
    // TODO: debug output
    let mod_vis = parsed.visibility;
    let mod_name = parsed.mod_name;
    let mod_outer_attrs = parsed.outer_attrs;
    let mod_inner_attrs = parsed.inner_attrs;
    let contents = build_contents(parsed.content)?;
    let result = quote! {
        #(#mod_outer_attrs)*
        #mod_vis mod #mod_name {
            #(#mod_inner_attrs)*
            use super::*;
            #contents
        }
    };
    Ok(result)
}

pub struct Context {
    content: RspgContent,
    grammar: Grammar<String, String>,
    nonterminal_map: BTreeMap<String, NonterminalDescription>,
    terminal_map: BTreeMap<String, TerminalDescription>,
}

pub fn build_contents(content: RspgContent) -> syn::Result<TokenStream> {
    let ctx = build_context(content)?;

    let rules: Vec<_> = ctx.grammar.rule_indices().collect();
    let first_sets = rspg::set::FirstSets::of_grammar(&ctx.grammar);
    let follow_sets = rspg::set::FollowSets::of_grammar(&ctx.grammar, &first_sets);
    let table =
        rspg::lr1::generator::Generator::construct(&ctx.grammar, &first_sets, "".to_string())
            .generate(&ctx.grammar)
            .ok_or_else(|| syn::Error::new(Span::call_site(), "failed to construct LR(1) table"))?;

    let mut result = proc_macro2::TokenStream::new();
    use_names! { _Grammar, _Vec, _String, _RuleIndex, _FirstSets, _FollowSets, _Table, }
    result.extend(embed_data(
        &ctx.grammar,
        "GRAMMAR",
        quote!(#_Grammar<#_String, #_String>),
    ));
    result.extend(embed_data(&rules, "RULES", quote!(#_Vec<#_RuleIndex>)));
    result.extend(embed_data(&first_sets, "FIRST_SETS", quote!(#_FirstSets)));
    result.extend(embed_data(
        &follow_sets,
        "FOLLOW_SETS",
        quote!(#_FollowSets),
    ));
    result.extend(embed_data(&table, "TABLE", quote!(#_Table)));

    result.extend(enum_parsed(&ctx));
    result.extend(token_impl(&ctx));
    result.extend(reducer(&ctx));
    result.extend(parser(&ctx));

    Ok(result)
}

pub fn build_context(content: RspgContent) -> syn::Result<Context> {
    let nonterminal_map: BTreeMap<_, _> = content
        .nonterminals
        .iter()
        .map(|n| (n.ident.to_string(), n.clone()))
        .collect();
    let terminal_map: BTreeMap<_, _> = content
        .terminals
        .iter()
        .map(|t| (t.lit.value(), t.clone()))
        .collect();

    let mut builder = rspg::grammar::GrammarBuilder::new();
    content.nonterminals.iter().for_each(|n| {
        builder.add_and_get_nonterminal(n.ident.to_string());
    });
    content.terminals.iter().for_each(|t| {
        builder.add_and_get_terminal(t.lit.value());
    });

    let errors = RefCell::new(Vec::new());
    let meet_nonterminal = |n: &Ident| {
        let s = n.to_string();
        if !nonterminal_map.contains_key(&s) {
            let message = format!("undefined nonterminal: {}", s);
            errors
                .borrow_mut()
                .push(syn::Error::new_spanned(n, message));
        }
    };
    let meet_terminal = |t: &LitStr| {
        let s = t.value();
        if !terminal_map.contains_key(&s) {
            let message = format!("undefined terminal: {:?}", s);
            errors
                .borrow_mut()
                .push(syn::Error::new_spanned(t, message));
        }
    };

    let start = &content.start.nonterminal;
    meet_nonterminal(start);
    builder = builder.start(start.to_string());
    for rule in &content.rules {
        meet_nonterminal(&rule.left);
        builder = builder.push_rule_left(rule.left.to_string());
        for right in &rule.right {
            builder = match &right.symbol {
                data::Symbol::Nonterminal(n) => {
                    meet_nonterminal(n);
                    builder.push_rule_right_nonterminal(n.to_string())
                }
                data::Symbol::Terminal(t) => {
                    meet_terminal(t);
                    builder.push_rule_right_terminal(t.value())
                }
            };
        }
    }
    let grammar = builder.build();

    match collect_errors(errors.into_inner().into_iter()) {
        None => Ok(Context {
            content,
            grammar,
            nonterminal_map,
            terminal_map,
        }),
        Some(e) => Err(e),
    }
}

pub fn embed_data<T>(data: &T, name: &str, ty: TokenStream) -> TokenStream
where
    T: serde::Serialize,
{
    let ident = Ident::new(name, Span::call_site());
    let string =
        ron::ser::to_string(data).unwrap_or_else(|_| panic!("failed to serialize data {}", name));
    use_names! { _lazy_static, _ron_from_str, }
    quote! {
        #_lazy_static! {
            pub static ref #ident: #ty = #_ron_from_str(#string).expect("failed to load embedded data");
        }
    }
}

pub fn enum_parsed(ctx: &Context) -> TokenStream {
    let Context {
        content: RspgContent { nonterminals, .. },
        ..
    } = ctx;

    let mut result = TokenStream::new();

    let idents = nonterminals.iter().map(|d| &d.ident);
    let types = nonterminals.iter().map(|d| &d.ty);

    let type_def = {
        let idents = idents.clone();
        let types = types.clone();
        quote! {
            pub enum Parsed {
                #(#idents(#types),)*
            }
        }
    };
    result.extend(type_def);

    let unwrap_idents = idents.clone().map(unwrap_ident);

    let type_impl = quote! {
        impl Parsed {
            #(
                pub fn #unwrap_idents(self) -> #types {
                    match self {
                        Self::#idents(x) => x,
                        _ => panic!("expect {}", stringify!(#idents)),
                    }
                }
            )*
        }
    };
    result.extend(type_impl);

    result
}

pub fn token_impl(ctx: &Context) -> TokenStream {
    let Context {
        grammar,
        content: RspgContent {
            token, terminals, ..
        },
        ..
    } = ctx;

    let ty = &token.ty;
    let lits = terminals.iter().map(|d| &d.lit);
    let pats = terminals.iter().map(|d| &d.pat);
    let indices = lits
        .clone()
        .map(|l| grammar.terminal_index(&l.value()).value());
    let _exprs = terminals.iter().map(|d| &d.expr);

    let pats2 = pats.clone();
    use_names! { _Grammar, _TerminalIndex, _String, _Token, _Ord, }

    // `_TerminalIndex` is used twice in `quote!`,
    // causing quote create a new `non_snake_case` variable.
    #[allow(non_snake_case)]
    {
        quote_spanned! {token.span() =>
            pub struct WrappedToken(#ty);

            impl #_Token<#_String> for WrappedToken {
                #[allow(unused_variables)]
                fn terminal(&self) -> #_String {
                    match self {
                        #(
                            WrappedToken(#pats) => #lits.to_string(),
                        )*
                    }
                }

                #[allow(unused_variables)]
                fn terminal_index<N>(
                    &self, grammar: &#_Grammar<N, #_String>
                ) -> #_TerminalIndex
                where
                    N: #_Ord,
                {
                    match self {
                        #(
                            WrappedToken(#pats2) => unsafe {
                                #_TerminalIndex::new(#indices)
                            },
                        )*
                    }
                }
            }
        }
    }
}

pub fn reducer(ctx: &Context) -> TokenStream {
    let Context {
        content, grammar, ..
    } = ctx;

    let rule_index_values = grammar.rule_indices().map(rspg::grammar::RuleIndex::value);

    let error_type = &content.error.ty;
    use_names! { _Result, }
    let result_type = quote! {#_Result<Parsed, #error_type>};

    let reduce = Ident::new("reduce", Span::call_site());
    let rule_body = rule_index_values
        .clone()
        .map(|i| rule_reducer(ctx, &reduce, &content.rules[i]));

    use_names! { _Reduce, }
    quote! {
        fn reducer(mut #reduce: #_Reduce<Parsed, WrappedToken>) -> #result_type {
            match #reduce.rule.value() {
                #(
                    #rule_index_values => #rule_body,
                )*
                _ => unreachable!(),
            }
        }
    }
}

pub fn rule_reducer(ctx: &Context, reduce: &Ident, rule: &data::Rule) -> TokenStream {
    let left = &rule.left;
    let left_type = &ctx.nonterminal_map[&left.to_string()].ty;
    let body = &rule.body;
    let binders = rule
        .right
        .iter()
        .map(|pat_symbol| binder(ctx, reduce, pat_symbol));
    let error_type = &ctx.content.error.ty;
    use_names! { _Result, }
    quote_spanned! {rule.span() =>
        {
            #(
                #binders
            )*
            #[allow(clippy::redundant_closure_call)]
            let value = (|| -> #_Result<#left_type, #error_type> {
                #body
            })();
            value.map(Parsed::#left)
        }
    }
}

pub fn binder(ctx: &Context, reduce: &Ident, pat_symbol: &data::PatSymbol) -> TokenStream {
    let data::PatSymbol { pat, symbol } = pat_symbol;
    match pat {
        None => TokenStream::new(),
        Some((_, pat, _)) => {
            let expr = match symbol {
                data::Symbol::Nonterminal(n) => {
                    let unwrapper = unwrap_ident(n);
                    quote_spanned! {pat_symbol.span() =>
                        #reduce.from
                            .pop_front()
                            .expect("expect a stack item")
                            .parsed()
                            .expect("expect a nonterminal")
                            .#unwrapper()
                    }
                }
                data::Symbol::Terminal(t) => {
                    let terminal = &ctx.terminal_map[&t.value()];
                    let pat = &terminal.pat;
                    let expr = &terminal.expr;
                    quote_spanned! {pat_symbol.span() =>
                        match #reduce.from
                                .pop_front()
                                .expect("expect a stack item")
                                .token()
                                .expect("expect a terminal") {
                            WrappedToken(#pat) => #expr,
                            _ => unreachable!(),
                        }
                    }
                }
            };
            quote_spanned! {pat_symbol.span() =>
                let #pat = #expr;
            }
        }
    }
}

pub fn parser(ctx: &Context) -> TokenStream {
    let Context { content, .. } = ctx;

    let token_type = &content.token.ty;
    let error_type = &content.error.ty;
    let start_type = &content
        .nonterminals
        .iter()
        .find(|n| n.ident == content.start.nonterminal)
        .unwrap()
        .ty;
    let unwrap_start = unwrap_ident(&content.start.nonterminal);
    use_names! { _lazy_static, _Result, _PhantomData, _Parser, _Reduce, _String, _Iterator, _ParserError, }
    quote! {
        #_lazy_static! {
            pub static ref PARSER: #_Parser<
                'static,
                'static,
                #_String,
                #_String,
                WrappedToken,
                Parsed,
                fn(#_Reduce<Parsed, WrappedToken>) -> #_Result<Parsed, #error_type>,
                #error_type,
            > = #_Parser {
                grammar: &GRAMMAR,
                table: &TABLE,
                reducer,
                phantom: #_PhantomData,
            };
        }

        pub fn parse<I>(input: I) -> #_Result<#start_type, #_ParserError<#error_type>>
        where
            I: #_Iterator<Item = #token_type>,
        {
            PARSER.parse(input.map(WrappedToken)).map(Parsed::#unwrap_start)
        }
    }
}

pub fn unwrap_ident(i: &Ident) -> Ident {
    Ident::new(
        &format!("unwrap_{}", i.to_string().to_lowercase()),
        i.span(),
    )
}

pub fn collect_errors<I>(mut es: I) -> Option<syn::Error>
where
    I: Iterator<Item = syn::Error>,
{
    match es.next() {
        Some(mut first) => {
            first.extend(es);
            Some(first)
        }
        None => None,
    }
}
