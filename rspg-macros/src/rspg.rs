mod data;

use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote;
use quote::quote_spanned;
use quote::ToTokens;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use syn::Ident;
use syn::Type;

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

pub fn rspg(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let parsed = syn::parse_macro_input!(input as data::RspgMod);
    // println!("{:#?}", parsed);
    let mod_vis = parsed.visibility;
    let mod_name = parsed.mod_name;
    let mod_outer_attrs = parsed.outer_attrs;
    let mod_inner_attrs = parsed.inner_attrs;
    let contents = build_contents(parsed.content);
    let result = quote! {
        #(#mod_outer_attrs)*
        #mod_vis mod #mod_name {
            #(#mod_inner_attrs)*
            use super::*;
            #contents
        }
    };
    result.into()
}

fn build_contents(content: data::RspgContent) -> TokenStream {
    let mut result = proc_macro2::TokenStream::new();

    use_names! { _Grammar, _Vec, _String, _RuleIndex, _FirstSets, _FollowSets, _Table, }

    let grammar = build_grammar(&content);
    assert_content(&content, &grammar);

    result.extend(embed_data(
        "GRAMMAR",
        quote!(#_Grammar<#_String, #_String>),
        &grammar,
    ));
    let rules: Vec<_> = grammar.rule_indices().collect();
    result.extend(embed_data("RULES", quote!(#_Vec<#_RuleIndex>), &rules));
    let first_sets = rspg::set::FirstSets::of_grammar(&grammar);
    result.extend(embed_data("FIRST_SETS", quote!(#_FirstSets), &first_sets));
    let follow_sets = rspg::set::FollowSets::of_grammar(&grammar, &first_sets);
    result.extend(embed_data(
        "FOLLOW_SETS",
        quote!(#_FollowSets),
        &follow_sets,
    ));
    let table = rspg::lr1::generator::Generator::construct(&grammar, &first_sets, "".to_string())
        .generate(&grammar)
        .unwrap();
    result.extend(embed_data("TABLE", quote!(#_Table), &table));

    result.extend(parsed_type(&content.nonterminals));
    result.extend(token_impl(&grammar, &content.token, &content.terminals));
    result.extend(reducer(&grammar, &content));

    result.extend(parser(&content));

    result
}

fn assert_content(content: &data::RspgContent, grammar: &rspg::grammar::Grammar<String, String>) {
    assert_eq!(
        content
            .terminals
            .iter()
            .map(|d| d.lit.value())
            .collect::<BTreeSet<_>>(),
        grammar.terminals().cloned().collect::<BTreeSet<_>>(),
        "terminal definitions should match terminals in rule"
    );
    assert_eq!(
        content
            .nonterminals
            .iter()
            .map(|d| d.ident.to_string())
            .collect::<BTreeSet<_>>(),
        grammar.nonterminals().cloned().collect::<BTreeSet<_>>(),
        "nonterminal definitions should match nonterminals in rule"
    );
}

fn build_grammar(content: &data::RspgContent) -> rspg::grammar::Grammar<String, String> {
    let mut builder = rspg::grammar::GrammarBuilder::new();
    builder = builder.start(content.start.nonterminal.to_string());
    for rule in &content.rules {
        builder = builder.push_rule_left(rule.left.to_string());
        for right in &rule.right {
            builder = match &right.symbol {
                data::Symbol::Nonterminal(n) => builder.push_rule_right_nonterminal(n.to_string()),
                data::Symbol::Terminal(t) => builder.push_rule_right_terminal(t.value()),
            };
        }
    }
    builder.build()
}

fn embed_data<T>(name: &str, ty: TokenStream, data: &T) -> TokenStream
where
    T: serde::Serialize,
{
    let ident = Ident::new(name, Span::call_site());
    let string =
        ron::ser::to_string(data).unwrap_or_else(|_| panic!("failed to serialize data {}", name));
    use_names! { _lazy_static, _ron_from_str, }
    quote! {
        #_lazy_static! {
            pub static ref #ident: #ty = #_ron_from_str(#string).unwrap();
        }
    }
}

fn parsed_type(nonterminals: &[data::NonterminalDescription]) -> TokenStream {
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

// `_TerminalIndex` is used twice in this function,
// causing quote create a new `non_snake_case` variable.
#[allow(non_snake_case)]
fn token_impl(
    grammar: &rspg::grammar::Grammar<String, String>,
    token: &data::TokenDescription,
    terminals: &[data::TerminalDescription],
) -> TokenStream {
    let ty = &token.ty;
    let lits = terminals.iter().map(|d| &d.lit);
    let pats = terminals.iter().map(|d| &d.pat);
    let indices = lits
        .clone()
        .map(|l| grammar.terminal_index(&l.value()).value());
    let _exprs = terminals.iter().map(|d| &d.expr);

    let pats2 = pats.clone();
    use_names! { _Grammar, _TerminalIndex, _String, _Token, _Ord, }
    quote! {
        impl #_Token<#_String> for #ty {
            #[allow(unused_variables)]
            fn terminal(&self) -> #_String {
                match self {
                    #(
                        #pats => #lits.to_string(),
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
                        #pats2 => unsafe {
                            #_TerminalIndex::new(#indices)
                        },
                    )*
                }
            }
        }
    }
}

fn reducer(
    grammar: &rspg::grammar::Grammar<String, String>,
    content: &data::RspgContent,
) -> TokenStream {
    let token_type = &content.token.ty;
    let rule_index_values = grammar.rule_indices().map(rspg::grammar::RuleIndex::value);

    let error_type = &content.error.ty;
    use_names! { _Result, }
    let result_type = quote! {#_Result<Parsed, #error_type>};

    let terminal_map = content
        .terminals
        .iter()
        .map(|t| (t.lit.value(), t))
        .collect();
    let nonterminal_map = content
        .nonterminals
        .iter()
        .map(|t| (t.ident.to_string(), t))
        .collect();
    let rule_body = rule_index_values.clone().map(|i| {
        rule_reducer(
            &quote! { _r.from },
            &content.rules[i],
            &terminal_map,
            &nonterminal_map,
            error_type,
        )
    });

    use_names! { _Reduce, }
    quote! {
        fn reducer(mut _r: #_Reduce<Parsed, #token_type>) -> #result_type {
            match _r.rule.value() {
                #(
                    #rule_index_values => #rule_body,
                )*
                _ => unreachable!(),
            }
        }
    }
}

fn rule_reducer(
    from: &TokenStream,
    rule: &data::Rule,
    terminal_map: &BTreeMap<String, &data::TerminalDescription>,
    nonterminal_map: &BTreeMap<String, &data::NonterminalDescription>,
    error_type: &Type,
) -> TokenStream {
    let left = &rule.left;
    let left_type = &nonterminal_map[&left.to_string()].ty;
    let body = &rule.body;
    let binders = rule
        .right
        .iter()
        .map(|pat_symbol| binder(from, pat_symbol, terminal_map));
    use_names! { _Result, }
    quote! {
        {
            #(
                #binders
            )*
            let value = (|| -> #_Result<#left_type, #error_type> {
                #body
            })();
            value.map(Parsed::#left)
        }
    }
}

fn binder(
    from: &TokenStream,
    pat_symbol: &data::PatSymbol,
    terminal_map: &BTreeMap<String, &data::TerminalDescription>,
) -> TokenStream {
    let data::PatSymbol { pat, symbol } = pat_symbol;
    let pat_tokens = match pat {
        Some((pat, _)) => pat.to_token_stream(),
        None => quote_spanned! {symbol.span() => _ },
    };
    let expr = match symbol {
        data::Symbol::Nonterminal(n) => {
            let unwrapper = unwrap_ident(n);
            quote! {
                #from
                    .pop_front()
                    .expect("expect a stack item")
                    .parsed()
                    .expect("expect a nonterminal")
                    .#unwrapper();
            }
        }
        data::Symbol::Terminal(t) => {
            let terminal = &terminal_map[&t.value()];
            let pat = &terminal.pat;
            let expr = &terminal.expr;
            quote! {
                match #from
                        .pop_front()
                        .expect("expect a stack item")
                        .token()
                        .expect("expect a terminal") {
                    #pat => #expr,
                    _ => unreachable!(),
                }
            }
        }
    };
    quote! {
        let #pat_tokens = #expr;
    }
}

fn unwrap_ident(i: &Ident) -> Ident {
    Ident::new(
        &format!("unwrap_{}", i.to_string().to_lowercase()),
        i.span(),
    )
}

fn parser(content: &data::RspgContent) -> TokenStream {
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
                #token_type,
                Parsed,
                fn(#_Reduce<Parsed, #token_type>) -> #_Result<Parsed, #error_type>,
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
            PARSER.parse(input).map(Parsed::#unwrap_start)
        }
    }
}
