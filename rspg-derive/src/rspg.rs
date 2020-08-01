mod data;

use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote;
use quote::ToTokens;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use syn::Ident;

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
    return result.into();
}

fn build_contents(content: data::RspgContent) -> TokenStream {
    let mut result = proc_macro2::TokenStream::new();

    let grammar = build_grammar(&content);
    assert_content(&content, &grammar);

    result.extend(embed_data(
        "GRAMMAR",
        quote!(::rspg::grammar::Grammar<String, String>),
        &grammar,
    ));
    let rules: Vec<_> = grammar.rule_indices().collect();
    result.extend(embed_data(
        "RULES",
        quote!(::std::vec::Vec<::rspg::grammar::RuleIndex>),
        &rules,
    ));
    let first_sets = rspg::set::FirstSets::of_grammar(&grammar);
    result.extend(embed_data(
        "FIRST_SETS",
        quote!(::rspg::set::FirstSets),
        &first_sets,
    ));
    let follow_sets = rspg::set::FollowSets::of_grammar(&grammar, &first_sets);
    result.extend(embed_data(
        "FOLLOW_SETS",
        quote!(::rspg::set::FollowSets),
        &follow_sets,
    ));
    let table = rspg::lr1::generator::Generator::construct(&grammar, &first_sets, "".to_string())
        .generate(&grammar)
        .unwrap();
    result.extend(embed_data(
        "TABLE",
        quote!(::rspg::lr1::table::Table),
        &table,
    ));

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
    let string = ron::ser::to_string(data).expect(&format!("failed to serialize data {}", name));
    quote! {
        ::lazy_static::lazy_static! {
            pub static ref #ident: #ty = ::ron::de::from_str(#string).unwrap();
        }
    }
}

fn parsed_type(nonterminals: &Vec<data::NonterminalDescription>) -> TokenStream {
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

fn token_impl(
    grammar: &rspg::grammar::Grammar<String, String>,
    token: &data::TokenDescription,
    terminals: &Vec<data::TerminalDescription>,
) -> TokenStream {
    let ty = &token.ty;
    let lits = terminals.iter().map(|d| &d.lit);
    let pats = terminals.iter().map(|d| &d.pat);
    let indices = lits
        .clone()
        .map(|l| grammar.terminal_index(&l.value()).value());
    let _exprs = terminals.iter().map(|d| &d.expr);

    let pats2 = pats.clone();
    quote! {
        impl ::rspg::token::Token<String> for #ty {
            #[allow(unused_variables)]
            fn terminal(&self) -> String {
                match self {
                    #(
                        #pats => #lits.to_string(),
                    )*
                }
            }

            #[allow(unused_variables)]
            fn terminal_index<N>(
                &self, grammar: &::rspg::grammar::Grammar<N, String>
            ) -> ::rspg::grammar::TerminalIndex
            where
                N: Ord,
            {
                match self {
                    #(
                        #pats2 => unsafe {
                            ::rspg::grammar::TerminalIndex::new(#indices)
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
    let result_type = quote! {::std::result::Result<Parsed, #error_type>};
    let terminal_map = content
        .terminals
        .iter()
        .map(|t| (t.lit.value(), t))
        .collect();
    let rule_body = rule_index_values
        .clone()
        .map(|i| rule_reducer(&quote! { _r.from }, &content.rules[i], &terminal_map));
    quote! {
        fn reducer(mut _r: ::rspg::lr1::parser::Reduce<Parsed, #token_type>) -> #result_type {
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
) -> TokenStream {
    let left = &rule.left;
    let body = &rule.body;
    let binders = rule
        .right
        .iter()
        .map(|pat_symbol| binder(from, pat_symbol, terminal_map));
    quote! {
        {
            (|| {
                #(
                    #binders
                )*
                #body
            })().map(Parsed::#left)
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
        None => quote! { _ },
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
    quote! {
        ::lazy_static::lazy_static! {
            pub static ref PARSER: ::rspg::lr1::parser::Parser<
                'static,
                'static,
                String,
                String,
                #token_type,
                Parsed,
                fn(::rspg::lr1::parser::Reduce<Parsed, Token>) -> Result<Parsed, #error_type>,
                #error_type,
            > = ::rspg::lr1::parser::Parser {
                grammar: &GRAMMAR,
                table: &TABLE,
                reducer,
                phantom: ::std::marker::PhantomData,
            };
        }
    }
}
