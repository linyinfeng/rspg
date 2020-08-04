pub(crate) mod generate;

#[proc_macro]
pub fn rspg(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    generate::generate(input.into())
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}
