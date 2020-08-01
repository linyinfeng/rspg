mod rspg;

#[proc_macro]
pub fn rspg(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    rspg::rspg(input)
}
