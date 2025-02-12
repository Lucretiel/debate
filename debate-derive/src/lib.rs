use proc_macro::TokenStream;

mod from_args;
//mod value;

#[proc_macro_derive(FromArgs, attributes(doc, debate))]
pub fn derive_args(item: TokenStream) -> TokenStream {
    match from_args::derive_args_result(item.into()) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

// #[proc_macro_derive(Value, attributes(debate))]
// pub fn derive_value(item: TokenStream) -> TokenStream {}
