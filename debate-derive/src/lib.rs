use proc_macro::TokenStream;

mod common;
mod from_args;
mod generics;
mod usage;
mod value;

#[proc_macro_derive(FromArgs, attributes(debate))]
pub fn derive_args(item: TokenStream) -> TokenStream {
    match from_args::derive_args_result(item.into()) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[proc_macro_derive(Value, attributes(debate))]
pub fn derive_value(item: TokenStream) -> TokenStream {
    match value::derive_value_result(item.into()) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[proc_macro_derive(Usage, attributes(debate, doc))]
pub fn derive_usage(item: TokenStream) -> TokenStream {
    match usage::derive_usage_result(item.into()) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}
