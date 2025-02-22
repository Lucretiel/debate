use proc_macro2::TokenStream as TokenStream2;
use syn::{Attribute, DataEnum, Generics, Ident, Token, Variant, punctuated::Punctuated};

pub fn derive_args_enum_subcommand(
    name: &Ident,
    variants: &Punctuated<Variant, Token![,]>,
    generics: &Generics,
    attrs: &[Attribute],
) -> syn::Result<TokenStream2> {
}
