mod subcommand;

use proc_macro2::TokenStream as TokenStream2;
use syn::{Attribute, DataEnum, Generics, Ident};

pub fn derive_args_enum(
    name: &Ident,
    data: &DataEnum,
    generics: &Generics,
    attrs: &[Attribute],
) -> syn::Result<TokenStream2> {
    todo!()
}
