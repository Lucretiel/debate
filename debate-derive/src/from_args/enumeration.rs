mod subcommand;

use darling::FromAttributes;
use proc_macro2::TokenStream as TokenStream2;
use syn::{Attribute, DataEnum, Generics, Ident, Token, Variant, punctuated::Punctuated};

#[derive(FromAttributes)]
#[darling(attributes(debate))]
struct EnumAttr {
    subcommand: Option<()>,
}

pub fn derive_args_enum(
    name: &Ident,
    variants: &Punctuated<Variant, Token![,]>,
    generics: &Generics,
    attrs: &[Attribute],
) -> syn::Result<TokenStream2> {
    let attr = EnumAttr::from_attributes(attrs)?;

    if attr.subcommand.is_some() {
        subcommand::derive_args_enum_subcommand(name, variants, generics)
    } else {
        panic!("not implemented yet")
    }
}
