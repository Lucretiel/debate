mod subcommand;

use darling::FromAttributes;
use proc_macro2::TokenStream as TokenStream2;
use syn::{Attribute, Generics, Ident, Lifetime, Token, Variant, punctuated::Punctuated};

use crate::generics::AngleBracedLifetime;

#[derive(FromAttributes)]
#[darling(attributes(debate))]
struct EnumAttr {
    subcommand: Option<()>,
}

pub fn derive_args_enum(
    name: &Ident,
    variants: &Punctuated<Variant, Token![,]>,
    lifetime: &Lifetime,
    type_lifetime: Option<&AngleBracedLifetime>,
    attrs: &[Attribute],
) -> syn::Result<TokenStream2> {
    let attr = EnumAttr::from_attributes(attrs)?;

    if attr.subcommand.is_some() {
        subcommand::derive_args_enum_subcommand(name, variants, lifetime, type_lifetime)
    } else {
        todo!("not implemented yet")
    }
}
