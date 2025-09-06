mod subcommand;

use darling::FromAttributes;
use proc_macro2::TokenStream as TokenStream2;
use syn::{Attribute, Ident, Lifetime, Token, Variant, punctuated::Punctuated};

use crate::{common::enumeration::ValueEnumAttr, generics::AngleBracedLifetime};

pub fn derive_args_enum(
    name: &Ident,
    variants: &Punctuated<Variant, Token![,]>,
    lifetime: &Lifetime,
    type_lifetime: Option<&AngleBracedLifetime>,
    attrs: &[Attribute],
) -> syn::Result<TokenStream2> {
    let attr = ValueEnumAttr::from_attributes(attrs)?;

    if attr.subcommand.is_some() {
        subcommand::derive_args_enum_subcommand(name, variants, lifetime, type_lifetime)
    } else {
        todo!(
            "not implemented yet; this will be a mutually exclusive group \
            of flags. Use `#[debate(subcommand)] to make a subcommand"
        )
    }
}
