mod flag_set;
mod subcommand;

use darling::FromAttributes as _;
use proc_macro2::TokenStream as TokenStream2;
use syn::{Attribute, Ident, Token, Variant, punctuated::Punctuated};

use crate::{common::enumeration::ValueEnumAttr, generics::AngleBracedLifetime};

pub fn derive_usage_enum(
    name: &Ident,
    variants: &Punctuated<Variant, Token![,]>,
    type_lifetime: Option<&AngleBracedLifetime>,
    attrs: &[Attribute],
) -> syn::Result<TokenStream2> {
    let attr = ValueEnumAttr::from_attributes(attrs)?;

    if attr.subcommand.is_some() {
        subcommand::derive_usage_enum_subcommand(name, variants, type_lifetime)
    } else {
        flag_set::derive_usage_enum_flag_set(name, variants, type_lifetime, &attr)
    }
}
