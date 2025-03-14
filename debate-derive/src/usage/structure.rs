use itertools::Itertools as _;
use proc_macro2::TokenStream as TokenStream2;

use syn::{Field, Ident, Token, punctuated::Punctuated};

use super::common::struct_usage_implementation;
use crate::{common::ParsedFieldInfo, generics::AngleBracedLifetime};

pub fn derive_usage_struct(
    name: &Ident,
    fields: &Punctuated<Field, Token![,]>,
    lifetime: Option<&AngleBracedLifetime>,
) -> syn::Result<TokenStream2> {
    let fields: Vec<ParsedFieldInfo> = fields
        .iter()
        .map(ParsedFieldInfo::from_field)
        .try_collect()?;

    Ok(struct_usage_implementation(name, &fields, lifetime))
}
