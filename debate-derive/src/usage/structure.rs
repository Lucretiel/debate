use darling::FromAttributes;
use itertools::Itertools as _;
use proc_macro2::TokenStream as TokenStream2;

use syn::{Attribute, Field, Ident, Token, punctuated::Punctuated};

use super::common::struct_usage_implementation;
use crate::{
    common::{ParsedFieldInfo, RawParsedTypeAttr},
    generics::AngleBracedLifetime,
};

pub fn derive_usage_struct(
    name: &Ident,
    fields: &Punctuated<Field, Token![,]>,
    lifetime: Option<&AngleBracedLifetime>,
    attrs: &[Attribute],
) -> syn::Result<TokenStream2> {
    let fields: Vec<ParsedFieldInfo> = fields
        .iter()
        .map(ParsedFieldInfo::from_field)
        .try_collect()?;

    let attrs = RawParsedTypeAttr::from_attributes(attrs)?;

    Ok(struct_usage_implementation(
        name,
        &fields,
        attrs.help_option(),
        lifetime,
    ))
}
