use darling::FromAttributes;
use heck::ToSnakeCase;
use itertools::Itertools as _;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{Attribute, Field, Ident, Token, punctuated::Punctuated};

use super::common::struct_usage_items;
use crate::{
    common::{ParsedFieldInfo, RawParsedTypeAttr, compute_docs},
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

    let docs = compute_docs(attrs)?;
    let docs = docs.quote();
    let attrs = RawParsedTypeAttr::from_attributes(attrs)?;
    let items = struct_usage_items(&fields, attrs.help_option());
    let command_name = name.to_string().to_snake_case();

    Ok(quote! {
        impl #lifetime ::debate::help::Usage for #name #lifetime {
            const NAME: &'static str = #command_name;
            const DESCRIPTION: ::debate::help::Description<'static> = #docs;
            const ITEMS: ::debate::help::UsageItems<'static> = ::debate::help::UsageItems::Parameters {
                parameters: #items,
            };
        }
    })
}
