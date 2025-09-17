pub mod flag_set;
pub mod subcommand;

use std::borrow::Cow;

use darling::{FromAttributes, util::SpannedValue};
use proc_macro2::Span;
use quote::format_ident;
use syn::Ident;

use super::IdentString;

#[derive(FromAttributes)]
#[darling(attributes(debate))]
pub struct ValueEnumAttr {
    pub subcommand: Option<()>,
    pub short: Option<SpannedValue<()>>,
    pub long: Option<SpannedValue<()>>,
}

pub fn create_non_colliding_ident<'a>(
    root: &str,
    variants: impl Iterator<Item = &'a IdentString<'a>> + Clone,
) -> Ident {
    let mut ident = Cow::Borrowed(root);

    while variants.clone().any(|existing| existing.as_str() == ident) {
        ident = Cow::Owned(format!("_{ident}"));
    }

    format_ident!("{ident}", span = Span::mixed_site())
}
