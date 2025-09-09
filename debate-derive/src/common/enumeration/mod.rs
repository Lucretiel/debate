pub mod flag_set;
pub mod subcommand;

use darling::FromAttributes;
use heck::ToKebabCase as _;
use itertools::Itertools as _;
use quote::format_ident;
use syn::{Fields, Ident, Type, Variant, spanned::Spanned as _};

use super::{Description, IdentString, ParsedFieldInfo, compute_docs};

#[derive(FromAttributes)]
#[darling(attributes(debate))]
pub struct ValueEnumAttr {
    pub subcommand: Option<()>,
    pub short: Option<()>,
    pub long: Option<()>,
}
