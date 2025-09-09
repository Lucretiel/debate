use darling::{
    FromAttributes,
    util::{Override, SpannedValue},
};
use itertools::Itertools as _;
use proc_macro2::{Delimiter, Group, TokenStream, TokenTree};
use quote::{ToTokens, format_ident, quote};
use syn::{Attribute, Expr, Ident, Type, Variant};

use crate::common::{
    Description, FieldDefault, FlagFieldInfo, FlagTags, IdentString, compute_docs,
};

#[derive(darling::FromAttributes, Debug)]
#[darling(attributes(debate))]
struct RawParsedFlagSetFlagAttr {
    long: Option<SpannedValue<Override<String>>>,
    short: Option<SpannedValue<Override<char>>>,
    default: Option<SpannedValue<Override<Expr>>>,
    placeholder: Option<SpannedValue<String>>,
    #[darling(rename = "override")]
    r#override: Option<SpannedValue<()>>,
}

enum VariantMode {
    Unit,
    Newtype,
    Struct,
}

struct FlagSetVariant<'a> {
    ident: &'a Ident,
    mode: VariantMode,
    fields: (),
}

enum FlagSetType<'a> {
    Unit,
    Typed(&'a Type),
}

impl<'a> ToTokens for FlagSetType<'a> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match *self {
            FlagSetType::Typed(ty) => ty.to_tokens(tokens),

            // Empty tuple
            FlagSetType::Unit => tokens.extend([TokenTree::Group(Group::new(
                Delimiter::Parenthesis,
                TokenStream::new(),
            ))]),
        }
    }
}

// TODO: most of this needs a span so that we can report errors when there
// are conflicts. `default` and `override` do, at least.
struct FlagSetFlag<'a> {
    docs: Description,
    placeholder: SpannedValue<String>,
    ty: &'a Type,
    default: Option<FieldDefault>,
    tags: FlagTags<SpannedValue<String>, SpannedValue<char>>,
    overridable: bool,

    // TODO: vibes are telling me to move this out to a separate struct
    // (maybe instead have it be `flags: Vec<(FlagSetFlag, Vec<Ident>)>`)
    variants: Vec<&'a Ident>,
}

fn add_or_update_flag<'a>(
    set: &mut Vec<FlagSetFlag<'a>>,
    flag: FlagSetFlag<'a>,
    variant: &'a Ident,
) -> syn::Result<()> {
    let tags = flag.tags.simplify();
    let long = tags.long();
    let short = tags.short();

    let existing_flag = set
        .iter_mut()
        .try_find(|existing| Err(syn::Error::new(variant.span(), "REPLACE ERROR MESSAGE")))?;

    if let Some(existing_flag) = existing_flag {
        todo!()
    } else {
        set.push(f);
    }
}

struct ParsedFlagSetInfo<'a> {
    superposition: Ident,
    variants: Vec<FlagSetVariant<'a>>,
    flags: Vec<FlagSetFlag<'a>>,
}

impl<'a> ParsedFlagSetInfo<'a> {
    pub fn from_variants(
        variants: impl IntoIterator<Item = &'a Variant>,
        auto_long: bool,
        auto_short: bool,
    ) -> syn::Result<Self> {
        let superposition = format_ident!("Superposition");

        let mut flags = Vec::new();

        let variants = variants
            .into_iter()
            .map(|variant| {
                let ident = IdentString::new(&variant.ident);

                match variant.fields {
                    syn::Fields::Unit => {
                        let attr = RawParsedFlagSetFlagAttr::from_attributes(&variant.attrs)?;
                        let docs = compute_docs(&variant.attrs)?;
                    }
                    syn::Fields::Unnamed(ref fields) => todo!(),
                    syn::Fields::Named(ref fields) => todo!(),
                }
            })
            .try_collect()?;

        todo!()
    }
}
