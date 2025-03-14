use darling::FromAttributes;
use heck::ToKebabCase as _;
use itertools::Itertools as _;
use quote::format_ident;
use syn::{Fields, FieldsNamed, FieldsUnnamed, Ident, Variant, spanned::Spanned as _};

use super::{IdentString, ParsedFieldInfo, compute_docs};

#[derive(FromAttributes)]
#[darling(attributes(debate))]
pub struct ValueEnumAttr {
    pub subcommand: Option<()>,
}

/// Ident of the unselected subcommand (that is, the enum variant)
pub enum Fallback<'a> {
    /// The user explicitly included this variant with `debate(fallback)`
    Explicit(&'a Ident),

    /// There's no user-provided unselected subcommand. This ident isn't a
    /// member of the original enum and has been computed to avoid a collision.
    Internal(Ident),
}

impl Fallback<'_> {
    pub fn ident(&self) -> &Ident {
        match *self {
            Fallback::Explicit(ident) => ident,
            Fallback::Internal(ref ident) => ident,
        }
    }
}

#[derive(darling::FromAttributes)]
#[darling(attributes(debate))]
struct VariantAttr {
    fallback: Option<()>,
}

pub enum VariantMode {
    Unit,
    Tuple,
    Struct,
}

pub struct ParsedSubcommandVariant<'a> {
    pub ident: IdentString<'a>,

    /// Case-converted command name
    pub command: String,

    /// All of the fields in this variant. Basically agnostic to
    /// struct/unit/tuple distinctions.
    pub fields: Vec<ParsedFieldInfo<'a>>,

    pub docs: String,

    pub mode: VariantMode,
}

pub struct ParsedSubcommandInfo<'a> {
    pub fallback: Fallback<'a>,
    pub variants: Vec<ParsedSubcommandVariant<'a>>,
}

impl<'a> ParsedSubcommandInfo<'a> {
    pub fn from_variants(variants: impl IntoIterator<Item = &'a Variant>) -> syn::Result<Self> {
        let mut fallback: Option<&Ident> = None;
        let mut parsed_variants = Vec::new();

        for variant in variants {
            let attr = VariantAttr::from_attributes(&variant.attrs)?;

            if let Some(()) = attr.fallback {
                if let Some(fallback) = fallback {
                    let mut err = syn::Error::new(
                        variant.span(),
                        "can't have more than one fallback variant",
                    );

                    err.combine(syn::Error::new(fallback.span(), "previous fallback here"));

                    return Err(err);
                }

                if !matches!(variant.fields, Fields::Unit) {
                    return Err(syn::Error::new(
                        variant.span(),
                        "fallback variant must be a unit variant",
                    ));
                }

                fallback = Some(&variant.ident);
            } else {
                let ident = IdentString::new(&variant.ident);
                // TODO: rename attribute
                let command = ident.as_str().to_kebab_case();

                let fields = match variant.fields {
                    Fields::Unnamed(FieldsUnnamed {
                        unnamed: ref fields,
                        ..
                    })
                    | Fields::Named(FieldsNamed {
                        named: ref fields, ..
                    }) => fields
                        .iter()
                        .map(ParsedFieldInfo::from_field)
                        .try_collect()?,

                    Fields::Unit => Vec::new(),
                };

                let mode = match variant.fields {
                    Fields::Named(_) => VariantMode::Struct,
                    Fields::Unnamed(_) => VariantMode::Tuple,
                    Fields::Unit => VariantMode::Unit,
                };

                let docs = compute_docs(&variant.attrs)?;

                parsed_variants.push(ParsedSubcommandVariant {
                    ident,
                    command,
                    fields,
                    mode,
                    docs,
                });
            }
        }

        let fallback = match fallback {
            Some(fallback) => Fallback::Explicit(fallback),
            None => {
                let mut base_ident = format_ident!("Fallback");

                // Performance nitpick: == on an identifier is internally doing
                // to_string
                while parsed_variants
                    .iter()
                    .any(|variant| *variant.ident.raw() == base_ident)
                {
                    base_ident = format_ident!("_{base_ident}");
                }

                Fallback::Internal(base_ident)
            }
        };

        Ok(Self {
            fallback,
            variants: parsed_variants,
        })
    }
}
