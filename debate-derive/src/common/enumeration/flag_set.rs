use std::{collections::HashMap, ops::ControlFlow};

use darling::{
    FromAttributes,
    util::{Override, SpannedValue},
};
use itertools::Itertools as _;
use proc_macro2::{Delimiter, Group, Span, TokenStream, TokenTree};
use quote::ToTokens;
use syn::{Attribute, Expr, Ident, Type, Variant, spanned::Spanned};

use crate::common::{
    Description, FieldDefault, FlagTags, IdentString, compute_docs, compute_placeholder,
    compute_tags,
};

use super::create_non_colliding_ident;

#[derive(darling::FromAttributes, Debug)]
#[darling(attributes(debate))]
struct RawParsedFlagSetFlagAttr {
    long: Option<SpannedValue<Override<String>>>,
    short: Option<SpannedValue<Override<char>>>,
    default: Option<SpannedValue<Override<Expr>>>,
    placeholder: Option<SpannedValue<String>>,
    #[darling(rename = "override")]
    overridable: Option<SpannedValue<()>>,
}

pub struct FlagSetFlagInfo<Type> {
    pub docs: Description,
    pub placeholder: SpannedValue<String>,
    pub ty: Type,
    pub default: Option<FieldDefault>,
    pub tags: FlagTags<SpannedValue<String>, SpannedValue<char>>,
    pub overridable: Option<SpannedValue<()>>,
}

pub enum VariantMode<'a> {
    Plain(FlagSetFlagInfo<FlagSetType<'a>>),
    Struct(Vec<(IdentString<'a>, FlagSetFlagInfo<&'a Type>)>),
}

pub struct FlagSetVariant<'a> {
    pub ident: IdentString<'a>,
    pub mode: VariantMode<'a>,
}

#[derive(Clone, Copy)]
pub enum FlagSetType<'a> {
    Unit,
    Typed(&'a Type),
}

impl<'a> From<&'a Type> for FlagSetType<'a> {
    fn from(ty: &'a Type) -> Self {
        Self::Typed(ty)
    }
}

impl ToTokens for FlagSetType<'_> {
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

pub struct ParsedFlagSetInfo<'a> {
    pub superposition: Ident,
    pub variants: Vec<FlagSetVariant<'a>>,
}

fn compute_flag_set_tags(
    auto_long: Option<&Span>,
    auto_short: Option<&Span>,
    long: Option<SpannedValue<Override<String>>>,
    short: Option<SpannedValue<Override<char>>>,
    ident: &IdentString,
) -> syn::Result<FlagTags<SpannedValue<String>, SpannedValue<char>>> {
    let long = long.or_else(|| auto_long.map(|long| SpannedValue::new(Override::Inherit, *long)));
    let short =
        short.or_else(|| auto_short.map(|short| SpannedValue::new(Override::Inherit, *short)));

    compute_tags(long, short, ident)?.ok_or_else(|| {
        syn::Error::new(
            ident.span(),
            "flag must have a short or long name. Add #[debate(short)] or \
            #[debate(long)] to the top-level enum to automatically add it to \
            all flags.",
        )
    })
}

fn create_flag<'a, Type>(
    auto_long: Option<&Span>,
    auto_short: Option<&Span>,

    ident: &IdentString<'a>,
    ty: Type,
    attrs: &'a [Attribute],
) -> syn::Result<FlagSetFlagInfo<Type>> {
    let attr = RawParsedFlagSetFlagAttr::from_attributes(attrs)?;

    Ok(FlagSetFlagInfo {
        docs: compute_docs(attrs)?,
        placeholder: compute_placeholder(attr.placeholder, ident)?,
        ty,
        default: FieldDefault::new(attr.default),
        tags: compute_flag_set_tags(auto_long, auto_short, attr.long, attr.short, ident)?,
        overridable: attr.overridable,
    })
}

fn create_whole_flag_from_variant<'a>(
    auto_long: Option<&Span>,
    auto_short: Option<&Span>,

    ident: IdentString<'a>,
    ty: FlagSetType<'a>,
    attrs: &'a [Attribute],
) -> syn::Result<FlagSetVariant<'a>> {
    Ok(FlagSetVariant {
        mode: VariantMode::Plain(create_flag(auto_long, auto_short, &ident, ty, attrs)?),
        ident,
    })
}

impl<'a> ParsedFlagSetInfo<'a> {
    pub fn from_variants(
        variants: impl IntoIterator<Item = &'a Variant>,
        auto_long: Option<&Span>,
        auto_short: Option<&Span>,
    ) -> syn::Result<Self> {
        let variants: Vec<FlagSetVariant<'a>> = variants
            .into_iter()
            .map(|variant| {
                let variant_ident = IdentString::new(&variant.ident);

                let mode = match variant.fields {
                    syn::Fields::Unit => create_flag(
                        auto_long,
                        auto_short,
                        &variant_ident,
                        FlagSetType::Unit,
                        &variant.attrs,
                    )
                    .map(VariantMode::Plain),

                    syn::Fields::Unnamed(ref fields) => create_flag(
                        auto_long,
                        auto_short,
                        &variant_ident,
                        fields
                            .unnamed
                            .iter()
                            .exactly_one()
                            .map(|field| &field.ty)
                            .map(FlagSetType::Typed)
                            .map_err(|_| {
                                syn::Error::new(
                                    fields.span(),
                                    "must have exactly one field (the flag argument)",
                                )
                            })?,
                        &variant.attrs,
                    )
                    .map(VariantMode::Plain),

                    syn::Fields::Named(ref fields) => fields
                        .named
                        .iter()
                        .map(|field| {
                            let field_ident = field
                                .ident
                                .as_ref()
                                .map(IdentString::new)
                                .expect("all fields in a struct variant have names");

                            // TODO: collision detection for flags within the
                            // same struct.
                            create_flag(
                                auto_long,
                                auto_short,
                                &field_ident,
                                &field.ty,
                                &field.attrs,
                            )
                            .map(|flag| (field_ident, flag))
                        })
                        .try_collect()
                        .map(VariantMode::Struct),
                };

                mode.map(|mode| FlagSetVariant {
                    ident: variant_ident,
                    mode,
                })
            })
            .try_collect()?;

        let superposition = create_non_colliding_ident(
            "Superposition",
            variants.iter().map(|variant| &variant.ident),
        );

        Ok(Self {
            superposition,
            variants,
        })
    }
}

pub struct FlagSetFlag<'a> {
    /// The first place this flag appears; either the variant name or the field
    /// name
    pub origin: &'a IdentString<'a>,

    /// All of the variants where this flag appears, along with the index
    /// of its location in that variant (if relevant)
    pub variants: Vec<(&'a IdentString<'a>, Option<usize>)>,

    /// Precomputed set of all the variants where this flag DOESN'T appear,
    /// along with their indices.
    pub excluded: HashMap<&'a str, usize>,

    /// Documentation for this flag. If the flag appears more than once in
    /// different variants, we choose the longest description.
    pub docs: &'a Description,

    /// Placeholder for the flag. Must be identical in all instances.
    pub placeholder: &'a str,

    /// Type of the flag (or Unit, for a unit enum). We don't check consistency
    /// between types; we rely on the compilers own type-checking to both
    /// check the type and produce a better error message about the wrong
    /// type
    pub ty: FlagSetType<'a>,

    /// Computed tags for this flag. Possibly enabled by a debate attribute
    /// on the enum itself, in addition to the flag
    pub tags: FlagTags<&'a str, char>,

    /// If true, subsequent instances of this flag override earlier ones.
    ///  this setting must be identical for all instances.
    pub overridable: bool,
}

/// Compare a pair of tags. In the past this returned useful spans of where
/// the comparison failures were, but for now it's a simple boolean op.
fn compare_tags<T: Eq>(left: Option<SpannedValue<T>>, right: Option<T>) -> bool {
    match (left, right) {
        (None, None) => true,
        (Some(_), None) => false,
        (None, Some(_)) => false,
        (Some(left), Some(right)) => *left == right,
    }
}

/// Find an element in an iterator with a fallible filter function. If the
/// filter functon returns an error, that error is propagated; otherwise,
/// the first matching element is returned. Note that this function returns
/// eagerly; if the filter finds an item, any future errors that might have
/// occurred won't be discovered.
fn try_find<I: IntoIterator, E>(
    iterator: I,
    mut filter: impl FnMut(&I::Item) -> Result<bool, E>,
) -> Result<Option<I::Item>, E> {
    iterator
        .into_iter()
        .try_for_each(|item| match filter(&item) {
            Err(err) => ControlFlow::Break(Err(err)),
            Ok(true) => ControlFlow::Break(Ok(item)),
            Ok(false) => ControlFlow::Continue(()),
        })
        .break_value()
        .transpose()
}

/// For a given instance of a flag, add it to the `set` and perform various
/// consistency checks on it. It's fine for flags to appear in more than
/// one variant of a flag set, but if they do, they must have identical tags
/// (both short and long), as well as a handful other other things that are
/// shared.
fn add_or_update_flag<'a, Type>(
    set: &mut Vec<FlagSetFlag<'a>>,
    flag: &'a FlagSetFlagInfo<Type>,
    field: Option<&'a IdentString<'a>>,
    variant: &'a IdentString<'a>,
    index: Option<usize>,
    all_variants: &HashMap<&'a str, usize>,
) -> syn::Result<()>
where
    Type: Copy + Into<FlagSetType<'a>>,
{
    let origin = field.unwrap_or(variant);

    let long = flag.tags.long();
    let short = flag.tags.short();

    let existing_flag = try_find(set.iter_mut(), |existing| {
        let existing_long = existing.tags.long();
        let existing_short = existing.tags.short();

        match (
            compare_tags(long, existing_long),
            compare_tags(short, existing_short),
        ) {
            (false, true) => Err("this instance has a different --long"),
            (true, false) => Err("this instance has a different -s short"),

            // Total mismatch, these aren't even the same flag
            (false, false) => Ok(false),

            // Found a match. Other parts of it need to be checked for
            // consistency.
            (true, true) => {
                // We check its placeholder and its override setting.
                // The docs will be merged (longer docs preferred).
                // We don't bother checking the type because that'll show up
                // separately when the rust compiler itself attemtps to compile
                // the generated code.
                // TODO: find a way to use the `checks!` macro here. It seems
                // impossible because you can't use `#[macro_export]` in a
                // proc macro crate, even to reuse a macro internally.
                if flag.placeholder.as_str() != existing.placeholder {
                    // Nit: placeholders are just a docs convention. In theory
                    // we should be able to detect and use a custom placeholder
                    // and reject only distinctions between custom placeholders.
                    Err("this instance has a different placeholder")
                } else if flag.overridable.is_some() != existing.overridable {
                    Err("this instance has a different `override` setting")
                } else {
                    Ok(true)
                }
            }
        }
        .map_err(|message| {
            let mut error = syn::Error::new(
                origin.span(),
                "multiple instances of the same tag must be (mostly) identical",
            );
            error.combine(syn::Error::new(existing.origin.span(), message));
            error
        })
    })?;

    let existing_flag = match existing_flag {
        Some(flag) => flag,
        None => {
            set.push(FlagSetFlag {
                origin,
                variants: Vec::new(),
                excluded: all_variants.clone(),
                docs: const { &Description::empty() },
                placeholder: &flag.placeholder,
                ty: flag.ty.into(),
                tags: flag.tags.simplify(),
                overridable: flag.overridable.is_some(),
            });

            set.last_mut().expect("we just pushed an item into the set")
        }
    };

    existing_flag.variants.push((variant, index));
    existing_flag.excluded.remove(variant.as_str());

    if existing_flag.docs.full.len() > flag.docs.full.len() {
        existing_flag.docs = &flag.docs;
    };

    Ok(())
}

/// Given the parsed variants, create the flat, deduplicated list of flags
/// within those variants. This is where various consistency checks happen
/// (duplicate flags must be mostly identical) and where things like the
/// exclusion sets are calculated.
pub fn compute_grouped_flags<'a>(
    variants: &'a [FlagSetVariant<'a>],
) -> syn::Result<Vec<FlagSetFlag<'a>>> {
    let mut flags = Vec::new();

    let all_variants = variants
        .iter()
        .map(|variant| variant.ident.as_str())
        .enumerate()
        .map(|(index, name)| (name, index))
        .collect();

    variants.iter().try_for_each(|variant| match variant.mode {
        VariantMode::Plain(ref flag) => {
            add_or_update_flag(&mut flags, &flag, None, &variant.ident, None, &all_variants)
        }
        VariantMode::Struct(ref fields) => {
            fields
                .iter()
                .enumerate()
                .try_for_each(|(index, (ident, flag))| {
                    add_or_update_flag(
                        &mut flags,
                        flag,
                        Some(ident),
                        &variant.ident,
                        Some(index),
                        &all_variants,
                    )
                })
        }
    })?;

    Ok(flags)
}
