use std::ops::ControlFlow;

use darling::{
    FromAttributes,
    util::{Override, SpannedValue},
};
use indexmap::IndexMap;
use itertools::Itertools as _;
use proc_macro2::{Delimiter, Group, Span, TokenStream, TokenTree};
use quote::ToTokens;
use syn::{Expr, Ident, Token, Type, Variant, punctuated::Punctuated, spanned::Spanned};

use crate::common::{
    Description, FieldDefault, FlagTags, IdentString, compute_docs, compute_placeholder,
    compute_tags, error_pair,
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

#[derive(Clone, Copy)]
pub enum Family<Info> {
    Solitary(Info),
    Sibling,
}

impl<Info> Family<Info> {
    pub fn merge(&mut self, other: Family<()>, info: Info) {
        if matches!(other, Solitary(())) {
            *self = Solitary(info)
        }
    }
}

use Family::*;

pub struct FlagSetFlagInfo<Info> {
    pub docs: Description,
    pub placeholder: SpannedValue<String>,
    pub info: Info,
    pub default: Option<FieldDefault>,
    pub tags: FlagTags<SpannedValue<String>, SpannedValue<char>>,
    pub overridable: Option<SpannedValue<()>>,
}

pub trait FlagSetFlagExtra<'a> {
    fn as_flag_set_type(&self) -> FlagType<'a>;
    fn field(&'a self) -> Option<&'a IdentString<'a>>;
    fn site(&self) -> FlagSetFlagSite<'_>;
}

pub struct FlagFieldInfo<'a> {
    pub ty: &'a Type,
    pub ident: IdentString<'a>,
}

impl<'a> FlagSetFlagExtra<'a> for FlagFieldInfo<'a> {
    fn as_flag_set_type(&self) -> FlagType<'a> {
        FlagType::Typed(self.ty)
    }

    fn field(&'a self) -> Option<&'a IdentString<'a>> {
        Some(&self.ident)
    }

    fn site(&self) -> FlagSetFlagSite<'_> {
        FlagSetFlagSite::Field(&self.ident)
    }
}

pub enum PlainFlagInfo<'a> {
    Unit,
    Newtype(&'a Type),
    Struct(FlagFieldInfo<'a>),
}

impl<'a> PlainFlagInfo<'a> {
    pub fn ty(&self) -> FlagType<'a> {
        match *self {
            PlainFlagInfo::Unit => FlagType::Unit,
            PlainFlagInfo::Newtype(ty) => FlagType::Typed(ty),
            PlainFlagInfo::Struct(ref info) => FlagType::Typed(info.ty),
        }
    }
}

impl<'a> FlagSetFlagExtra<'a> for PlainFlagInfo<'a> {
    fn as_flag_set_type(&self) -> FlagType<'a> {
        self.ty()
    }

    fn field(&self) -> Option<&IdentString<'a>> {
        match *self {
            Self::Unit => None,
            Self::Newtype(_) => None,
            Self::Struct(ref info) => Some(&info.ident),
        }
    }

    fn site(&self) -> FlagSetFlagSite<'_> {
        match *self {
            Self::Unit => FlagSetFlagSite::Unit,
            Self::Newtype(_) => FlagSetFlagSite::Newtype,
            Self::Struct(ref field) => FlagSetFlagSite::Field(&field.ident),
        }
    }
}

// impl<'a> From<FlagFieldInfo<'a>> for PlainFlagInfo<'a> {
//     fn from(info: FlagFieldInfo<'a>) -> Self {
//         Self::Struct(info)
//     }
// }

// We disable this lint here for two reasons:
// - We expect the typical case to be plain variants, which means that there's
//   not a lot of wasted space
// - FlagSetVariant objects live in an IndexMap after construction, so they
//   aren't passed by move more than once and therefore don't impose any copy
//   penalty that the `Box` would help with
#[expect(clippy::large_enum_variant)]
pub enum FlagSetVariant<'a> {
    /// A plain variant is a unit variant, a newtype variant, OR a struct
    /// variant with exactly one field
    Plain(FlagSetFlagInfo<PlainFlagInfo<'a>>),
    Struct(Vec<FlagSetFlagInfo<FlagFieldInfo<'a>>>),
}

#[derive(Clone, Copy)]
pub enum FlagType<'a> {
    Unit,
    Typed(&'a Type),
}

impl ToTokens for FlagType<'_> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match *self {
            FlagType::Typed(ty) => ty.to_tokens(tokens),

            // Empty tuple
            FlagType::Unit => tokens.extend([TokenTree::Group(Group::new(
                Delimiter::Parenthesis,
                TokenStream::new(),
            ))]),
        }
    }
}

pub struct ParsedFlagSetInfo<'a> {
    pub superposition: Ident,
    pub variants: IndexMap<IdentString<'a>, FlagSetVariant<'a>>,
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

// This should be a function, but there are tricky borrow issues that are
// theoretically resolvable with excessive traits but I can't be bothered
macro_rules! create_flag {
    (
        $auto_long:ident,
        $auto_short:ident,

        source: $source:expr,
        info: $info:expr,
        attrs: $attrs:expr,
    ) => {
        RawParsedFlagSetFlagAttr::from_attributes($attrs).and_then(|attr| {
            Ok(FlagSetFlagInfo {
                docs: compute_docs($attrs)?,
                placeholder: compute_placeholder(attr.placeholder, $source)?,

                default: FieldDefault::new(attr.default),
                tags: compute_flag_set_tags(
                    $auto_long,
                    $auto_short,
                    attr.long,
                    attr.short,
                    $source,
                )?,
                overridable: attr.overridable,
                info: $info,
            })
        })
    };
}

impl<'a> ParsedFlagSetInfo<'a> {
    pub fn from_variants(
        variants: &'a Punctuated<Variant, Token![,]>,
        auto_long: Option<&Span>,
        auto_short: Option<&Span>,
    ) -> syn::Result<Self> {
        if variants.is_empty() {
            return Err(syn::Error::new(
                variants.span(),
                "must have at least one variant",
            ));
        }
        let variants: IndexMap<IdentString<'a>, FlagSetVariant<'a>> = variants
            .into_iter()
            .map(|variant| {
                let variant_ident = IdentString::new(&variant.ident);

                let mode = match variant.fields {
                    syn::Fields::Unit => create_flag!(
                        auto_long,
                        auto_short,
                        source: &variant_ident,
                        info: PlainFlagInfo::Unit,
                        attrs: &variant.attrs,
                    )
                    .map(FlagSetVariant::Plain),

                    syn::Fields::Unnamed(ref fields) => create_flag!(
                        auto_long,
                        auto_short,
                        source: &variant_ident,
                        info: fields
                            .unnamed
                            .iter()
                            .exactly_one()
                            .map(|field| &field.ty)
                            .map(PlainFlagInfo::Newtype)
                            .map_err(|_| {
                                syn::Error::new(
                                    fields.span(),
                                    "must have exactly one field (the flag argument)",
                                )
                            })?,
                        attrs: &variant.attrs,
                    )
                    .map(FlagSetVariant::Plain),

                    syn::Fields::Named(ref fields) => match fields
                        .named
                        .iter()
                        // Pre-compute the IdentString for each field
                        .map(|field| {
                            (
                                field
                                    .ident
                                    .as_ref()
                                    .map(IdentString::new)
                                    .expect("all fields in a struct variant have names"),
                                field,
                            )
                        })
                        .exactly_one()
                    {
                        Ok((ident, field)) => create_flag!(
                            auto_long,
                            auto_short,
                            source: &ident,
                            info: PlainFlagInfo::Struct(FlagFieldInfo {
                                ty: &field.ty, ident,
                            }),
                            attrs: &field.attrs,
                        )
                        .map(FlagSetVariant::Plain),

                        Err(fields) => fields
                            .map(|(ident, field)| {
                                create_flag!(
                                    auto_long,
                                    auto_short,
                                    source: &ident,
                                    info: FlagFieldInfo {
                                        ident, ty: &field.ty,
                                    },
                                    attrs: &field.attrs,
                                )

                                // TODO: collision detection for flags within the
                                // same struct.
                            })
                            .try_collect()
                            .map(FlagSetVariant::Struct),
                    },
                };

                mode.map(|mode| (variant_ident, mode))
            })
            .try_collect()?;

        let superposition = create_non_colliding_ident("Superposition", variants.keys());

        Ok(Self {
            superposition,
            variants,
        })
    }
}

pub enum FlagSetFlagSite<'a> {
    Unit,
    Newtype,
    Field(&'a IdentString<'a>),
}

impl<'a> FlagSetFlagSite<'a> {
    pub fn field(&self) -> Option<&'a IdentString<'a>> {
        match self {
            Self::Field(ident) => Some(ident),
            Self::Unit | Self::Newtype => None,
        }
    }
}

/// For a given global flag, this includes information about one of the
/// variants attached to that flag.
pub struct FlagSetFlagVariant<'a> {
    /// The name of a variant this flag belongs to.
    pub ident: &'a IdentString<'a>,

    /// The index of this *variant*. Used in viability set calculations.
    pub index: usize,

    /// The location of this flag within this variant. Used in state initializers.
    pub field_index: usize,

    /// The location of this flag within this variant, as an identifier.
    pub site: FlagSetFlagSite<'a>,
}

pub struct FlagSetFlag<'a> {
    /// The first place this flag appears; either the variant name or the field
    /// name
    pub origin: &'a IdentString<'a>,

    /// All of the variants where this flag appears, along with the index
    /// of its location in that variant's state, and (if relevant) the name
    /// of the
    pub variants: Vec<FlagSetFlagVariant<'a>>,

    /// Documentation for this flag. If the flag appears more than once in
    /// different variants, we choose the longest description.
    pub docs: &'a Description,

    /// Placeholder for the flag. Must be identical in all instances.
    pub placeholder: &'a str,

    /// Type of the flag (or Unit, for a unit enum). We don't check consistency
    /// between types; we rely on the compilers own type-checking to both
    /// check the type and produce a better error message about the wrong
    /// type
    pub ty: FlagType<'a>,

    /// Computed tags for this flag. Possibly enabled by a debate attribute
    /// on the enum itself, in addition to the flag
    pub tags: FlagTags<&'a str, char>,

    /// If true, subsequent instances of this flag override earlier ones.
    /// this setting must be identical for all instances.
    pub overridable: bool,

    /// If true, there is at least one variant of this flag that does *not*
    /// have a default specified. This means that it's possible for a
    /// requirement error to cite this flag.
    pub ever_required: bool,

    /// Record if there are *any* solitary instances of this flag. This is
    /// used to prevent more than one solitary instance of the flag being
    /// created. This includes the span of the variant that produced the
    /// solitary version of this flag, for error reporting purposes.
    pub family: Family<&'a IdentString<'a>>,
}

impl<'a> FlagSetFlag<'a> {
    /// If this flag is associated with exactly 1 variant, get that variant.
    /// Unique flags don't need to have their state recorded when they're in
    /// superposition, because the arrival of a unique flag always takes us
    /// directly to this specific state variant.
    #[inline(always)]
    #[must_use]
    pub fn unique_variant(&self) -> Option<&FlagSetFlagVariant<'a>> {
        self.variants.iter().exactly_one().ok()
    }
}

enum TagComparison {
    Match,
    Different,
    LongMismatch,
    ShortMismatch,
}

fn compare_tags(left: &FlagTags<&str, char>, right: &FlagTags<&str, char>) -> TagComparison {
    use FlagTags::*;

    match (*left, *right) {
        // Tags are identical
        (Long(left), Long(right)) if left == right => TagComparison::Match,
        (Short(left), Short(right)) if left == right => TagComparison::Match,
        (
            LongShort {
                long: l1,
                short: s1,
            },
            LongShort {
                long: l2,
                short: s2,
            },
        ) if l1 == l2 && s1 == s2 => TagComparison::Match,

        // Partial mismatch is an error (long matches, short differs)
        (Long(left), LongShort { long: right, .. })
        | (LongShort { long: left, .. }, Long(right))
        | (LongShort { long: left, .. }, LongShort { long: right, .. })
            if left == right =>
        {
            TagComparison::ShortMismatch
        }

        // Partial mismatch is an error (short matches, long differs)
        (Short(left), LongShort { short: right, .. })
        | (LongShort { short: left, .. }, Short(right))
        | (LongShort { short: left, .. }, LongShort { short: right, .. })
            if left == right =>
        {
            TagComparison::LongMismatch
        }

        // The tags are entirely different
        _ => TagComparison::Different,
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
///
/// Returns the index of the added or updated flag.
fn add_or_update_flag<'a, Info>(
    set: &mut Vec<FlagSetFlag<'a>>,
    variant_flag: &'a FlagSetFlagInfo<Info>,
    family: Family<()>,
    variant: &'a IdentString<'a>,
    variant_index: usize,
    field_index: usize,
) -> syn::Result<usize>
where
    Info: FlagSetFlagExtra<'a>,
{
    let origin = variant_flag.info.field().unwrap_or(variant);
    let tags = variant_flag.tags.simplify();

    let mismatch_error = |existing_span, message| {
        error_pair(
            origin.span(),
            "multiple instances of the same tag must be (mostly) identical",
            existing_span,
            message,
        )
    };

    let existing_flag = try_find(set.iter_mut().enumerate(), |(_, existing)| {
        let mismatch_error = |message| mismatch_error(existing.origin.span(), message);

        match compare_tags(&tags, &existing.tags) {
            TagComparison::LongMismatch => {
                Err(mismatch_error("this instance has a different --long"))
            }
            TagComparison::ShortMismatch => {
                Err(mismatch_error("this instance has a different -s short"))
            }

            TagComparison::Different => Ok(false),
            TagComparison::Match => {
                if let (Solitary(()), Solitary(existing_variant)) = (family, existing.family) {
                    Err(error_pair(
                        variant.span(),
                        "this variant will never be selected (another variant has the same flags)",
                        existing_variant.span(),
                        "this variant's flag is identical",
                    ))
                } else if variant_flag.placeholder.as_str() != existing.placeholder {
                    // Nit: placeholders are just a docs convention. In theory
                    // we should be able to detect and use a custom placeholder
                    // and reject only distinctions between custom placeholders.
                    Err(mismatch_error("this instance has a different placeholder"))
                } else if variant_flag.overridable.is_some() != existing.overridable {
                    Err(mismatch_error(
                        "this instance has a different `override` setting",
                    ))
                } else {
                    Ok(true)
                }
            }
        }
    })?;

    let (flag_index, existing_flag) = match existing_flag {
        Some(flag) => flag,
        None => {
            let index = set.len();

            set.push(FlagSetFlag {
                origin,
                variants: Vec::new(),
                docs: const { &Description::empty() },
                placeholder: &variant_flag.placeholder,
                ty: variant_flag.info.as_flag_set_type(),
                tags,
                overridable: variant_flag.overridable.is_some(),
                family: Family::Sibling,
                ever_required: false,
            });

            (
                index,
                set.last_mut().expect("we just pushed an item into the set"),
            )
        }
    };

    existing_flag.variants.push(FlagSetFlagVariant {
        ident: variant,
        index: variant_index,
        field_index,
        site: variant_flag.info.site(),
    });
    existing_flag.family.merge(family, variant);
    existing_flag.ever_required = existing_flag.ever_required || variant_flag.default.is_none();

    if existing_flag.docs.full.len() > variant_flag.docs.full.len() {
        existing_flag.docs = &variant_flag.docs;
    };

    Ok(flag_index)
}

pub struct VariantFieldSource<'a, Info> {
    /// The location, in the GLOBAL flags state list, where this flag can be
    /// found
    pub index: usize,

    /// Is this flag unique (that, does this flag ONLY appear in this variant).
    /// Affects code gen.
    pub unique: bool,

    /// Everything else interesting about this flag as it exists on the variant
    pub field: &'a FlagSetFlagInfo<Info>,
}

pub enum VariantFieldSourcesMode<'a> {
    Plain(VariantFieldSource<'a, PlainFlagInfo<'a>>),
    Struct(Vec<VariantFieldSource<'a, FlagFieldInfo<'a>>>),
}

pub struct VariantFieldSources<'a> {
    pub mode: VariantFieldSourcesMode<'a>,
    pub reachable: bool,
}

/// Given the parsed variants, create the flat, deduplicated list of flags
/// within those variants. This is where various consistency checks happen
/// (duplicate flags must be mostly identical) and where things like the
/// exclusion sets are calculated.
pub fn compute_grouped_flags<'a>(
    variants: &'a IndexMap<IdentString<'a>, FlagSetVariant<'a>>,
) -> syn::Result<(
    IndexMap<&'a IdentString<'a>, VariantFieldSources<'a>>,
    Vec<FlagSetFlag<'a>>,
)> {
    let mut flags = Vec::new();

    let mut sources: IndexMap<&'a IdentString<'a>, VariantFieldSources<'a>> = variants
        .iter()
        .enumerate()
        .map(|(variant_index, (variant_ident, variant))| {
            match variant {
                FlagSetVariant::Plain(flag) => add_or_update_flag(
                    &mut flags,
                    flag,
                    Family::Solitary(()),
                    variant_ident,
                    variant_index,
                    0,
                )
                .map(|flag_index| VariantFieldSource {
                    index: flag_index,
                    field: flag,
                    unique: false,
                })
                .map(VariantFieldSourcesMode::Plain),
                FlagSetVariant::Struct(fields) => fields
                    .iter()
                    .enumerate()
                    .map(|(field_index, flag)| {
                        add_or_update_flag(
                            &mut flags,
                            flag,
                            Family::Sibling,
                            variant_ident,
                            variant_index,
                            field_index,
                        )
                        .map(|field_index| VariantFieldSource {
                            index: field_index,
                            field: flag,
                            unique: false,
                        })
                    })
                    .try_collect()
                    .map(VariantFieldSourcesMode::Struct),
            }
            .map(|mode| VariantFieldSources {
                mode,
                reachable: false,
            })
            .map(|sources| (variant_ident, sources))
        })
        .try_collect()?;

    // Update all the sources with reachability. Needed to precompute flags,
    // to verify which flags only have one possible variant.
    for (index, flag) in flags.iter().enumerate() {
        if let Some(unique_variant) = flag.unique_variant() {
            let source = sources
                .get_mut(unique_variant.ident)
                .expect("all source variants should exist");

            source.reachable = true;
            match source.mode {
                VariantFieldSourcesMode::Plain(ref mut source) => source.unique = true,
                VariantFieldSourcesMode::Struct(ref mut sources) => {
                    sources
                        .iter_mut()
                        .find(|source| source.index == index)
                        .expect("source flag must exist here")
                        .unique = true
                }
            }
        }
    }

    Ok((sources, flags))
}
