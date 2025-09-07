pub mod enumeration;
pub mod value;

use std::{borrow::Cow, sync::OnceLock};

use darling::{
    FromAttributes as _,
    util::{Override, SpannedValue},
};
use heck::{ToKebabCase as _, ToShoutySnakeCase, ToTitleCase};
use itertools::Itertools as _;
use proc_macro2::TokenStream as TokenStream2;
use quote::ToTokens;
use regex::Regex;
use syn::{Attribute, Expr, Field, Ident, Type, spanned::Spanned as _};

macro_rules! regex {
    ($pattern:literal) => {{
        static REGEX: OnceLock<Regex> = OnceLock::new();
        REGEX.get_or_init(|| Regex::new($pattern).expect("failed to compile regex"))
    }};
}

/// A borrowed identifier that also has its string pre-computed for easy reuse.
pub struct IdentString<'a> {
    raw: &'a Ident,
    string: String,
}

impl<'a> IdentString<'a> {
    pub fn new(ident: &'a Ident) -> Self {
        Self {
            string: ident.to_string(),
            raw: ident,
        }
    }

    pub fn as_str(&self) -> &str {
        self.string.as_str()
    }

    pub fn raw(&self) -> &'a Ident {
        self.raw
    }
}

impl ToTokens for IdentString<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.raw.to_tokens(tokens);
    }
}

pub enum FlattenOr<F, T> {
    Flatten(F),
    Normal(T),
}

/// Attributes for a field. "Raw" because it doesn't yet do various
/// correctness checks, such as ensuring `flatten` is unique, or that `invert`
/// only appears on flags.
#[derive(darling::FromAttributes, Debug)]
#[darling(attributes(debate))]
struct RawParsedFieldAttr {
    long: Option<Override<SpannedValue<String>>>,
    short: Option<Override<SpannedValue<char>>>,
    default: Option<Override<Expr>>,
    placeholder: Option<SpannedValue<String>>,
    // invert = "no-verbose"
    // override

    // TODO: add a parse step where `flatten` must not coexist with the other
    // variants. Consider switching from `darling` to `deluxe`, which apparently
    // handles this.
    flatten: Option<()>,
}

/// Setting for a default for a field
pub enum FieldDefault {
    /// Use `Default::default` to populate this field if it was absent
    Trait,

    /// Use this expression to populate the field if it was absent
    Expr(Expr),
}

impl FieldDefault {
    #[inline]
    #[must_use]
    pub fn new(default: Option<Override<Expr>>) -> Option<Self> {
        default.map(|default| match default {
            Override::Explicit(default) => Self::Expr(default),
            Override::Inherit => Self::Trait,
        })
    }
}

pub enum FlagTags<Long, Short> {
    Long(Long),
    Short(Short),
    LongShort { long: Long, short: Short },
}

impl FlagTags<SpannedValue<String>, SpannedValue<char>> {
    #[inline]
    #[must_use]
    pub fn long(&self) -> Option<SpannedValue<&str>> {
        match *self {
            FlagTags::Long(ref long) | FlagTags::LongShort { ref long, .. } => {
                Some(SpannedValue::new(long.as_str(), long.span()))
            }
            FlagTags::Short(_) => None,
        }
    }

    #[inline]
    #[must_use]
    pub fn short(&self) -> Option<SpannedValue<char>> {
        match *self {
            FlagTags::Short(short) | FlagTags::LongShort { short, .. } => Some(short),
            FlagTags::Long(_) => None,
        }
    }

    // Shed the spanned value stuff if we don't need it
    #[inline]
    #[must_use]
    pub fn simplify(&self) -> FlagTags<&str, char> {
        match self {
            FlagTags::Long(long) => FlagTags::Long(long.as_str()),
            FlagTags::Short(short) => FlagTags::Short(**short),
            FlagTags::LongShort { long, short } => FlagTags::LongShort {
                long: long.as_str(),
                short: **short,
            },
        }
    }
}

#[derive(Default)]
pub struct Description {
    pub short: String,
    pub long: String,
}

pub struct PositionalFieldInfo<'a> {
    pub ident: IdentString<'a>,

    /// The placeholder for this field, shown to the user in usage messages.
    /// usually all caps.
    pub placeholder: SpannedValue<String>,
    pub ty: &'a Type,
    pub default: Option<FieldDefault>,
    pub docs: Description,
}

pub struct FlagFieldInfo<'a> {
    /// Identifier for this field
    pub ident: IdentString<'a>,

    /// The placeholder for this field, shown to the user in usage messages.
    pub placeholder: SpannedValue<String>,

    /// Type of this field
    pub ty: &'a Type,

    /// Default value setting (either to use the `Default` trait, a specific
    /// expression, or none)
    pub default: Option<FieldDefault>,
    pub docs: Description,
    pub tags: FlagTags<SpannedValue<String>, SpannedValue<char>>,
}

pub struct FlattenFieldInfo<'a> {
    /// Documentation for this field
    pub docs: Description,

    /// Identifier for this field
    pub ident: IdentString<'a>,

    /// Printed group name used for this field
    pub title: SpannedValue<String>,

    /// When this entire field is used as a placeholder (basically just for
    /// subcommands, but could be useful later)
    pub placeholder: SpannedValue<String>,

    /// Type of this field
    pub ty: &'a Type,
}

pub fn compute_docs(attrs: &[Attribute]) -> syn::Result<Description> {
    let body: String = attrs
        .iter()
        .filter_map(|attr| match attr.meta {
            syn::Meta::NameValue(ref meta) => Some(meta),
            _ => None,
        })
        .filter(|meta| meta.path.is_ident("doc"))
        .map(|meta| match meta.value {
            Expr::Lit(ref lit) => match lit.lit {
                syn::Lit::Str(ref lit) => Ok(lit.value()),
                _ => Err(syn::Error::new(meta.span(), "malformed #[doc] attribute")),
            },
            Expr::Macro(ref expr) => Err(syn::Error::new(
                expr.span(),
                "macro #[doc] attributes aren't supported",
            )),
            _ => Err(syn::Error::new(meta.span(), "malformed #[doc] attribute")),
        })
        .map_ok(|mut doc| {
            doc.push('\n');
            doc
        })
        // Don't bother trying to handle lines separately, because we might
        // be getting docs from many attributes (///) or just one (/* */).
        // Always concatenate and figure it out from there.
        .try_collect()?;

    // textwrap doesn't support multiple paragraphs, so we need to separate it
    // ourselves.

    // Preserve leading whitespace, but not newlines
    let leading_whitespace = regex!("^[\t\n ]*\n");

    // Paragraphs are separated by at least two newlines, with any amount of
    // whitespace in between.
    let paragraph_separator = regex!("[\t ]*\n[*\t\n ]*\n");

    let body = body.trim_end();
    let body = leading_whitespace.replace(body, "");

    let mut paragraphs = paragraph_separator.split(&body).map(|paragraph| {
        // This is basically `textwrap::refill`, except that `refill` takes
        // care to preserve line prefixes, and we explicitly are trying not
        // to do that.
        let (unfilled, _) = textwrap::unfill(paragraph);

        // One cute thing we get to do is wrap to 80 actual real columns of
        // text, because indentation is handled separately.
        textwrap::fill(&unfilled, 80)
    });

    let Some(first) = paragraphs.next() else {
        return Ok(Description::default());
    };

    let short = first.clone();
    let long = paragraphs.fold(first, |mut body, paragraph| {
        body.reserve(paragraph.len() + 2);
        body.push_str("\n\n");
        body.push_str(&paragraph);
        body
    });

    Ok(Description { short, long })
}

pub enum ParsedFieldInfo<'a> {
    Positional(PositionalFieldInfo<'a>),
    Option(FlagFieldInfo<'a>),
    Flatten(FlattenFieldInfo<'a>),
}

impl<'a> ParsedFieldInfo<'a> {
    pub fn from_field(field: &'a Field) -> syn::Result<Self> {
        let parsed = RawParsedFieldAttr::from_attributes(&field.attrs)?;
        let docs = compute_docs(&field.attrs)?;

        let ty = &field.ty;
        let ident = IdentString::new(field.ident.as_ref().ok_or_else(|| {
            syn::Error::new(
                field.span(),
                "can't use anonymous fields in debate derives. This should \
                have been detected already, it's probably a bug in `debate`.",
            )
        })?);

        // TODO: enforce that flatten doesn't coexist with other variants.
        if let Some(()) = parsed.flatten {
            let name = ident.as_str();

            let group_name = SpannedValue::new(
                match name.ends_with("command") {
                    true => Cow::Owned(format!("{name}s")),
                    false => Cow::Borrowed(name),
                }
                .to_title_case(),
                ident.span(),
            );

            let placeholder = SpannedValue::new(name.to_shouty_snake_case(), ident.span());

            return Ok(Self::Flatten(FlattenFieldInfo {
                ident,
                ty,
                title: group_name,
                placeholder,
                docs,
            }));
        }

        let long = parsed
            .long
            .map(|long| compute_long(long.explicit(), &ident))
            .transpose()?;

        let short = parsed
            .short
            .map(|short| compute_short(short.explicit(), &ident))
            .transpose()?;

        let placeholder = compute_placeholder(parsed.placeholder, &ident)?;

        let default = FieldDefault::new(parsed.default);

        Ok(
            match match (long, short) {
                (None, None) => None,
                (Some(long), None) => Some(FlagTags::Long(long)),
                (None, Some(short)) => Some(FlagTags::Short(short)),
                (Some(long), Some(short)) => Some(FlagTags::LongShort { long, short }),
            } {
                None => Self::Positional(PositionalFieldInfo {
                    ident,
                    placeholder,
                    ty,
                    default,
                    docs,
                }),
                Some(tags) => Self::Option(FlagFieldInfo {
                    ident,
                    placeholder,
                    ty,
                    default,
                    docs,
                    tags,
                }),
            },
        )
    }

    pub fn get_positional(
        &self,
    ) -> Option<FlattenOr<&FlattenFieldInfo<'a>, &PositionalFieldInfo<'a>>> {
        match self {
            ParsedFieldInfo::Positional(info) => Some(FlattenOr::Normal(info)),
            ParsedFieldInfo::Flatten(info) => Some(FlattenOr::Flatten(info)),
            ParsedFieldInfo::Option(_) => None,
        }
    }
}

fn compute_long(
    long: Option<SpannedValue<String>>,
    field_name: &IdentString<'_>,
) -> syn::Result<SpannedValue<String>> {
    let long = long.unwrap_or_else(|| {
        SpannedValue::new(field_name.as_str().to_kebab_case(), field_name.span())
    });

    if long.starts_with("--") {
        Err(syn::Error::new(
            long.span(),
            "long parameters don't need to start with --; this is handled automatically",
        ))
    } else if long.starts_with('-') {
        Err(syn::Error::new(
            long.span(),
            "long parameters don't start with '-'",
        ))
    } else if !long.starts_with(|c: char| c.is_alphabetic()) {
        Err(syn::Error::new(
            long.span(),
            "long parameters should start with something alphabetic. This might be relaxed later.",
        ))
    } else if long.contains('=') {
        Err(syn::Error::new(
            long.span(),
            "long parameters must not include an '=', as it is the argument separator",
        ))
    } else if long.contains(|c: char| c.is_whitespace()) {
        Err(syn::Error::new(
            long.span(),
            "long parameters shouldn't include whitespace",
        ))
    } else {
        Ok(long)
    }
}

fn compute_short(
    short: Option<SpannedValue<char>>,
    field_name: &IdentString<'_>,
) -> syn::Result<SpannedValue<char>> {
    let c = short.unwrap_or_else(|| {
        // TODO: use a capital char if the lower case one is taken already
        SpannedValue::new(
            field_name
                .as_str()
                .chars()
                .next()
                .expect("Identifiers can't be empty"),
            field_name.span(),
        )
    });

    if *c == '-' {
        Err(syn::Error::new(c.span(), "short parameter must not be '-'"))
    } else if !c.is_ascii_graphic() {
        Err(syn::Error::new(
            c.span(),
            "short parameter should be an ascii printable",
        ))
    } else {
        Ok(c)
    }
}

fn compute_placeholder(
    placeholder: Option<SpannedValue<String>>,
    field_name: &IdentString<'_>,
) -> syn::Result<SpannedValue<String>> {
    let placeholder = placeholder.unwrap_or_else(|| {
        SpannedValue::new(
            field_name.as_str().to_shouty_snake_case(),
            field_name.span(),
        )
    });

    if placeholder.contains(|c: char| c.is_whitespace()) {
        Err(syn::Error::new(
            placeholder.span(),
            "placeholder shouldn't include whitespace",
        ))
    } else {
        Ok(placeholder)
    }
}

/// Attributes for a type. In the future this may split into separate types for
#[derive(darling::FromAttributes, Debug)]
#[darling(attributes(debate))]
pub struct RawParsedTypeAttr {
    help: Option<()>,
    // TODO: global long/short
}

impl RawParsedTypeAttr {
    pub fn help_option(&self) -> HelpOption<'_> {
        HelpOption {
            long: self.help.map(|()| "help"),
            short: self.help.map(|()| 'h'),
        }
    }
}

/// The set of flags that will signal a request for help
pub struct HelpOption<'a> {
    pub long: Option<&'a str>,
    pub short: Option<char>,
}

impl<'a> HelpOption<'a> {
    pub const fn new() -> Self {
        Self {
            long: None,
            short: None,
        }
    }

    pub const fn as_tags(&self) -> Option<FlagTags<&'a str, char>> {
        match (self.long, self.short) {
            (None, None) => None,
            (Some(long), None) => Some(FlagTags::Long(long)),
            (None, Some(short)) => Some(FlagTags::Short(short)),
            (Some(long), Some(short)) => Some(FlagTags::LongShort { long, short }),
        }
    }
}
