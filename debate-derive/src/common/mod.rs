pub mod enumeration;
pub mod value;

use std::{borrow::Cow, fmt::Display, sync::OnceLock};

use darling::{
    FromAttributes as _,
    util::{Override, SpannedValue},
};
use heck::{ToKebabCase as _, ToShoutySnakeCase, ToTitleCase};
use itertools::Itertools as _;
use proc_macro2::{Span, TokenStream as TokenStream2};
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
#[derive(Clone, Eq)]
pub struct IdentString<'a> {
    raw: &'a Ident,
    string: String,
}

impl<'a> IdentString<'a> {
    #[must_use]
    pub fn new(ident: &'a Ident) -> Self {
        Self {
            string: ident.to_string(),
            raw: ident,
        }
    }

    #[inline]
    #[must_use]
    pub fn as_str(&self) -> &str {
        self.string.as_str()
    }

    #[inline]
    #[must_use]
    pub fn raw(&self) -> &'a Ident {
        self.raw
    }

    #[inline]
    #[must_use]
    pub fn as_spanned_str(&self) -> SpannedValue<&str> {
        SpannedValue::new(self.string.as_str(), self.raw.span())
    }
}

impl ToTokens for IdentString<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.raw.to_tokens(tokens);
    }
}

impl PartialEq<str> for IdentString<'_> {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl PartialEq<IdentString<'_>> for str {
    fn eq(&self, other: &IdentString<'_>) -> bool {
        self == other.as_str()
    }
}

impl PartialEq<IdentString<'_>> for IdentString<'_> {
    fn eq(&self, other: &IdentString<'_>) -> bool {
        self.as_str() == other.as_str()
    }
}

impl std::hash::Hash for IdentString<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state);
    }
}

pub enum FlattenOr<F, T> {
    Flatten(F),
    Normal(T),
}

/// Attributes for a field. "Raw" because it doesn't yet do various
/// correctness checks, such as ensuring `flatten` is unique, or that `invert`
/// only appears on flags.
#[derive(darling::FromAttributes)]
#[darling(attributes(debate))]
struct RawParsedFieldAttr {
    long: Option<SpannedValue<Override<String>>>,
    short: Option<SpannedValue<Override<char>>>,
    default: Option<SpannedValue<Override<Expr>>>,
    placeholder: Option<SpannedValue<String>>,
    invert: Option<SpannedValue<Override<String>>>,
    #[darling(rename = "override")]
    r#override: Option<SpannedValue<()>>,
    flatten: Option<SpannedValue<()>>,
}

macro_rules! reject_some {
    ($message:literal; $($candidate:expr $(,)?)+) => {
        $(
            if let Some(ref value) = $candidate {
                Err(syn::Error::new(value.span(), $message))
            } else
        )+ { Ok(()) }
    };
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
    pub fn new(default: Option<SpannedValue<Override<Expr>>>) -> Option<Self> {
        default.map(|default| match default.into_inner() {
            Override::Explicit(default) => Self::Expr(default),
            Override::Inherit => Self::Trait,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FlagTags<Long, Short> {
    Long(Long),
    Short(Short),
    LongShort { long: Long, short: Short },
}

impl<'a> FlagTags<&'a str, char> {
    #[inline]
    #[must_use]
    pub fn long(&self) -> Option<&'a str> {
        match *self {
            FlagTags::Long(long) | FlagTags::LongShort { long, .. } => Some(long),
            FlagTags::Short(_) => None,
        }
    }

    #[inline]
    #[must_use]
    pub fn short(&self) -> Option<char> {
        match *self {
            FlagTags::Short(short) | FlagTags::LongShort { short, .. } => Some(short),
            FlagTags::Long(_) => None,
        }
    }
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
    pub fn simplify(&self) -> FlagTags<&'_ str, char> {
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

pub struct Description {
    pub succinct: String,
    pub full: String,
}

impl Description {
    pub const fn empty() -> Self {
        Self {
            succinct: String::new(),
            full: String::new(),
        }
    }
}

pub struct PositionalFieldInfo<'a> {
    pub ident: IdentString<'a>,
    pub docs: Description,

    /// The placeholder for this field, shown to the user in usage messages.
    /// usually all caps.
    pub placeholder: SpannedValue<String>,
    pub ty: &'a Type,
    pub default: Option<FieldDefault>,
}

// NOTE: I doubt it matters, but just in case, I'm trying to keep the
// common fields between this and `PositionalFieldInfo` in the same order,
// because it might help optimize field access out of the `ParsedFieldInfo`
// enum.
pub struct FlagFieldInfo<'a> {
    /// Identifier for this field
    pub ident: IdentString<'a>,
    pub docs: Description,

    /// The placeholder for this field, shown to the user in usage messages.
    pub placeholder: SpannedValue<String>,

    /// Type of this field
    pub ty: &'a Type,

    /// Default value setting (either to use the `Default` trait, a specific
    /// expression, or none)
    pub default: Option<FieldDefault>,

    pub tags: FlagTags<SpannedValue<String>, SpannedValue<char>>,

    /// If given, we should also create a --invert tag with this name.
    pub invert: Option<SpannedValue<String>>,

    /// If given, subsequent uses of this flag override earlier ones.
    pub overridable: bool,
}

pub struct FlattenFieldInfo<'a> {
    /// Identifier for this field
    pub ident: IdentString<'a>,

    /// Documentation for this field
    pub docs: Description,

    /// When this entire field is used as a placeholder (basically just for
    /// subcommands, but could be useful later)
    pub placeholder: SpannedValue<String>,

    /// Type of this field
    pub ty: &'a Type,

    /// Printed group name used for this field
    pub title: SpannedValue<String>,
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
        return Ok(Description::empty());
    };

    let short = first.clone();
    let long = paragraphs.fold(first, |mut body, paragraph| {
        body.reserve(paragraph.len() + 2);
        body.push_str("\n\n");
        body.push_str(&paragraph);
        body
    });

    Ok(Description {
        succinct: short,
        full: long,
    })
}

pub enum ParsedFieldInfo<'a> {
    Positional(PositionalFieldInfo<'a>),
    Flag(FlagFieldInfo<'a>),
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

        if parsed.flatten.is_some() {
            // FIXME: find a less brittle way to enforce the valid mutually
            // permissible sets here.
            reject_some!(
                "`flatten` is incompatible with other attributes";
                parsed.long, parsed.short, parsed.default, parsed.placeholder,
                parsed.invert, parsed.r#override,
            )?;

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

        let placeholder = compute_placeholder(parsed.placeholder, &ident)?;
        let default = FieldDefault::new(parsed.default);

        if let Some(tags) = compute_tags(parsed.long, parsed.short, &ident)? {
            let invert = parsed
                .invert
                .map(|invert| {
                    let long = tags.long().ok_or_else(|| {
                        syn::Error::new(
                            invert.span(),
                            "#[debate(invert)] requires either a --long tag \
                            (to prepend with --no-long) or a manual name \
                            (such as invert=\"quiet\"",
                        )
                    })?;

                    compute_invert(unpack_spanned_override(invert), long)
                })
                .transpose()?;

            Ok(Self::Flag(FlagFieldInfo {
                ident,
                placeholder,
                ty,
                default,
                docs,
                invert,
                overridable: parsed.r#override.is_some(),
                tags,
            }))
        } else {
            reject_some!(
                "`invert` can only be used with flags, not positionals";
                parsed.invert
            )?;

            reject_some!(
                "`override` can only be used with flags, not positionals";
                parsed.r#override
            )?;

            Ok(Self::Positional(PositionalFieldInfo {
                ident,
                placeholder,
                ty,
                default,
                docs,
            }))
        }
    }

    pub fn get_positional(
        &self,
    ) -> Option<FlattenOr<&FlattenFieldInfo<'a>, &PositionalFieldInfo<'a>>> {
        match self {
            ParsedFieldInfo::Positional(info) => Some(FlattenOr::Normal(info)),
            ParsedFieldInfo::Flatten(info) => Some(FlattenOr::Flatten(info)),
            ParsedFieldInfo::Flag(_) => None,
        }
    }
}

/// We use `SpannedValue<Override<T>>` in several places, because it's useful
/// for error reporting to record the span of either the argument or (if there
/// was no argument) the naked key. Function simplifies this down to an
/// optional value, with the span attached to the value.
#[inline]
#[must_use]
fn unpack_spanned_override<T>(value: SpannedValue<Override<T>>) -> Option<SpannedValue<T>> {
    let span = value.span();

    match value.into_inner() {
        Override::Inherit => None,
        Override::Explicit(value) => Some(SpannedValue::new(value, span)),
    }
}

/// A common pattern is to have attributes resembling: `#[debate(short, long=foo)`.
/// In the case of `short`, where the key is present but without a value,
/// we usually derive the value from the field name. This function handles that
/// logic, and additionally validates the value (whether user-provided or
/// computed).
///
/// If the value ends up derived from the field identifier, the span is set
/// to that identifier.
fn derive_from_field_name<T>(
    user_value: Option<SpannedValue<T>>,
    field_name: SpannedValue<&str>,
    from_field_name: impl FnOnce(&str) -> T,
    check: impl FnOnce(&T) -> Result<(), &'static str>,
) -> syn::Result<SpannedValue<T>> {
    let value = user_value
        .unwrap_or_else(|| SpannedValue::new(from_field_name(&field_name), field_name.span()));

    match check(&value) {
        Ok(()) => Ok(value),
        Err(message) => Err(syn::Error::new(value.span(), message)),
    }
}

/// Helper macro that checks each input expression and creates an error with
/// a message if any fail.
macro_rules! checks {
    ($($check:expr => $message:literal),+ $(,)?) => {
        $(
            if $check { Err($message) } else
        )+
        { Ok(()) }
    }
}

#[inline]
fn compute_long(
    long: Option<SpannedValue<String>>,
    field_name: &IdentString<'_>,
) -> syn::Result<SpannedValue<String>> {
    derive_from_field_name(
        long,
        field_name.as_spanned_str(),
        |field| field.to_kebab_case(),
        |long| {
            checks! {
                long.starts_with("--") =>
                    "long parameters don't need to start with --; this is \
                    handled automatically",
                long.starts_with('-') =>
                    "long parameters don't start with '-'",
                !long.starts_with(|c: char| c.is_alphabetic()) =>
                    "long parameters should start with something alphabetic. \
                    This might be relaxed later.",
                long.contains('=') =>
                    "long parameters must not include an '=', as it is the \
                    argument separator",
                long.contains(|c: char| c.is_whitespace()) =>
                    "long parameters shouldn't include whitespace",
            }
        },
    )
}

#[inline]
fn compute_short(
    short: Option<SpannedValue<char>>,
    field_name: &IdentString<'_>,
) -> syn::Result<SpannedValue<char>> {
    derive_from_field_name(
        short,
        field_name.as_spanned_str(),
        |field| {
            field
                .chars()
                .next()
                .expect("Identifiers can't be empty")
                .to_ascii_lowercase()
        },
        |short| {
            checks! {
                *short == '-' => "short parameter must not be '-'",
                !short.is_ascii_graphic() => "short parameter should be an ascii printable",
            }
        },
    )
}

pub fn compute_tags(
    long: Option<SpannedValue<Override<String>>>,
    short: Option<SpannedValue<Override<char>>>,
    ident: &IdentString<'_>,
) -> syn::Result<Option<FlagTags<SpannedValue<String>, SpannedValue<char>>>> {
    let long = long
        .map(|long| compute_long(unpack_spanned_override(long), ident))
        .transpose()?;

    let short = short
        .map(|short| compute_short(unpack_spanned_override(short), ident))
        .transpose()?;

    Ok(match (long, short) {
        (None, None) => None,
        (Some(long), None) => Some(FlagTags::Long(long)),
        (None, Some(short)) => Some(FlagTags::Short(short)),
        (Some(long), Some(short)) => Some(FlagTags::LongShort { long, short }),
    })
}

#[inline]
fn compute_placeholder(
    placeholder: Option<SpannedValue<String>>,
    field_name: &IdentString<'_>,
) -> syn::Result<SpannedValue<String>> {
    derive_from_field_name(
        placeholder,
        field_name.as_spanned_str(),
        |field| field.to_shouty_snake_case(),
        |placeholder| {
            checks! {
                placeholder.contains(|c: char| c.is_whitespace()) => "placeholder shouldn't include whitespace",
            }
        },
    )
}

fn strip_no(tag: &str) -> Option<&str> {
    let (suffix, may_omit_separator) = if let Some(suffix) = tag.strip_suffix("no") {
        (suffix, true)
    } else if let Some(suffix) = tag.strip_suffix("No") {
        (suffix, true)
    } else if let Some(suffix) = tag.strip_suffix("NO") {
        (suffix, false)
    } else {
        return None;
    };

    if let Some(suffix) = suffix.strip_prefix(['-', '_']) {
        Some(suffix)
    } else if may_omit_separator && suffix.starts_with(|c: char| !c.is_lowercase()) {
        Some(suffix)
    } else {
        None
    }
}

#[inline]
fn compute_invert(
    // If given, the user's preference for the flag. If omitted, we'll compute
    // it by prefixing the `long` tag with `no-` (or, if it has a `no-` already,
    // by stripping it)
    invert: Option<SpannedValue<String>>,

    // The --long tag of the flag being modified
    long: SpannedValue<&str>,
) -> syn::Result<SpannedValue<String>> {
    derive_from_field_name(
        invert,
        long,
        |long| {
            // First, try to strip a `no` prefix. There aren't many variations
            // here, so we can pretty much just try all of them.
            if let Some(stripped) = strip_no(long) {
                return stripped.to_owned();
            }

            // Rudimentary case detection
            let underscore = long.contains('_');
            let dash = long.contains('-');
            let upper = long.contains(|c: char| c.is_uppercase());
            let lower = long.contains(|c: char| c.is_lowercase());

            let word = match (upper, lower) {
                (false, _) => "no",
                (true, true) => "No",
                (true, false) => "NO",
            };

            let mark = match (dash, underscore) {
                (true, _) => "-",
                (false, true) => "_",
                // SeemsLikeCamelCase
                (false, false) if lower && upper => "",
                (false, false) => "-",
            };

            format!("{word}{mark}{long}")
        },
        // We assume any relevant checks already happened to the long tag.
        // We still have to do duplicate detection, but that happens separately.
        |_| Ok(()),
    )
}

/// Attributes for a type. In the future this may split into separate types for
/// the different types we can tag.
#[derive(darling::FromAttributes, Debug)]
#[darling(attributes(debate))]
pub struct RawParsedTypeAttr {
    help: Option<SpannedValue<()>>,
    // TODO: global long/short
    // TODO: rename attribute to rename the command
}

impl RawParsedTypeAttr {
    pub fn help_option(&self) -> Option<HelpFlag<'_>> {
        self.help.as_ref().map(|help| HelpFlag {
            tags: FlagTags::LongShort {
                long: "help",
                short: 'h',
            },
            span: help.span(),
        })
    }
}

/// The set of flags that will signal a request for help
// This should probably become a pair of spanned values
pub struct HelpFlag<'a> {
    pub tags: FlagTags<&'a str, char>,
    pub span: Span,
}

pub fn error_pair(
    origin1: Span,
    message1: impl Display,
    origin2: Span,
    message2: impl Display,
) -> syn::Error {
    let mut error = syn::Error::new(origin1, message1);
    error.combine(syn::Error::new(origin2, message2));
    error
}
