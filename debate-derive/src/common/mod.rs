pub mod enumeration;
pub mod value;

use std::sync::OnceLock;

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

/// Attributes for a field
#[derive(darling::FromAttributes, Debug)]
#[darling(attributes(debate))]
struct RawParsedFieldAttr {
    long: Option<Override<SpannedValue<String>>>,
    short: Option<Override<SpannedValue<char>>>,
    default: Option<Override<Expr>>,
    placeholder: Option<SpannedValue<String>>,
    // clear = "no-verbose"
    // overridable

    // TODO: add a parse step where `flatten` must not coexist with the other
    // variants. Consider switching from `darling` to `deluxe`, which apparently
    // handles this.
    flatten: Option<()>,
}

pub enum FieldDefault {
    None,
    Trait,
    Expr(Expr),
}

impl FieldDefault {
    pub fn new(default: Option<Override<Expr>>) -> Self {
        match default {
            Some(Override::Explicit(default)) => Self::Expr(default),
            Some(Override::Inherit) => Self::Trait,
            None => Self::None,
        }
    }
}

pub enum OptionTag<Long, Short> {
    Long(Long),
    Short(Short),
    LongShort { long: Long, short: Short },
}

impl OptionTag<SpannedValue<String>, SpannedValue<char>> {
    pub fn long(&self) -> Option<SpannedValue<&str>> {
        match *self {
            OptionTag::Long(ref long) | OptionTag::LongShort { ref long, .. } => {
                Some(SpannedValue::new(long.as_str(), long.span()))
            }
            OptionTag::Short(_) => None,
        }
    }

    pub fn short(&self) -> Option<SpannedValue<char>> {
        match *self {
            OptionTag::Short(short) | OptionTag::LongShort { short, .. } => Some(short),
            OptionTag::Long(_) => None,
        }
    }

    // Shed the spanned value stuff if we don't need it
    pub fn simplify(&self) -> OptionTag<&str, char> {
        match self {
            OptionTag::Long(long) => OptionTag::Long(long.as_str()),
            OptionTag::Short(short) => OptionTag::Short(**short),
            OptionTag::LongShort { long, short } => OptionTag::LongShort {
                long: long.as_str(),
                short: **short,
            },
        }
    }
}

pub struct PositionalFieldInfo<'a> {
    pub ident: IdentString<'a>,
    pub placeholder: SpannedValue<String>,
    pub ty: &'a Type,
    pub default: FieldDefault,
    pub docs: String,
}

pub struct OptionFieldInfo<'a> {
    pub ident: IdentString<'a>,
    pub placeholder: SpannedValue<String>,
    pub ty: &'a Type,
    pub default: FieldDefault,
    pub docs: String,
    pub tags: OptionTag<SpannedValue<String>, SpannedValue<char>>,
}

pub struct FlattenFieldInfo<'a> {
    pub docs: String,
    pub ident: Option<IdentString<'a>>,
    pub group_name: Option<SpannedValue<String>>,
    /// When this entire field is used as a placeholder (basically just for
    /// subcommands, but could be useful later)
    pub placeholder: Option<SpannedValue<String>>,
    pub ty: &'a Type,
}

impl FlattenFieldInfo<'_> {
    #[inline]
    pub fn ident_str(&self) -> Option<&str> {
        self.ident.as_ref().map(|ident| ident.as_str())
    }
}

// TODO: compute short + long docs
// TODO: rewrap
pub fn compute_docs(attrs: &[Attribute]) -> syn::Result<String> {
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
    let leading_whitespace = regex!("^[\t\n ]*\n");
    let paragraph_separator = regex!("[\t ]*\n[*\t\n ]*\n");

    let body = body.trim_end();
    let body = leading_whitespace.replace(body, "");

    Ok(paragraph_separator
        .split(&body)
        .map(|paragraph| {
            // This is basically `textwrap::refill`, except that `refill` takes
            // care to preserve line prefixes, and we explicitly are trying not
            // to do that.
            let (unfilled, _) = textwrap::unfill(paragraph);
            textwrap::fill(&unfilled, 80)
        })
        .join("\n\n"))
}

pub enum ParsedFieldInfo<'a> {
    Positional(PositionalFieldInfo<'a>),
    Option(OptionFieldInfo<'a>),
    Flatten(FlattenFieldInfo<'a>),
}

impl<'a> ParsedFieldInfo<'a> {
    pub fn from_field(field: &'a Field) -> syn::Result<Self> {
        let parsed = RawParsedFieldAttr::from_attributes(&field.attrs)?;
        let docs = compute_docs(&field.attrs)?;

        let ty = &field.ty;
        let ident = field.ident.as_ref().map(IdentString::new);

        // TODO: enforce that flatten doesn't coexist with other variants.
        if let Some(()) = parsed.flatten {
            // TODO: allow rename for flattened fields
            let group_name = ident.as_ref().map(|ident| {
                let name = ident.as_str();

                let name = if name.eq_ignore_ascii_case("subcommand") {
                    "Subcommands".to_owned()
                } else if name.eq_ignore_ascii_case("command") {
                    "Commands".to_owned()
                } else {
                    name.to_title_case()
                };

                SpannedValue::new(name, ident.span())
            });

            let placeholder = ident.as_ref().map(|ident| {
                SpannedValue::new(ident.as_str().to_shouty_snake_case(), ident.span())
            });

            return Ok(Self::Flatten(FlattenFieldInfo {
                ident,
                ty,
                group_name,
                placeholder,
                docs,
            }));
        }

        let ident = ident.ok_or_else(|| {
            syn::Error::new(
                field.span(),
                "can't use non-flattened fields in tuple structs",
            )
        })?;

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
                (Some(long), None) => Some(OptionTag::Long(long)),
                (None, Some(short)) => Some(OptionTag::Short(short)),
                (Some(long), Some(short)) => Some(OptionTag::LongShort { long, short }),
            } {
                None => Self::Positional(PositionalFieldInfo {
                    ident,
                    placeholder,
                    ty,
                    default,
                    docs,
                }),
                Some(tags) => Self::Option(OptionFieldInfo {
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

    pub const fn as_tags(&self) -> Option<OptionTag<&'a str, char>> {
        match (self.long, self.short) {
            (None, None) => None,
            (Some(long), None) => Some(OptionTag::Long(long)),
            (None, Some(short)) => Some(OptionTag::Short(short)),
            (Some(long), Some(short)) => Some(OptionTag::LongShort { long, short }),
        }
    }
}
