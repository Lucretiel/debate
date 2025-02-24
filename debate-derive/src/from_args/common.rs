use darling::{
    FromAttributes,
    util::{Override, SpannedValue},
};
use heck::ToKebabCase as _;
use proc_macro2::{Literal, TokenStream as TokenStream2};
use quote::{ToTokens, format_ident, quote};
use syn::{Expr, Field, Ident, Index, Type, spanned::Spanned as _};

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

/// Create a state block, in curlies. Used both for the struct state, and
/// for separate enum variant states
pub fn struct_state_block_from_fields<'a>(
    fields: impl IntoIterator<Item = &'a ParsedFieldInfo<'a>>,
) -> impl ToTokens {
    {
        let field_state_types = fields
            .into_iter()
            .map(|info| match *info {
                ParsedFieldInfo::Positional(PositionalFieldInfo { ty, .. })
                | ParsedFieldInfo::Option(OptionFieldInfo { ty, .. }) => FlattenOr::Normal(ty),
                ParsedFieldInfo::Flatten(FlattenFieldInfo { ty, .. }) => FlattenOr::Flatten(ty),
            })
            .map(|field| match field {
                FlattenOr::Flatten(ty) => quote! {
                    <#ty as ::debate::from_args::BuildFromArgs<'arg>>::State
                },
                FlattenOr::Normal(ty) => quote! {
                    ::core::option::Option<#ty>
                },
            });

        quote! {
            {
                position: u16,
                phantom: ::core::marker::PhantomData<& 'arg ()>,
                fields: (#(#field_state_types,)*),
            }
        }
    }
}

pub fn struct_state_init_block_from_fields<'a>(
    fields: impl IntoIterator<Item = &'a ParsedFieldInfo<'a>>,
) -> impl ToTokens {
    let field_state_initializers = fields.into_iter().map(|info| match *info {
        ParsedFieldInfo::Positional(_) | ParsedFieldInfo::Option(_) => quote! {
            ::core::option::Option::None
        },
        ParsedFieldInfo::Flatten(_) => quote! {
            ::core::default::Default::default()
        },
    });

    quote! {
        {
            position: 0,
            phantom: ::core::marker::PhantomData,
            fields: (#(#field_state_initializers,)*),
        }
    }
}

#[derive(darling::FromAttributes, Debug)]
#[darling(attributes(debate))]
struct RawParsedAttr {
    long: Option<Override<SpannedValue<String>>>,
    short: Option<Override<SpannedValue<char>>>,
    default: Option<Override<Expr>>,
    // clear = "no-verbose"
    // placeholder = "VALUE"
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

pub struct PositionalFieldInfo<'a> {
    pub ident: IdentString<'a>,
    pub ty: &'a Type,
    pub default: FieldDefault,
    pub docs: String,
}

pub struct OptionFieldInfo<'a> {
    pub ident: IdentString<'a>,
    pub ty: &'a Type,
    pub default: FieldDefault,
    pub docs: String,
    pub tags: OptionTag,
}

pub struct FlattenFieldInfo<'a> {
    pub ident: Option<IdentString<'a>>,
    pub ty: &'a Type,
}

impl FlattenFieldInfo<'_> {
    #[inline]
    pub fn ident_str(&self) -> Option<&str> {
        self.ident.as_ref().map(|ident| ident.as_str())
    }
}

pub enum ParsedFieldInfo<'a> {
    Positional(PositionalFieldInfo<'a>),
    Option(OptionFieldInfo<'a>),
    Flatten(FlattenFieldInfo<'a>),
}

impl<'a> ParsedFieldInfo<'a> {
    pub fn from_field(field: &'a Field) -> syn::Result<Self> {
        let parsed = RawParsedAttr::from_attributes(&field.attrs)?;

        let ty = &field.ty;
        let ident = field.ident.as_ref().map(IdentString::new);

        // TODO: enforce that flatten doesn't coexist with other variants.
        if let Some(()) = parsed.flatten {
            return Ok(Self::Flatten(FlattenFieldInfo { ident, ty }));
        }

        let ident = ident.ok_or_else(|| {
            syn::Error::new(
                field.span(),
                "can't use non-flattened fields in tuple structs",
            )
        })?;

        let long = parsed
            .long
            .map(|long| compute_long(long.explicit(), ident.raw()))
            .transpose()?;

        let short = parsed
            .short
            .map(|short| compute_short(short.explicit(), ident.raw()))
            .transpose()?;

        let default = FieldDefault::new(parsed.default);

        Ok(
            match match (long, short) {
                (None, None) => None,
                (Some(long), None) => Some(OptionTag::Long(long)),
                (None, Some(short)) => Some(OptionTag::Short(short)),
                (Some(long), Some(short)) => Some(OptionTag::LongShort(long, short)),
            } {
                None => Self::Positional(PositionalFieldInfo {
                    ident,
                    ty,
                    default,
                    docs: String::new(),
                }),
                Some(tags) => Self::Option(OptionFieldInfo {
                    ident,
                    ty,
                    default,
                    docs: String::new(),
                    tags,
                }),
            },
        )
    }

    pub fn get_positional(&self) -> Option<FlattenOr<&FlattenFieldInfo, &PositionalFieldInfo>> {
        match self {
            ParsedFieldInfo::Positional(info) => Some(FlattenOr::Normal(info)),
            ParsedFieldInfo::Flatten(info) => Some(FlattenOr::Flatten(info)),
            ParsedFieldInfo::Option(_) => None,
        }
    }
}

fn compute_long(
    long: Option<SpannedValue<String>>,
    field_name: &Ident,
) -> syn::Result<SpannedValue<String>> {
    // Currently this can never fail, but we want to leave open the possibility
    let long = long.unwrap_or_else(|| {
        SpannedValue::new(field_name.to_string().to_kebab_case(), field_name.span())
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
    } else {
        Ok(long)
    }
}

fn compute_short(
    short: Option<SpannedValue<char>>,
    field_name: &Ident,
) -> syn::Result<SpannedValue<char>> {
    let c = short.unwrap_or_else(|| {
        SpannedValue::new(
            field_name
                .to_string()
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

pub enum OptionTag {
    Long(SpannedValue<String>),
    Short(SpannedValue<char>),
    LongShort(SpannedValue<String>, SpannedValue<char>),
}

impl OptionTag {
    pub fn long(&self) -> Option<SpannedValue<&str>> {
        match *self {
            OptionTag::Long(ref long) | OptionTag::LongShort(ref long, _) => {
                Some(SpannedValue::new(long.as_str(), long.span()))
            }
            OptionTag::Short(_) => None,
        }
    }

    pub fn short(&self) -> Option<SpannedValue<char>> {
        match *self {
            OptionTag::Short(short) | OptionTag::LongShort(_, short) => Some(short),
            OptionTag::Long(_) => None,
        }
    }
}

/// Create an expression that applies an incoming `argument` to a given parameter
/// by calling either `Parameter::initial_method` or `Parameter::follow_up_method`,
/// depending on whether the field is present already. This expression's type
/// is always `Result<(), impl ParameterError>`. The field is
/// `fields.#field-index`.
pub fn apply_arg_to_field(
    field_index: &Index,
    initial_method: &Ident,
    follow_up_method: &Ident,
) -> impl ToTokens {
    quote! {
        match fields.#field_index {
            ::core::option::Option::None => match ::debate::parameter::Parameter::#initial_method(argument) {
                ::core::result::Result::Err(err) => ::core::result::Result::Err(err),
                ::core::result::Result::Ok(value) => {
                    fields.#field_index = ::core::option::Option::Some(value);
                    ::core::result::Result::Ok(())
                }
            }
            ::core::option::Option::Some(ref mut old) => ::debate::parameter::Parameter::#follow_up_method(
                old,
                argument
            ),
        }
    }
}

/// Given an expression that returns a Result<(), E>, wrap it such that `Ok`
/// is RETURNED, `DetectUnrecognized::Unrecognized` is propagated as the value
/// of the expression, and other errors are returned as an error via `wrap_error`
pub fn handle_propagated_error(
    expr: impl ToTokens,
    unrecognized_bind: impl ToTokens,
    unrecognized: impl ToTokens,
    wrap_error: impl FnOnce(&Ident) -> TokenStream2,
) -> TokenStream2 {
    let err = format_ident!("error");
    let wrap_err = wrap_error(&err);

    quote! {
        match (#expr) {
            ::core::result::Result::Ok(()) => return ::core::result::Result::Ok(()),
            ::core::result::Result::Err(err) => match err {
                ::debate::util::DetectUnrecognized::Unrecognized(#unrecognized_bind) => #unrecognized,
                ::debate::util::DetectUnrecognized::Error(#err) => return #wrap_err,
            }
        }
    }
}

/// Create an expression that calls a state method on a given field, then
/// handles the error returned by it (specifically, it uses DetectUnrecognized
/// to allow arguments to be retried).
// TODO: wrap the error in `StateError::flatten`
pub fn handle_flatten(
    expr: impl ToTokens,
    field_name: Option<&str>,
    unrecognized_bind: impl ToTokens,
    unrecognized: impl ToTokens,
) -> impl ToTokens {
    handle_propagated_error(
        expr,
        unrecognized_bind,
        unrecognized,
        |err| match field_name {
            None => quote! { #err },
            Some(field_name) => quote! {
                ::debate::state::Error::flattened(#field_name, #err)
            },
        },
    )
}

/// Given a set of fields, create an iterator of blocks suitable for use
/// as part of the body of `visit_positional`.
///
/// Specifically, the iterator will emit blocks resembling:
///
/// ```
/// if *position == scrutinee {
///     *position = {/* handle argument for this field */}
/// }
/// ```
pub fn visit_positional_arms_for_fields<'a>(
    arg_ident: &Ident,
    add_arg_ident: &Ident,
    fields: impl IntoIterator<Item = &'a ParsedFieldInfo<'a>>,
) -> impl Iterator<Item = TokenStream2> {
    fields
        .into_iter()
        .enumerate()
        .map(|(idx, field)| (Index::from(idx), field))
        .filter_map(|(idx, info)| info.get_positional().map(|info| (idx, info)))
        .enumerate()
        .map(|(position, info)| (Literal::usize_unsuffixed(position), info))
        .map(|(position, (idx, info))| match info {
            // This is a regular positional field
            FlattenOr::Normal(info) => {
                let field_str = info.ident.as_str();
                let apply_argument = apply_arg_to_field(&idx, arg_ident, add_arg_ident);

                let body = handle_propagated_error(
                    apply_argument,
                    quote! { () },
                    quote! { #position + 1 },
                    |err| {
                        quote! {
                            ::debate::state::Error::parameter(
                                #field_str,
                                #err
                            )
                        }
                    },
                );

                // TODO: consider `loop { match { } }` instead of a waterfall
                // of `if`
                quote! {
                    if *position == #position {
                        *position = #body;
                    }
                }
            }
            // This is a flattened field
            FlattenOr::Flatten(info) => {
                let expr = quote! {
                    ::debate::state::State::add_positional(
                        &mut fields.#idx,
                        argument
                    )
                };

                let body = handle_flatten(
                    expr,
                    info.ident_str(),
                    quote! { () },
                    quote! { #position + 1 },
                );

                quote! {
                    if *position == #position {
                        *position = #body;
                    }
                }
            }
        })
}
