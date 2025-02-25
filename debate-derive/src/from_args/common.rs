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

pub fn indexed_fields<'a>(
    fields: &'a [ParsedFieldInfo<'a>],
) -> impl Iterator<Item = (Index, &'a ParsedFieldInfo<'a>)> {
    fields
        .iter()
        .enumerate()
        .map(|(index, field)| (Index::from(index), field))
}

pub fn option_fields<'a>(
    fields: &'a [ParsedFieldInfo<'a>],
) -> impl Iterator<Item = (Index, &'a OptionFieldInfo<'a>)> {
    indexed_fields(fields).filter_map(|(index, field)| match field {
        ParsedFieldInfo::Option(field) => Some((index, field)),
        ParsedFieldInfo::Positional(_) | ParsedFieldInfo::Flatten(_) => None,
    })
}

pub fn flatten_fields<'a>(
    fields: &'a [ParsedFieldInfo<'a>],
) -> impl Iterator<Item = (Index, &'a FlattenFieldInfo<'a>)> {
    indexed_fields(fields).filter_map(|(index, field)| match field {
        ParsedFieldInfo::Flatten(field) => Some((index, field)),
        ParsedFieldInfo::Option(_) | ParsedFieldInfo::Positional(_) => None,
    })
}

pub fn positional_flatten_fields<'a>(
    fields: &'a [ParsedFieldInfo<'a>],
) -> impl Iterator<
    Item = (
        Index,
        Literal,
        FlattenOr<&'a FlattenFieldInfo<'a>, &'a PositionalFieldInfo<'a>>,
    ),
> {
    indexed_fields(fields)
        .filter_map(|(idx, field)| field.get_positional().map(|info| (idx, info)))
        .enumerate()
        .map(|(position, (index, field))| (index, Literal::usize_unsuffixed(position), field))
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
    fields_ident: &Ident,
    argument_ident: &Ident,
    field_index: &Index,
    initial_method: &Ident,
    follow_up_method: &Ident,
) -> impl ToTokens {
    quote! {
        match #fields_ident.#field_index {
            ::core::option::Option::None => match ::debate::parameter::Parameter::#initial_method(#argument_ident) {
                ::core::result::Result::Err(err) => ::core::result::Result::Err(err),
                ::core::result::Result::Ok(value) => {
                    #fields_ident.#field_index = ::core::option::Option::Some(value);
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
                ::debate::util::DetectUnrecognized::Error(#err) => return ::core::result::Result::Err(#wrap_err),
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
) -> TokenStream2 {
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
pub fn visit_positional_arms_for_fields(
    fields_ident: &Ident,
    argument_ident: &Ident,
    arg_ident: &Ident,
    add_arg_ident: &Ident,
    fields: &[ParsedFieldInfo<'_>],
) -> impl Iterator<Item = TokenStream2> {
    positional_flatten_fields(fields).map(|(idx, position, info)| {
        let body = match info {
            // This is a regular positional field
            FlattenOr::Normal(info) => {
                let field_str = info.ident.as_str();
                let apply_argument = apply_arg_to_field(
                    fields_ident,
                    argument_ident,
                    &idx,
                    arg_ident,
                    add_arg_ident,
                );

                handle_propagated_error(
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
                )
            }
            // This is a flattened field
            FlattenOr::Flatten(info) => {
                let expr = quote! {
                    ::debate::state::State::add_positional(
                        &mut fields.#idx,
                        argument
                    )
                };

                handle_flatten(
                    expr,
                    info.ident_str(),
                    quote! { () },
                    quote! { #position + 1 },
                )
            }
        };

        quote! {
            if *position == #position {
                *position = #body;
            }
        }
    })
}

#[expect(clippy::too_many_arguments)]
fn complete_option_body<'a>(
    fields_ident: &Ident,
    argument_ident: &Ident,
    option_expr: impl ToTokens,

    fields: &'a [ParsedFieldInfo<'a>],
    make_scrutinee: impl Fn(&'a OptionFieldInfo<'a>) -> Option<Literal>,

    parameter_method: &Ident,
    add_parameter_method: &Ident,

    flatten_state_method: &Ident,
    flatten_rebind_argument: impl ToTokens,
) -> TokenStream2 {
    let local_arms = option_fields(fields)
        .filter_map(move |(index, field)| {
            make_scrutinee(field).map(|scrutinee| (field, scrutinee, index))
        })
        .map(move |(field, scrutinee, index)| {
            let field_name = field.ident.as_str();

            let expr = apply_arg_to_field(
                fields_ident,
                argument_ident,
                &index,
                parameter_method,
                add_parameter_method,
            );

            quote! {
                #scrutinee => match (#expr) {
                    ::core::result::Result::Ok(()) => ::core::result::Result::Ok(()),
                    ::core::result::Result::Err(err) => ::core::result::Result::Err(
                        ::debate::state::Error::parameter(#field_name, err)
                    ),
                },
            }
        });

    let flatten_arms = flatten_fields(fields).map(|(index, info)| {
        let body = handle_flatten(
            quote! {
                ::debate::state::State::#flatten_state_method(
                    &mut fields.#index,
                    option,
                    argument
                )
            },
            info.ident_str(),
            argument_ident,
            argument_ident,
        );

        quote! {
            let #flatten_rebind_argument = #body;
        }
    });

    quote! {
        match (#option_expr) {
            #(#local_arms)*
            _ => {
                #(#flatten_arms)*

                ::core::result::Result::Err(
                    ::debate::state::Error::unrecognized(
                        #flatten_rebind_argument
                    )
                )
            }
        }
    }
}

pub fn complete_long_option_body(
    fields_ident: &Ident,
    argument_ident: &Ident,
    option_ident: &Ident,

    fields: &[ParsedFieldInfo<'_>],
) -> TokenStream2 {
    complete_option_body(
        fields_ident,
        argument_ident,
        quote! { #option_ident.bytes() },
        fields,
        |info| {
            info.tags
                .long()
                .map(|long| Literal::byte_string(long.as_bytes()))
        },
        &format_ident!("arg"),
        &format_ident!("add_arg"),
        &format_ident!("add_long_option"),
        quote! { () },
    )
}

pub fn complete_long_body(
    fields_ident: &Ident,
    argument_ident: &Ident,
    option_ident: &Ident,

    fields: &[ParsedFieldInfo<'_>],
) -> TokenStream2 {
    complete_option_body(
        fields_ident,
        argument_ident,
        quote! { #option_ident.bytes() },
        fields,
        |info| {
            info.tags
                .long()
                .map(|long| Literal::byte_string(long.as_bytes()))
        },
        &format_ident!("present"),
        &format_ident!("add_present"),
        &format_ident!("add_long"),
        argument_ident,
    )
}

pub fn complete_short_body(
    fields_ident: &Ident,
    argument_ident: &Ident,
    option_ident: &Ident,

    fields: &[ParsedFieldInfo<'_>],
) -> TokenStream2 {
    complete_option_body(
        fields_ident,
        argument_ident,
        option_ident,
        fields,
        |info| {
            info.tags
                .short()
                .map(|short| Literal::byte_character(*short as u8))
        },
        &format_ident!("present"),
        &format_ident!("add_present"),
        &format_ident!("add_short"),
        argument_ident,
    )
}

pub fn final_field_initializers(
    fields_ident: &Ident,
    fields: &[ParsedFieldInfo<'_>],
) -> impl Iterator<Item = TokenStream2> {
    struct NormalFieldInfo<'a> {
        long: Option<&'a str>,
        short: Option<char>,
        ident: &'a IdentString<'a>,
        default: &'a FieldDefault,
    }

    indexed_fields(fields)
        .map(|(index, field)| {
            (
                index,
                match field {
                    ParsedFieldInfo::Positional(field) => FlattenOr::Normal(NormalFieldInfo {
                        long: None,
                        short: None,
                        ident: &field.ident,
                        default: &field.default,
                    }),
                    ParsedFieldInfo::Option(field) => FlattenOr::Normal(NormalFieldInfo {
                        long: field.tags.long().as_deref().copied(),
                        short: field.tags.short().as_deref().copied(),
                        ident: &field.ident,
                        default: &field.default,
                    }),
                    ParsedFieldInfo::Flatten(field) => FlattenOr::Flatten(field),
                },
            )
        })
        .map(move |(idx, field)| match field {
            FlattenOr::Normal(field) => {
                let ident = field.ident.raw();
                let ident_str = field.ident.as_str();

                let long = match field.long {
                    Some(long) => quote! {::core::option::Option::Some(#long)},
                    None => quote! {::core::option::Option::None},
                };

                let short = match field.short {
                    Some(short) => quote! {::core::option::Option::Some(#short)},
                    None => quote! {::core::option::Option::None},
                };

                let default = match field.default {
                    FieldDefault::Expr(expr) => quote! { #expr },
                    FieldDefault::Trait => quote! { ::core::default::Default::default() },
                    FieldDefault::None => quote! {
                    match ::debate::parameter::Parameter::absent() {
                        ::core::result::Result::Ok(value) => value,
                        ::core::result::Result::Err(::debate::parameter::RequiredError) =>
                            return ::core::result::Result::Err(
                                ::debate::from_args::Error::required(
                                    #ident_str, #long, #short
                                )
                            ),
                        }
                    },
                };

                quote! {
                    #ident: match #fields_ident.#idx {
                        ::core::option::Option::Some(value) => value,
                        ::core::option::Option::None => #default,
                    }
                }
            }
            FlattenOr::Flatten(FlattenFieldInfo { ident, .. }) => {
                let wrap_err = match ident.as_ref().map(|ident| ident.as_str()) {
                    Some(ident) => quote! {
                        ::debate::from_args::Error::flattened(#ident, err)
                    },
                    None => quote! { err },
                };

                let expr = quote! {
                    match ::debate::from_args::BuildFromArgs::build(
                        #fields_ident.#idx
                    ) {
                        ::core::result::Result::Ok(value) => value,
                        ::core::result::Result::Err(err) => return (
                            ::core::result::Result::Err(#wrap_err)
                        ),
                    }
                };

                match ident {
                    Some(ident) => quote! { #ident : #expr },
                    None => expr,
                }
            }
        })
}
