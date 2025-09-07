use proc_macro2::{Literal, Span, TokenStream as TokenStream2};
use quote::{ToTokens, format_ident, quote};
use syn::{Ident, Index, Lifetime};

use crate::common::{
    FieldDefault, FlagFieldInfo, FlattenFieldInfo, FlattenOr, IdentString, ParsedFieldInfo,
    PositionalFieldInfo,
};

enum HelpMode {
    Succinct,
    Full,
}

pub fn struct_state_block<'a>(
    position_ty: impl ToTokens,
    lifetime: &Lifetime,
    fields_types: impl IntoIterator<Item = TokenStream2>,
) -> TokenStream2 {
    let fields_types = fields_types.into_iter();

    quote! {
        {
            position: #position_ty,
            phantom: ::core::marker::PhantomData<& #lifetime ()>,
            fields: ( #(#fields_types,)* ),
        }
    }
}

/// Create a state block, in curlies. Used both for the struct state, and
/// for separate enum variant states
pub fn struct_state_block_from_fields<'a>(
    fields: impl IntoIterator<Item = &'a ParsedFieldInfo<'a>>,
    lifetime: &Lifetime,
) -> TokenStream2 {
    let field_state_types = fields
        .into_iter()
        .map(|info| match *info {
            ParsedFieldInfo::Positional(PositionalFieldInfo { ty, .. })
            | ParsedFieldInfo::Option(FlagFieldInfo { ty, .. }) => FlattenOr::Normal(ty),
            ParsedFieldInfo::Flatten(FlattenFieldInfo { ty, .. }) => FlattenOr::Flatten(ty),
        })
        .map(|field| match field {
            FlattenOr::Flatten(ty) => quote! {
                <#ty as ::debate::build::BuildFromArgs<#lifetime>>::State
            },
            FlattenOr::Normal(ty) => quote! {
                ::core::option::Option<#ty>
            },
        });

    struct_state_block(quote! { u16 }, lifetime, field_state_types)
}

/// Create a default initializer block for a structure
pub fn struct_state_init_block_from_field_count<'a>(num_fields: usize) -> TokenStream2 {
    let field_state_initializers = (0..num_fields).into_iter().map(|_| {
        quote! {
            ::core::default::Default::default()
        }
    });

    quote! {
        {
            position: ::core::default::Default::default(),
            phantom: ::core::marker::PhantomData,
            // It would be nice to just call `default()` here, but that's only
            // defined for tuples up to 12 values.
            fields: (#(#field_state_initializers,)*),
        }
    }
}

fn indexed_fields<'a>(
    fields: &'a [ParsedFieldInfo<'a>],
) -> impl Iterator<Item = (Index, &'a ParsedFieldInfo<'a>)> + DoubleEndedIterator {
    fields
        .iter()
        .enumerate()
        .map(|(index, field)| (Index::from(index), field))
}

fn option_fields<'a>(
    fields: &'a [ParsedFieldInfo<'a>],
) -> impl Iterator<Item = (Index, &'a FlagFieldInfo<'a>)> {
    indexed_fields(fields).filter_map(|(index, field)| match field {
        ParsedFieldInfo::Option(field) => Some((index, field)),
        ParsedFieldInfo::Positional(_) | ParsedFieldInfo::Flatten(_) => None,
    })
}

fn flatten_fields<'a>(
    fields: &'a [ParsedFieldInfo<'a>],
) -> impl Iterator<Item = (Index, &'a FlattenFieldInfo<'a>)> + DoubleEndedIterator {
    indexed_fields(fields).filter_map(|(index, field)| match field {
        ParsedFieldInfo::Flatten(field) => Some((index, field)),
        ParsedFieldInfo::Option(_) | ParsedFieldInfo::Positional(_) => None,
    })
}

fn positional_flatten_fields<'a>(
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

/// Create an expression that applies an incoming `argument` to a given parameter
/// by calling either `Parameter::initial_method` or `Parameter::follow_up_method`,
/// depending on whether the field is present already. This expression's type
/// is always `Result<(), impl ParameterError>`. The field is
/// `#fields_ident.#field_index`.
pub fn apply_arg_to_field(
    fields_ident: &Ident,
    argument_ident: &Ident,
    field_index: &Index,
    parameter_trait: &Ident,
    initial_method: &Ident,
    follow_up_method: &Ident,
) -> impl ToTokens {
    quote! {
        match #fields_ident.#field_index {
            ::core::option::Option::None => match ::debate::parameter::#parameter_trait::#initial_method(#argument_ident) {
                ::core::result::Result::Err(err) => ::core::result::Result::Err(err),
                ::core::result::Result::Ok(value) => {
                    #fields_ident.#field_index = ::core::option::Option::Some(value);
                    ::core::result::Result::Ok(())
                }
            }
            ::core::option::Option::Some(ref mut old) => ::debate::parameter::#parameter_trait::#follow_up_method(
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
pub fn handle_flatten(
    expr: impl ToTokens,
    field_name: &str,
    unrecognized_bind: impl ToTokens,
    unrecognized: impl ToTokens,
) -> TokenStream2 {
    handle_propagated_error(expr, unrecognized_bind, unrecognized, |err| {
        quote! { ::debate::state::Error::flattened(#field_name, #err) }
    })
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
    trait_ident: &Ident,
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
                    trait_ident,
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
                    info.ident.as_str(),
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

/// Shared logic for creating the match block that handles options of all
/// kinds.
#[expect(clippy::too_many_arguments)]
fn complete_option_body<'a>(
    fields_ident: &Ident,
    argument_ident: &Ident,
    option_expr: impl ToTokens,

    fields: &'a [ParsedFieldInfo<'a>],
    help: Option<(Literal, HelpMode)>,
    make_scrutinee: impl Fn(&'a FlagFieldInfo<'a>) -> Option<Literal>,

    trait_ident: &Ident,
    parameter_method: &Ident,
    add_parameter_method: &Ident,

    flatten_state_method: &Ident,
    flatten_rebind_argument: impl ToTokens,
) -> TokenStream2 {
    let help_arm = help.map(|(help, mode)| {
        let mode = match mode {
            HelpMode::Succinct => "Succinct",
            HelpMode::Full => "Full",
        };

        let mode = Ident::new(mode, Span::mixed_site());

        quote! {
            #help => ::core::result::Result::Err(
                ::debate::state::Error::help_requested(
                    ::debate::help::HelpRequest:: #mode
                )
            ),
        }
    });

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
                trait_ident,
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
            info.ident.as_str(),
            argument_ident,
            argument_ident,
        );

        quote! {
            let #flatten_rebind_argument = #body;
        }
    });

    quote! {
        match (#option_expr) {
            #help_arm
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
    parameter_ident: &Ident,

    fields: &[ParsedFieldInfo<'_>],
    help: Option<&str>,
) -> TokenStream2 {
    complete_option_body(
        fields_ident,
        argument_ident,
        quote! { #option_ident.bytes() },
        fields,
        help.map(|long| (Literal::byte_string(long.as_bytes()), HelpMode::Full)),
        |info| {
            info.tags
                .long()
                .map(|long| Literal::byte_string(long.as_bytes()))
        },
        parameter_ident,
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
    parameter_ident: &Ident,

    fields: &[ParsedFieldInfo<'_>],
    help: Option<&str>,
) -> TokenStream2 {
    complete_option_body(
        fields_ident,
        argument_ident,
        quote! { #option_ident.bytes() },
        fields,
        help.map(|long| (Literal::byte_string(long.as_bytes()), HelpMode::Full)),
        |info| {
            info.tags
                .long()
                .map(|long| Literal::byte_string(long.as_bytes()))
        },
        parameter_ident,
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
    parameter_ident: &Ident,

    fields: &[ParsedFieldInfo<'_>],
    help: Option<char>,
) -> TokenStream2 {
    complete_option_body(
        fields_ident,
        argument_ident,
        option_ident,
        fields,
        help.map(|short| (Literal::byte_character(short as u8), HelpMode::Succinct)),
        |info| {
            info.tags
                .short()
                .map(|short| Literal::byte_character(*short as u8))
        },
        parameter_ident,
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
        placeholder: &'a str,
        ident: &'a IdentString<'a>,
        default: Option<&'a FieldDefault>,
    }

    indexed_fields(fields)
        .map(|(index, field)| {
            (
                index,
                match field {
                    ParsedFieldInfo::Positional(field) => FlattenOr::Normal(NormalFieldInfo {
                        long: None,
                        short: None,
                        placeholder: &field.placeholder,
                        ident: &field.ident,
                        default: field.default.as_ref(),
                    }),
                    ParsedFieldInfo::Option(field) => FlattenOr::Normal(NormalFieldInfo {
                        long: field.tags.long().as_deref().copied(),
                        short: field.tags.short().as_deref().copied(),
                        placeholder: &field.placeholder,
                        ident: &field.ident,
                        default: field.default.as_ref(),
                    }),
                    ParsedFieldInfo::Flatten(field) => FlattenOr::Flatten(field),
                },
            )
        })
        .map(move |(idx, field)| match field {
            FlattenOr::Normal(field) => {
                let ident = field.ident.raw();
                let ident_str = field.ident.as_str();
                let placeholder = field.placeholder;

                // Technically we allocate too eagerly here. It reads better
                // this way, imo, and we trust the optimizer to reorder a lot
                // of this stuff so that (for example) the `let error` is only
                // evaluated in the `FieldDefault::None` case.
                let short = match field.short {
                    Some(short) => quote! {::core::option::Option::Some(#short)},
                    None => quote! {::core::option::Option::None},
                };

                let error = match (field.long, field.short) {
                    (Some(long), _) => quote! {
                        required_long( #ident_str, #long, #short )
                    },
                    (None, Some(short)) => quote! {
                        required_short( #ident_str, #short )
                    },
                    (None, None) => quote! {
                        required_positional( #ident_str, #placeholder )
                    },
                };

                let default = match field.default {
                    Some(FieldDefault::Expr(expr)) => quote! { #expr },
                    Some(FieldDefault::Trait) => quote! { ::core::default::Default::default() },
                    None => quote! {
                        match ::debate::parameter::Parameter::absent() {
                            ::core::result::Result::Ok(value) => value,
                            ::core::result::Result::Err(::debate::parameter::RequiredError) => {
                                return ::core::result::Result::Err(
                                    ::debate::build::Error:: #error,
                                )
                            }
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
                let field_name = ident.as_str();

                quote! {
                    #ident : match ::debate::build::BuildFromArgs::build(
                        #fields_ident.#idx,
                    ) {
                        ::core::result::Result::Ok(value) => value,
                        ::core::result::Result::Err(err) => return (
                            ::core::result::Result::Err(
                                ::debate::build::Error::flattened(#field_name, err)
                            )
                        ),
                    }
                }
            }
        })
}

/// Create a series of calls to `state.get_subcommand_path` for each
/// of the flattened fields, calling `return Ok(out)` if there's a successful
/// hit. We check each field in reverse order, which ends up making more sense
/// for --help.
pub fn get_subcommand_field_visitor_calls(
    fields_ident: &Ident,
    visitor_ident: &Ident,

    fields: &[ParsedFieldInfo<'_>],
) -> impl Iterator<Item = TokenStream2> {
    flatten_fields(fields).rev().map(move |(index, field)| {
        let field_name = field.ident.as_str();

        quote! {
            let #visitor_ident = match ::debate::state::State::get_subcommand_path(
                &#fields_ident.#index,
                ::debate::state::SubcommandPathVisitorWithItem::new(
                    ::debate::state::SubcommandPathItem::Field(#field_name),
                    #visitor_ident,
                )
            ) {
                ::core::result::Result::Ok(out) => return ::core::result::Result::Ok(out),
                ::core::result::Result::Err(visitor) => visitor.into_inner(),
            };
        }
    })
}
