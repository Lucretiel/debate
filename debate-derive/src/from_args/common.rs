use proc_macro2::{Literal, Span, TokenStream as TokenStream2};
use quote::{ToTokens, format_ident, quote};
use syn::{Ident, Index, Lifetime};

use crate::common::{
    FieldDefault, FlagFieldInfo, FlagTags, FlattenFieldInfo, FlattenOr, IdentString,
    ParsedFieldInfo, PositionalFieldInfo,
};

pub enum HelpMode {
    Succinct,
    Full,
}

#[must_use]
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
#[must_use]
pub fn struct_state_block_from_fields(
    fields: &[ParsedFieldInfo<'_>],
    lifetime: &Lifetime,
) -> TokenStream2 {
    let field_state_types = fields
        .iter()
        .map(|info| match *info {
            ParsedFieldInfo::Positional(PositionalFieldInfo { ty, .. })
            | ParsedFieldInfo::Flag(FlagFieldInfo { ty, .. }) => FlattenOr::Normal(ty),
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

    let position_tracker_type = match fields.iter().any(|field| match *field {
        ParsedFieldInfo::Positional(_) | ParsedFieldInfo::Flatten(_) => true,
        ParsedFieldInfo::Flag(_) => false,
    }) {
        true => quote! { u16 },
        false => quote! { () },
    };

    // TODO: if there are no flattened or positional fields, there's no need
    // for a `position` in the state. In theory we should be able to just
    // swap it out here for a `()`, because there's no codegen that interacts
    // with the field as an integer if there's no body of the positional
    // handler block.
    struct_state_block(position_tracker_type, lifetime, field_state_types)
}

/// Create a default initializer block for a structure
#[must_use]
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

/// Basically the same as `enumerate`, but the items are paired with a
/// `syn::Index` instead, suitable for use to look stuff up in a tuple.
#[must_use]
pub fn indexed<'a, T>(
    fields: &'a [T],
) -> impl Iterator<Item = (Index, &'a T)> + DoubleEndedIterator + Clone {
    fields
        .iter()
        .enumerate()
        .map(|(index, field)| (Index::from(index), field))
}

#[must_use]
fn flag_fields<'a>(
    fields: &'a [ParsedFieldInfo<'a>],
) -> impl Iterator<Item = (Index, &'a FlagFieldInfo<'a>)> {
    indexed(fields).filter_map(|(index, field)| match field {
        ParsedFieldInfo::Flag(field) => Some((index, field)),
        ParsedFieldInfo::Positional(_) | ParsedFieldInfo::Flatten(_) => None,
    })
}

#[must_use]
fn flatten_fields<'a>(
    fields: &'a [ParsedFieldInfo<'a>],
) -> impl Iterator<Item = (Index, &'a FlattenFieldInfo<'a>)> + DoubleEndedIterator {
    indexed(fields).filter_map(|(index, field)| match field {
        ParsedFieldInfo::Flatten(field) => Some((index, field)),
        ParsedFieldInfo::Flag(_) | ParsedFieldInfo::Positional(_) => None,
    })
}

#[must_use]
fn positional_flatten_fields<'a>(
    fields: &'a [ParsedFieldInfo<'a>],
) -> impl Iterator<
    Item = (
        Index,
        Literal,
        FlattenOr<&'a FlattenFieldInfo<'a>, &'a PositionalFieldInfo<'a>>,
    ),
> {
    indexed(fields)
        .filter_map(|(idx, field)| field.get_positional().map(|info| (idx, info)))
        .enumerate()
        .map(|(position, (index, field))| (index, Literal::usize_unsuffixed(position), field))
}

#[derive(Clone, Copy)]
pub enum FieldNature<'a> {
    /// Option<T>, and additionally has invert tags
    Invertable(FlagTags<&'a str, char>),

    /// Option<T>
    Optional,

    /// T
    Extant,
}

impl<'a> FieldNature<'a> {
    pub fn invert_tags(&self) -> Option<FlagTags<&'a str, char>> {
        match *self {
            FieldNature::Invertable(tags) => Some(tags),
            FieldNature::Optional | FieldNature::Extant => None,
        }
    }
}

/// Create an expression that applies an incoming `argument` to a given field
/// by calling either `Parameter::initial_method` or `Parameter::follow_up_method`,
/// depending on whether the field is present already. This expression's type
/// is always `Result<(), impl ParameterError>`. The field is
/// `#fields_ident.#field_index`.
///
/// If `Parameter::follow_up_method` is omitted, then we assume this is an
/// overridable field and that we should always use `initial_method`

#[must_use]
fn apply_arg_to_field(
    fields_ident: &Ident,
    argument_ident: &Ident,
    field_index: &Index,
    parameter_trait: &Ident,
    initial_method: &Ident,
    follow_up_method: Option<&Ident>,
    nature: &FieldNature<'_>,
) -> TokenStream2 {
    let decorated_value = match nature {
        FieldNature::Extant => quote! { value },
        FieldNature::Invertable(_) | FieldNature::Optional => {
            quote! { ::core::option::Option::Some(value) }
        }
    };

    let new_argument_expr = quote! {
        match ::debate::parameter::#parameter_trait::#initial_method(#argument_ident) {
            ::core::result::Result::Err(err) => ::core::result::Result::Err(err),
            ::core::result::Result::Ok(value) => {
                #fields_ident.#field_index = #decorated_value;
                ::core::result::Result::Ok(())
            }
        }
    };

    let Some(follow_up_method) = follow_up_method else {
        return new_argument_expr;
    };

    let existing_argument_expr = quote! {
        ::debate::parameter::#parameter_trait::#follow_up_method(
            old,
            #argument_ident,
        )
    };

    let match_body = match nature {
        FieldNature::Extant => quote! { ref mut old => #existing_argument_expr, },
        FieldNature::Invertable(_) | FieldNature::Optional => quote! {
            ::core::option::Option::None => #new_argument_expr,
            ::core::option::Option::Some(ref mut old) => #existing_argument_expr,
        },
    };

    quote! {
        match #fields_ident.#field_index { #match_body }
    }
}

/// Given an expression that returns a Result<(), E>, wrap it such that `Ok`
/// is RETURNED, `DetectUnrecognized::Unrecognized` is propagated as the value
/// of the expression, and other errors are returned as an error via `wrap_error`

#[must_use]
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

#[inline]
#[must_use]
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
#[must_use]
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
                    &format_ident!("PositionalParameter"),
                    arg_ident,
                    Some(add_arg_ident),
                    &FieldNature::Optional,
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

/// Trait to convert a flag's tag into a `Literal` expression scrutinee in
/// a `match` arm. Currently, we do bytewise matching, so strings become
/// byte arrays and characters become u8 byte literals.
pub trait MakeScrutinee {
    fn make_scrutinee(self) -> Literal;
}

impl MakeScrutinee for char {
    fn make_scrutinee(self) -> Literal {
        // I hate using `as` here, but we did already guarantee it's an ascii
        // byte, I guess? Maybe `FlagTags` can pre-compute the scrutinee.
        Literal::byte_character(u8::try_from(self).expect("short flag wasn't an ascii character"))
    }
}

impl MakeScrutinee for &str {
    fn make_scrutinee(self) -> Literal {
        Literal::byte_string(self.as_bytes())
    }
}

pub trait FlagField<'a> {
    fn name(&self) -> &str;
    fn tags(&self) -> FlagTags<&'a str, char>;
    fn overridable(&self) -> bool;
    fn nature(&self) -> FieldNature<'a>;
}

impl<'a> FlagField<'a> for &'a FlagFieldInfo<'_> {
    #[inline(always)]
    fn name(&self) -> &str {
        self.ident.as_str()
    }

    #[inline(always)]
    fn tags(&self) -> FlagTags<&'a str, char> {
        self.tags.simplify()
    }

    #[inline(always)]
    fn overridable(&self) -> bool {
        self.overridable
    }

    #[inline(always)]
    fn nature(&self) -> FieldNature<'a> {
        match self.invert {
            Some(ref long) => FieldNature::Invertable(FlagTags::Long(long)),
            None => FieldNature::Optional,
        }
    }
}

/// Shared logic for creating the match block that handles options for
/// pretty much anything. Abstracts over struct-like entities and flagset
/// logic.
#[expect(clippy::too_many_arguments)]
// Might swap this out for a struct someday, to make it easier to read.
// This versuon is kinda nice cause you don't have to have explicit generic
// parameters for all the `impl Trait` arguments.
#[must_use]
pub fn complete_flag_body<'a, Flag: FlagField<'a>, Tag: MakeScrutinee>(
    fields_ident: &Ident,
    argument_ident: &Ident,
    option_expr: impl ToTokens,

    help: Option<(Tag, HelpMode)>,
    flag_fields: impl Iterator<Item = (Index, Flag)>,
    get_tag: impl Fn(FlagTags<&'a str, char>) -> Option<Tag>,

    parameter_method: &Ident,
    add_parameter_method: &Ident,

    conflict_arm: TokenStream2,
    flatten_exprs: impl IntoIterator<Item = TokenStream2>,
    flatten_rebind_argument: impl ToTokens,
) -> TokenStream2 {
    let help_arm = help.map(|(help, mode)| {
        let help = help.make_scrutinee();
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

    let parameter = format_ident!("Parameter");

    let local_arms = flag_fields.flat_map(|(index, field)| {
        let field_name = field.name();
        let get_tag = &get_tag;
        let nature = field.nature();

        let arm = get_tag(field.tags()).map(|tag| {
            let scrutinee = tag.make_scrutinee();
            let expr = apply_arg_to_field(
                fields_ident,
                argument_ident,
                &index,
                &parameter,
                parameter_method,
                match field.overridable() {
                    true => None,
                    false => Some(add_parameter_method),
                },
                &nature,
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

        // We assume that a flag with an Extant nature never has an invert,
        // since the whole point of invert is to reset the flag to None.
        let invert_arm = nature.invert_tags().and_then(get_tag).map(|tag| {
            let scrutinee = tag.make_scrutinee();
            quote! {
                #scrutinee => match ::debate::parameter::Parameter::#parameter_method(
                    #argument_ident
                ) {
                    // TODO: deduplicate this with `apply_arg_to_field`
                    ::core::result::Result::Ok(()) => {
                        #fields_ident.#index = ::core::option::Option::None;
                        ::core::result::Result::Ok(())
                    }
                    ::core::result::Result::Err(err) => ::core::result::Result::Err(
                        ::debate::state::Error::parameter(#field_name, err)
                    )
                },
            }
        });

        [arm, invert_arm]
    });

    let flatten_exprs = flatten_exprs.into_iter();

    quote! {
        match (#option_expr) {
            #help_arm
            #(#local_arms)*
            _ => {
                #(
                    let #flatten_rebind_argument = #flatten_exprs;
                )*

                match (#option_expr) {
                    #conflict_arm
                    _ => ::core::result::Result::Err(
                        ::debate::state::Error::unrecognized(
                            #flatten_rebind_argument
                        )
                    )
                }
            }
        }
    }
}

/// Shared logic for creating the match block that handles options for
/// struct-like entities
#[expect(clippy::too_many_arguments)]
#[inline]
#[must_use]
fn complete_struct_flag_body<'a, Tag: MakeScrutinee>(
    fields_ident: &Ident,
    argument_ident: &Ident,
    option_expr: impl ToTokens,

    fields: &'a [ParsedFieldInfo<'_>],
    help: Option<(Tag, HelpMode)>,
    get_tag: impl Fn(FlagTags<&'a str, char>) -> Option<Tag>,

    parameter_method: &Ident,
    add_parameter_method: &Ident,
    flatten_state_method: &Ident,
    flatten_rebind_argument: impl ToTokens,
) -> TokenStream2 {
    let flatten_exprs = flatten_fields(fields).map(|(index, info)| {
        handle_flatten(
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
        )
    });

    complete_flag_body(
        fields_ident,
        argument_ident,
        option_expr,
        help,
        flag_fields(fields),
        get_tag,
        parameter_method,
        add_parameter_method,
        quote! {},
        flatten_exprs,
        flatten_rebind_argument,
    )
}

#[inline]
#[must_use]
pub fn complete_long_arg_body(
    fields_ident: &Ident,
    argument_ident: &Ident,
    option_ident: &Ident,

    fields: &[ParsedFieldInfo<'_>],
    help: Option<&str>,
) -> TokenStream2 {
    complete_struct_flag_body(
        fields_ident,
        argument_ident,
        quote! { #option_ident.bytes() },
        fields,
        help.map(|long| (long, HelpMode::Full)),
        |tags| tags.long(),
        &format_ident!("arg"),
        &format_ident!("add_arg"),
        &format_ident!("add_long_argument"),
        quote! { () },
    )
}

#[inline]
#[must_use]
pub fn complete_long_body(
    fields_ident: &Ident,
    argument_ident: &Ident,
    option_ident: &Ident,

    fields: &[ParsedFieldInfo<'_>],
    help: Option<&str>,
) -> TokenStream2 {
    complete_struct_flag_body(
        fields_ident,
        argument_ident,
        quote! { #option_ident.bytes() },
        fields,
        help.map(|long| (long, HelpMode::Full)),
        |tags| tags.long(),
        &format_ident!("present"),
        &format_ident!("add_present"),
        &format_ident!("add_long"),
        argument_ident,
    )
}

#[inline]
#[must_use]
pub fn complete_short_body(
    fields_ident: &Ident,
    argument_ident: &Ident,
    option_ident: &Ident,

    fields: &[ParsedFieldInfo<'_>],
    help: Option<char>,
) -> TokenStream2 {
    complete_struct_flag_body(
        fields_ident,
        argument_ident,
        option_ident,
        fields,
        help.map(|short| (short, HelpMode::Succinct)),
        |tags| tags.short(),
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

    indexed(fields)
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
                    ParsedFieldInfo::Flag(field) => FlattenOr::Normal(NormalFieldInfo {
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
