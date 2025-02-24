use std::collections::{HashMap, hash_map::Entry};
use std::fmt::Display;
use std::hash::Hash;

use darling::util::SpannedValue;
use itertools::Itertools as _;
use lazy_format::lazy_format;
use proc_macro2::{Literal, Span, TokenStream as TokenStream2};
use quote::{ToTokens, format_ident, quote};
use syn::{Attribute, Field, Generics, Ident, Index, Token, Type, punctuated::Punctuated};

use super::common::{
    FieldDefault, FlattenFieldInfo, FlattenOr, IdentString, OptionFieldInfo, ParsedFieldInfo,
    PositionalFieldInfo, apply_arg_to_field, handle_flatten, struct_state_block_from_fields,
    struct_state_init_block_from_fields, visit_positional_arms_for_fields,
};

/// Create match arms for long_option, long, and short
fn create_local_option_arms<'a>(
    fields: impl IntoIterator<Item = (Index, &'a OptionFieldInfo<'a>)>,
    make_scrutinee: impl Fn(&'a OptionFieldInfo<'a>) -> Option<Literal>,
    initial_method: &Ident,
    follow_up_method: &Ident,
) -> impl Iterator<Item = impl ToTokens> {
    fields
        .into_iter()
        .filter_map(move |(index, field)| {
            make_scrutinee(field).map(|scrutinee| (field, scrutinee, index))
        })
        .map(move |(info, scrutinee, index)| {
            let field_name = info.ident.as_str();

            let expr = apply_arg_to_field(&index, initial_method, follow_up_method);

            quote! {
                #scrutinee => match (#expr) {
                    ::core::result::Result::Ok(()) => ::core::result::Result::Ok(()),
                    ::core::result::Result::Err(err) => ::core::result::Result::Err(
                        ::debate::state::Error::parameter(#field_name, err)
                    ),
                },
            }
        })
}

fn detect_collision<T: Hash + Eq + Copy, M: Display>(
    known_tags: &mut HashMap<T, Span>,
    new_tag: Option<SpannedValue<T>>,
    message: impl Fn(T) -> M,
) -> syn::Result<()> {
    match new_tag {
        Some(tag) => match known_tags.entry(*tag) {
            Entry::Occupied(entry) => {
                let mut err1 = syn::Error::new(
                    tag.span(),
                    lazy_format!("duplicate option {tag}", tag = message(*tag)),
                );
                let err2 = syn::Error::new(*entry.get(), "original use here");

                err1.combine(err2);
                Err(err1)
            }
            Entry::Vacant(entry) => {
                entry.insert(tag.span());
                Ok(())
            }
        },
        None => Ok(()),
    }
}

pub fn derive_args_struct(
    name: &Ident,
    fields: &Punctuated<Field, Token![,]>,
    generics: &Generics,
    attrs: &[Attribute],
) -> syn::Result<TokenStream2> {
    // TODO: handle generics. My current idea is this:
    // - enforce at most one lifetime. Not much reason I can see to allow an
    //   opt-out of this. We assume that lifetime is the 'arg lifetime.
    // - detect the presence of type and const parameters in the subtypes.
    //   add where bounds as needed for these: `where #field_type: Parameter<'arg>`
    //   (or `BuildFromArgs<'arg>`)
    // - handle the basics: reproduce the existing bounds
    let fields: Vec<ParsedFieldInfo> = fields
        .iter()
        .map(ParsedFieldInfo::from_field)
        .try_collect()?;

    // Collision detection
    {
        let mut long_tags = HashMap::new();
        let mut short_tags = HashMap::new();

        for tags in fields.iter().filter_map(|field| match field {
            ParsedFieldInfo::Option(option) => Some(&option.tags),
            ParsedFieldInfo::Positional(_) | ParsedFieldInfo::Flatten(_) => None,
        }) {
            detect_collision(&mut long_tags, tags.long(), |tag| lazy_format!("--{tag}"))?;
            detect_collision(&mut short_tags, tags.short(), |tag| lazy_format!("-{tag}"))?;
        }
    }

    let state_block = struct_state_block_from_fields(&fields);
    let state_init_block = struct_state_init_block_from_fields(&fields);

    // Reuse these everywhere
    let arg_ident = format_ident!("arg");
    let present_ident = format_ident!("present");
    let add_arg_ident = format_ident!("add_arg");
    let add_present_ident = format_ident!("add_present");

    let argument_ident = format_ident!("argument");

    let visit_positional_arms =
        visit_positional_arms_for_fields(&arg_ident, &add_arg_ident, &fields);

    let option_fields = fields
        .iter()
        .enumerate()
        .filter_map(|(idx, info)| match info {
            ParsedFieldInfo::Option(info) => Some((Index::from(idx), info)),
            ParsedFieldInfo::Positional(_) | ParsedFieldInfo::Flatten(_) => None,
        });

    let flatten_fields = fields
        .iter()
        .enumerate()
        .filter_map(|(idx, info)| match info {
            ParsedFieldInfo::Flatten(info) => Some((Index::from(idx), info)),
            ParsedFieldInfo::Option(_) | ParsedFieldInfo::Positional(_) => None,
        });

    let visit_long_option_arms = create_local_option_arms(
        option_fields.clone(),
        |info| {
            info.tags
                .long()
                .map(|long| Literal::byte_string(long.as_bytes()))
        },
        &arg_ident,
        &add_arg_ident,
    );

    let visit_long_option_flattened_arms = flatten_fields.clone().map(|(index, info)| {
        let expr = quote! {
            ::debate::state::State::add_long_option(
                &mut fields.#index,
                option,
                argument
            )
        };

        handle_flatten(expr, info.ident_str(), quote! { () }, quote! { {} })
    });

    let visit_long_arms = create_local_option_arms(
        option_fields.clone(),
        |info| {
            info.tags
                .long()
                .map(|long| Literal::byte_string(long.as_bytes()))
        },
        &present_ident,
        &add_present_ident,
    );

    let visit_long_flattened_arms = flatten_fields.clone().map(|(index, info)| {
        let expr = quote! {
            ::debate::state::State::add_long(
                &mut fields.#index,
                option,
                argument
            )
        };
        let body = handle_flatten(expr, info.ident_str(), &argument_ident, &argument_ident);

        quote! {
            let #argument_ident = #body;
        }
    });

    let visit_short_arms = create_local_option_arms(
        option_fields,
        |info| {
            info.tags
                .short()
                .map(|short| Literal::byte_character(*short as u8))
        },
        &present_ident,
        &add_present_ident,
    );

    let visit_short_flattened_arms = flatten_fields.map(|(index, info)| {
        let expr = quote! {
            ::debate::state::State::add_short(
                &mut fields.#index,
                option,
                argument,
            )
        };

        let body = handle_flatten(expr, info.ident_str(), &argument_ident, &argument_ident);

        quote! {
            let #argument_ident = #body;
        }
    });

    let final_field_initializers = fields
        .iter()
        .map(|info| match info {
            ParsedFieldInfo::Positional(info) => {
                FlattenOr::Normal((None, None, &info.ident, &info.default))
            }
            ParsedFieldInfo::Option(info) => FlattenOr::Normal((
                info.tags.long(),
                info.tags.short(),
                &info.ident,
                &info.default,
            )),
            ParsedFieldInfo::Flatten(flatten_field_info) => FlattenOr::Flatten(flatten_field_info),
        })
        .enumerate()
        .map(|(idx, field)| (Index::from(idx), field))
        .map(|(idx, field)| match field {
            FlattenOr::Normal((long, short, ident, default)) => {
                let field_ident_str = ident.as_str();

                let long = match long.as_deref() {
                    Some(&long) => quote! {::core::option::Option::Some(#long)},
                    None => quote! {::core::option::Option::None},
                };

                let short = match short.as_deref() {
                    Some(&short) => quote! {::core::option::Option::Some(#short)},
                    None => quote! {::core::option::Option::None},
                };

                let default = match default {
                    FieldDefault::Expr(expr) => quote! { #expr },
                    FieldDefault::Trait => quote! { ::core::default::Default::default() },
                    FieldDefault::None => quote! { match ::debate::parameter::Parameter::absent() {
                        ::core::result::Result::Ok(value) => value,
                        ::core::result::Result::Err(::debate::parameter::RequiredError) =>
                            return ::core::result::Result::Err(
                                ::debate::from_args::Error::required(
                                    #field_ident_str, #long, #short
                                )
                            ),
                    }},
                };

                quote! {
                    #ident: match fields.#idx {
                        ::core::option::Option::Some(value) => value,
                        ::core::option::Option::None => #default,
                    },
                }
            }
            FlattenOr::Flatten(FlattenFieldInfo { ident, .. }) => {
                let expr = quote! {
                    match ::debate::from_args::BuildFromArgs::build(
                        fields.#idx
                    ) {
                        ::core::result::Result::Ok(value) => value,
                        ::core::result::Result::Err(err) => return ::core::result::Result::Err(err),
                    },
                };

                match ident {
                    Some(ident) => quote! { #ident : #expr },
                    None => expr,
                }
            }
        });

    let state_ident = format_ident!("__{name}State");

    Ok(quote! {
        #[doc(hidden)]
        struct #state_ident<'arg> #state_block

        // Sadly we can't derive debug, because default is only implemented for
        // tuples up to 12 elements.
        impl<'arg> ::core::default::Default for #state_ident<'arg> {
            fn default() -> Self {
                Self #state_init_block
            }
        }

        impl<'arg> ::debate::state::State<'arg> for #state_ident<'arg> {
            fn add_positional<E>(
                &mut self,
                argument: ::debate_parser::Arg<'arg>
            ) -> ::core::result::Result<(), E>
            where
                E: ::debate::state::Error<'arg, ()>
            {
                let fields = &mut self.fields;
                let position = &mut self.position;

                #(#visit_positional_arms)*

                ::core::result::Result::Err(
                    ::debate::state::Error::unrecognized(())
                )
            }

            fn add_long_option<E>(
                &mut self,
                option: ::debate_parser::Arg<'arg>,
                argument: ::debate_parser::Arg<'arg>
            ) -> ::core::result::Result<(), E>
            where
                E: ::debate::state::Error<'arg, ()>
            {
                let fields = &mut self.fields;
                let position = &mut self.position;

                match option.bytes() {
                    #(#visit_long_option_arms)*
                    _ => {
                        #(#visit_long_option_flattened_arms)*

                        ::core::result::Result::Err(
                            ::debate::state::Error::unrecognized(())
                        )
                    }
                }
            }

            fn add_long<A, E>(
                &mut self,
                option: ::debate_parser::Arg<'arg>,
                argument: A
            ) -> ::core::result::Result<(), E>
            where
                A: ::debate_parser::ArgAccess<'arg>,
                E: ::debate::state::Error<'arg, A>
            {
                let fields = &mut self.fields;
                let position = &mut self.position;

                match option.bytes() {
                    #(#visit_long_arms)*
                    _ => {
                        #(#visit_long_flattened_arms)*

                        ::core::result::Result::Err(
                            ::debate::state::Error::unrecognized(
                                argument
                            )
                        )
                    }
                }
            }

            fn add_short<A, E>(
                &mut self,
                option: u8,
                argument: A
            ) -> ::core::result::Result<(), E>
            where
                A: ::debate_parser::ArgAccess<'arg>,
                E: ::debate::state::Error<'arg, A>
            {
                let fields = &mut self.fields;
                let position = &mut self.position;

                match option {
                    #(#visit_short_arms)*
                    _ => {
                        #(#visit_short_flattened_arms)*

                        ::core::result::Result::Err(
                            ::debate::state::Error::unrecognized(
                                argument
                            )
                        )
                    }
                }
            }
        }

        impl<'arg> ::debate::from_args::BuildFromArgs<'arg> for #name {
            type State = #state_ident<'arg>;

            fn build<E>(state: Self::State) -> ::core::result::Result<Self, E>
            where
                E: ::debate::from_args::Error<'arg>
            {
                let fields = state.fields;

                ::core::result::Result::Ok(Self {
                    #(#final_field_initializers)*
                })
            }
        }
    })
}
