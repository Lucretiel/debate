use std::collections::{HashMap, hash_map::Entry};
use std::fmt::Display;
use std::hash::Hash;

use darling::util::SpannedValue;
use itertools::Itertools as _;
use lazy_format::lazy_format;
use proc_macro2::{Literal, Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{Attribute, Field, Generics, Ident, Index, Token, punctuated::Punctuated};

use crate::from_args::common::{
    complete_long_body, complete_long_option_body, complete_short_body, flatten_fields,
    option_fields,
};

use super::common::{
    FieldDefault, FlattenFieldInfo, FlattenOr, ParsedFieldInfo, handle_flatten,
    struct_state_block_from_fields, struct_state_init_block_from_fields,
    visit_positional_arms_for_fields,
};

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

    // Reuse these everywhere
    let arg_ident = format_ident!("arg");
    let add_arg_ident = format_ident!("add_arg");

    let fields_ident = format_ident!("fields");

    Ok(super::from_args_impl(
        name,
        |state, lifetime| {
            let state_block = struct_state_block_from_fields(&fields);
            let state_init_block = struct_state_init_block_from_fields(&fields);

            quote! {
                #[doc(hidden)]
                struct #state<#lifetime> #state_block

                // Sadly we can't derive debug, because default is only implemented for
                // tuples up to 12 elements.
                impl<#lifetime> ::core::default::Default for #state<#lifetime> {
                    fn default() -> Self {
                        Self #state_init_block
                    }
                }
            }
        },
        |state, lifetime| quote! { #state<#lifetime> },
        |argument| {
            let visit_positional_arms = visit_positional_arms_for_fields(
                &fields_ident,
                argument,
                &arg_ident,
                &add_arg_ident,
                &fields,
            );

            quote! {
                let #fields_ident = &mut self.fields;
                let position = &mut self.position;

                #(#visit_positional_arms)*

                ::core::result::Result::Err(
                    ::debate::state::Error::unrecognized(())
                )
            }
        },
        |option, argument| {
            let body = complete_long_option_body(&fields_ident, argument, option, &fields);

            quote! {
                let fields = &mut self.fields;

                #body
            }
        },
        |option, argument| {
            let body = complete_long_body(&fields_ident, argument, option, &fields);

            quote! {
                let fields = &mut self.fields;

                #body
            }
        },
        |option, argument| {
            let body = complete_short_body(&fields_ident, argument, option, &fields);

            quote! {
                let fields = &mut self.fields;

                #body
            }
        },
        |state| {
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
                    ParsedFieldInfo::Flatten(flatten_field_info) => {
                        FlattenOr::Flatten(flatten_field_info)
                    }
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
                            FieldDefault::None => quote! {
                            match ::debate::parameter::Parameter::absent() {
                                ::core::result::Result::Ok(value) => value,
                                ::core::result::Result::Err(::debate::parameter::RequiredError) =>
                                    return ::core::result::Result::Err(
                                        ::debate::from_args::Error::required(
                                            #field_ident_str, #long, #short
                                        )
                                    ),
                                }
                            },
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
                                ::core::result::Result::Err(err) => return (
                                    ::core::result::Result::Err(err)
                                ),
                            },
                        };

                        match ident {
                            Some(ident) => quote! { #ident : #expr },
                            None => expr,
                        }
                    }
                });

            quote! {
                let fields = #state.fields;

                ::core::result::Result::Ok(Self {
                    #(#final_field_initializers)*
                })
            }
        },
    ))
}
