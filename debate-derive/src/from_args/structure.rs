use std::collections::{HashMap, hash_map::Entry};
use std::fmt::Display;
use std::hash::Hash;

use darling::util::SpannedValue;
use itertools::Itertools as _;
use lazy_format::lazy_format;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::Lifetime;
use syn::{Attribute, Field, Generics, Ident, Token, punctuated::Punctuated};

use crate::common::ParsedFieldInfo;
use crate::from_args::common::{
    complete_long_body, complete_long_option_body, complete_short_body, final_field_initializers,
    struct_state_block_from_fields, struct_state_init_block_from_fields,
    visit_positional_arms_for_fields,
};
use crate::generics::{AngleBracedLifetime, compute_generics};

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
    lifetime: &Lifetime,
    type_lifetime: Option<&AngleBracedLifetime>,
    attrs: &[Attribute],
) -> syn::Result<TokenStream2> {
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

    let parameter_ident = format_ident!("Parameter");
    let positional_parameter_ident = format_ident!("PositionalParameter");

    Ok(super::from_args_impl(
        name,
        lifetime,
        type_lifetime,
        |state| {
            let state_block = struct_state_block_from_fields(&fields);
            let state_init_block = struct_state_init_block_from_fields(&fields);

            quote! {
                #[doc(hidden)]
                struct #state <#lifetime> #state_block

                // Sadly we can't derive debug, because default is only implemented for
                // tuples up to 12 elements.
                impl<#lifetime> ::core::default::Default for #state <#lifetime>
                {
                    fn default() -> Self {
                        Self #state_init_block
                    }
                }
            }
        },
        |argument| {
            let visit_positional_arms = visit_positional_arms_for_fields(
                &fields_ident,
                argument,
                &positional_parameter_ident,
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
            let body = complete_long_option_body(
                &fields_ident,
                argument,
                option,
                &parameter_ident,
                &fields,
            );

            quote! {
                let fields = &mut self.fields;

                #body
            }
        },
        |option, argument| {
            let body =
                complete_long_body(&fields_ident, argument, option, &parameter_ident, &fields);

            quote! {
                let fields = &mut self.fields;

                #body
            }
        },
        |option, argument| {
            let body =
                complete_short_body(&fields_ident, argument, option, &parameter_ident, &fields);

            quote! {
                let fields = &mut self.fields;

                #body
            }
        },
        |state| {
            let final_field_initializers = final_field_initializers(&fields_ident, &fields);

            // TODO: support for tuple structs here
            quote! {
                let #fields_ident = #state.fields;

                ::core::result::Result::Ok(Self {
                    #(#final_field_initializers,)*
                })
            }
        },
    ))
}
