use std::collections::{HashMap, hash_map::Entry};
use std::fmt::Display;
use std::hash::Hash;

use darling::util::SpannedValue;
use itertools::Itertools as _;
use lazy_format::lazy_format;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{Attribute, Field, Generics, Ident, Token, punctuated::Punctuated};

use crate::from_args::common::{
    ParsedFieldInfo, complete_long_body, complete_long_option_body, complete_short_body,
    final_field_initializers, struct_state_block_from_fields, struct_state_init_block_from_fields,
    visit_positional_arms_for_fields,
};
use crate::generics::compute_generics;

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
    let computed_generics = compute_generics(
        &generics,
        fields.iter().map(|field| &field.ty),
        |lt| quote! { ::debate::build::BuildFromArgs<#lt> },
    )?;

    let impl_generics = &computed_generics.impl_block_generics;
    let state_type_generics = &computed_generics.type_generics_with_lt;
    let where_clause = &computed_generics.where_clause;

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
        &computed_generics,
        |state| {
            let state_block = struct_state_block_from_fields(&fields);
            let state_init_block = struct_state_init_block_from_fields(&fields);

            quote! {
                #[doc(hidden)]
                struct #state #impl_generics #where_clause #state_block

                // Sadly we can't derive debug, because default is only implemented for
                // tuples up to 12 elements.
                impl #impl_generics ::core::default::Default for #state #state_type_generics
                    #where_clause
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
