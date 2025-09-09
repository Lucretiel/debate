use std::collections::{HashMap, hash_map::Entry};
use std::fmt::Display;
use std::hash::Hash;

use darling::FromAttributes;
use darling::util::SpannedValue;
use itertools::Itertools as _;
use lazy_format::lazy_format;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::Lifetime;
use syn::{Attribute, Field, Ident, Token, punctuated::Punctuated};

use crate::common::{ParsedFieldInfo, RawParsedTypeAttr};
use crate::from_args::common::{
    complete_long_arg_body, complete_long_body, complete_short_body, final_field_initializers,
    get_subcommand_field_visitor_calls, struct_state_block_from_fields,
    struct_state_init_block_from_field_count, visit_positional_arms_for_fields,
};
use crate::generics::AngleBracedLifetime;

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
    let attr = RawParsedTypeAttr::from_attributes(attrs)?;
    let help = attr.help_option();

    let help_long = help.as_ref().and_then(|help| {
        help.tags
            .long()
            .map(|long| SpannedValue::new(long, help.span))
    });
    let help_short = help.as_ref().and_then(|help| {
        help.tags
            .short()
            .map(|short| SpannedValue::new(short, help.span))
    });

    let fields: Vec<ParsedFieldInfo> = fields
        .iter()
        .map(ParsedFieldInfo::from_field)
        .try_collect()?;

    // Collision detection
    // TODO: move collision detection to ParsedFieldInfo::from_field. This will
    //   allow us to do collision detection in enums and derive(Usage)
    {
        let mut long_tags = HashMap::from_iter(help_long.map(|long| (*long, long.span())));
        let mut short_tags = HashMap::from_iter(help_short.map(|short| (*short, short.span())));

        for flag in fields.iter().filter_map(|field| match field {
            ParsedFieldInfo::Flag(flag) => Some(flag),
            ParsedFieldInfo::Positional(_) | ParsedFieldInfo::Flatten(_) => None,
        }) {
            // TODO: I'm not totally satisfied with the way collision error
            // locations are reported here. Might need to introduce the idea
            // of a double-span for things like `#[debate(short)]foo: i32``,
            // where we can track both the activation of the feature `short`
            // and the specific name `foo`.
            detect_collision(&mut long_tags, flag.tags.long(), |tag| {
                lazy_format!("--{tag}")
            })?;
            detect_collision(
                &mut long_tags,
                flag.invert
                    .as_ref()
                    .map(|invert| SpannedValue::new(invert.as_str(), invert.span())),
                |tag| lazy_format!("--{tag}"),
            )?;
            detect_collision(&mut short_tags, flag.tags.short(), |tag| {
                lazy_format!("-{tag}")
            })?;
        }
    }

    // Reuse these everywhere
    let arg_ident = format_ident!("arg");
    let add_arg_ident = format_ident!("add_arg");
    let fields_ident = format_ident!("fields");

    let parameter_ident = format_ident!("Parameter");
    let positional_parameter_ident = format_ident!("PositionalParameter");

    let state_ident = format_ident!("__{name}State");
    let argument = format_ident!("argument");
    let option = format_ident!("option");
    let visitor = format_ident!("visitor");

    let state_block = struct_state_block_from_fields(&fields, lifetime);
    let state_init_block = struct_state_init_block_from_field_count(fields.len());

    let visit_positional_arms = visit_positional_arms_for_fields(
        &fields_ident,
        &argument,
        &positional_parameter_ident,
        &arg_ident,
        &add_arg_ident,
        &fields,
    );

    let long_option_body = complete_long_arg_body(
        &fields_ident,
        &argument,
        &option,
        &parameter_ident,
        &fields,
        help_long.map(|help| *help),
    );

    let long_body = complete_long_body(
        &fields_ident,
        &argument,
        &option,
        &parameter_ident,
        &fields,
        help_long.map(|help| *help),
    );

    let short_body = complete_short_body(
        &fields_ident,
        &argument,
        &option,
        &parameter_ident,
        &fields,
        help_short.map(|help| *help),
    );

    let subcommand_context_visitor_calls =
        get_subcommand_field_visitor_calls(&fields_ident, &visitor, &fields);

    let final_field_initializers = final_field_initializers(&fields_ident, &fields);

    Ok(quote! {
        #[doc(hidden)]
        struct #state_ident <#lifetime> #state_block

        // Sadly we can't derive debug, because default is only implemented for
        // tuples up to 12 elements.
        impl<#lifetime> ::core::default::Default for #state_ident <#lifetime> {
            fn default() -> Self {
                Self #state_init_block
            }
        }

        impl<#lifetime> ::debate::state::State<#lifetime> for #state_ident <#lifetime> {
            fn add_positional<E>(
                &mut self,
                #argument: & #lifetime ::debate_parser::Arg
            ) -> ::core::result::Result<(), E>
            where
                E: ::debate::state::Error<#lifetime, ()>
            {
                let #fields_ident = &mut self.fields;
                let position = &mut self.position;

                #(#visit_positional_arms)*

                ::core::result::Result::Err(
                    ::debate::state::Error::unrecognized(())
                )
            }

            fn add_long_argument<E>(
                &mut self,
                option: & #lifetime ::debate_parser::Arg,
                argument: & #lifetime ::debate_parser::Arg
            ) -> ::core::result::Result<(), E>
            where
                E: ::debate::state::Error<#lifetime, ()>
            {
                let #fields_ident = &mut self.fields;

                #long_option_body
            }

            fn add_long<A, E>(
                &mut self,
                option: & #lifetime ::debate_parser::Arg,
                argument: A
            ) -> ::core::result::Result<(), E>
            where
                A: ::debate::parameter::ArgAccess<#lifetime>,
                E: ::debate::state::Error<#lifetime, A>
            {
                let #fields_ident = &mut self.fields;

                #long_body
            }

            fn add_short<A, E>(
                &mut self,
                option: u8,
                argument: A
            ) -> ::core::result::Result<(), E>
            where
                A: ::debate::parameter::ArgAccess<#lifetime>,
                E: ::debate::state::Error<#lifetime, A>
            {
                let #fields_ident = &mut self.fields;

                #short_body
            }

            fn get_subcommand_path<V: ::debate::state::SubcommandVisitor>(
                &self,
                #visitor: V
            ) -> ::core::result::Result<V::Output, V> {
                let #fields_ident = &self.fields;

                #(#subcommand_context_visitor_calls)*

                let _ = #fields_ident;

                ::core::result::Result::Err(visitor)
            }
        }

        impl<#lifetime> ::debate::build::BuildFromArgs<#lifetime> for #name #type_lifetime {
            type State = #state_ident <#lifetime>;

            fn build<E>(state: Self::State) -> ::core::result::Result<Self,E>
            where
                E: ::debate::build::Error
            {
                let #fields_ident = state.fields;

                ::core::result::Result::Ok(Self {
                    #(#final_field_initializers,)*
                })
            }
        }
    })
}

pub fn derive_args_newtype_struct(
    name: &Ident,
    field: &Field,
    lifetime: &Lifetime,
    type_lifetime: Option<&AngleBracedLifetime>,
) -> syn::Result<TokenStream2> {
    let inner_ty = &field.ty;

    Ok(quote! {
        impl<#lifetime> ::debate::build::BuildFromArgs<#lifetime> for #name #type_lifetime {
            type State = <#inner_ty as ::debate::build::BuildFromArgs<#lifetime>>::State;

            fn build<E>(state: Self::State) -> ::core::result::Result<Self, E>
            where
                E: ::debate::build::Error
            {
                match ::debate::build::BuildFromArgs::build(state) {
                    Ok(inner) => Ok(Self(inner)),
                    Err(err) => Err(err),
                }
            }
        }
    })
}
