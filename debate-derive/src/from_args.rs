pub mod common;
pub mod enumeration;
pub mod structure;

use itertools::Itertools;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{DeriveInput, Fields, Ident, spanned::Spanned as _};
use syn::{Lifetime, parse_quote};

use self::enumeration::derive_args_enum;
use self::structure::derive_args_struct;

pub fn derive_args_result(item: TokenStream2) -> syn::Result<TokenStream2> {
    let input: DeriveInput = syn::parse2(item)?;

    if let Some(param) = input.generics.const_params().next() {
        return Err(syn::Error::new(
            param.span(),
            "const generics aren't supported by `derive(FromArgs)`",
        ));
    }

    if let Some(param) = input.generics.type_params().next() {
        return Err(syn::Error::new(
            param.span(),
            "generic types aren't supported by `derive(FromArgs)`",
        ));
    }

    let lifetime = input.generics.lifetimes().at_most_one().map_err(|_| {
        syn::Error::new(
            input.generics.span(),
            "`derive(FromArgs)` type may have at most one lifetime, for borrowed CLI args",
        )
    })?;

    match input.data {
        syn::Data::Struct(ref data) => derive_args_struct(
            &input.ident,
            match data.fields {
                Fields::Named(ref fields) => &fields.named,
                Fields::Unnamed(ref fields) => &fields.unnamed,
                Fields::Unit => {
                    return Err(syn::Error::new(
                        input.span(),
                        "can't derive `FromArgs` on a unit struct",
                    ));
                }
            },
            &input.generics,
            &input.attrs,
        ),
        syn::Data::Enum(ref data) => {
            derive_args_enum(&input.ident, &data.variants, &input.generics, &input.attrs)
        }
        syn::Data::Union(_) => {
            return Err(syn::Error::new(
                input.span(),
                "can't derive `FromArgs` on a union",
            ));
        }
    }
}

// TODO: generics
fn from_args_impl(
    ident: &Ident,

    state_definition: impl FnOnce(&Ident, &Lifetime) -> TokenStream2,
    state_type: impl FnOnce(&Ident, &Lifetime) -> TokenStream2,

    add_positional_body: impl FnOnce(&Ident) -> TokenStream2,
    add_long_option_body: impl FnOnce(&Ident, &Ident) -> TokenStream2,
    add_long_body: impl FnOnce(&Ident, &Ident) -> TokenStream2,
    add_short_body: impl FnOnce(&Ident, &Ident) -> TokenStream2,

    // State type, state variable
    build_body: impl FnOnce(&Ident) -> TokenStream2,
) -> TokenStream2 {
    let state_ident = format_ident!("__{ident}State");
    let lifetime = parse_quote!('arg);
    let argument = format_ident!("argument");
    let option = format_ident!("option");
    let state = format_ident!("state");

    let state_definition = state_definition(&state_ident, &lifetime);
    let state_type = state_type(&state_ident, &lifetime);

    let add_positional_body = add_positional_body(&argument);
    let add_long_option_body = add_long_option_body(&option, &argument);
    let add_long_body = add_long_body(&option, &argument);
    let add_short_body = add_short_body(&option, &argument);

    let build_body = build_body(&state);

    quote! {
        #state_definition

        impl<#lifetime> ::debate::state::State<#lifetime> for #state_type {
            fn add_positional<E>(
                &mut self,
                #argument: ::debate_parser::Arg<#lifetime>
            ) -> ::core::result::Result<(), E>
            where
                E: ::debate::state::Error<#lifetime, ()>
            {
                #add_positional_body
            }

            fn add_long_option<E>(
                &mut self,
                option: ::debate_parser::Arg<#lifetime>,
                argument: ::debate_parser::Arg<#lifetime>
            ) -> ::core::result::Result<(), E>
            where
                E: ::debate::state::Error<#lifetime, ()>
            {
                #add_long_option_body
            }

            fn add_long<A, E>(
                &mut self,
                option: ::debate_parser::Arg<#lifetime>,
                argument: A
            ) -> ::core::result::Result<(), E>
            where
                A: ::debate_parser::ArgAccess<#lifetime>,
                E: ::debate::state::Error<#lifetime, A>
            {
                #add_long_body
            }

            fn add_short<A, E>(
                &mut self,
                option: u8,
                argument: A
            ) -> ::core::result::Result<(), E>
            where
                A: ::debate_parser::ArgAccess<#lifetime>,
                E: ::debate::state::Error<#lifetime, A>
            {
                #add_short_body
            }
        }

        impl<#lifetime> ::debate::from_args::BuildFromArgs<#lifetime> for #ident {
            type State = #state_type;

            fn build<E>(state: Self::State) -> ::core::result::Result<Self, E>
            where
                E: ::debate::from_args::Error<#lifetime>
            {
                #build_body
            }
        }
    }
}
