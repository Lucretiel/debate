pub mod common;
pub mod enumeration;
pub mod structure;

use itertools::Itertools;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{DeriveInput, Fields, Ident, spanned::Spanned as _};
use syn::{Lifetime, parse_quote};

use crate::generics::ComputedGenerics;

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
        syn::Data::Union(_) => Err(syn::Error::new(
            input.span(),
            "can't derive `FromArgs` on a union",
        )),
    }
}

fn from_args_impl(
    ident: &Ident,
    generics: &ComputedGenerics,

    state_definition: impl FnOnce(&Ident) -> TokenStream2,

    add_positional_body: impl FnOnce(&Ident) -> TokenStream2,
    add_long_option_body: impl FnOnce(&Ident, &Ident) -> TokenStream2,
    add_long_body: impl FnOnce(&Ident, &Ident) -> TokenStream2,
    add_short_body: impl FnOnce(&Ident, &Ident) -> TokenStream2,

    // State type, state variable
    build_body: impl FnOnce(&Ident) -> TokenStream2,
) -> TokenStream2 {
    let lifetime = &generics.lifetime;
    let impl_generics = &generics.impl_block_generics;
    let type_generics = &generics.type_generics;
    let state_type_generics = &generics.type_generics_with_lt;
    let where_clause = &generics.where_clause;

    let state_ident = format_ident!("__{ident}State");
    let argument = format_ident!("argument");
    let option = format_ident!("option");
    let state = format_ident!("state");

    let state_definition = state_definition(&state_ident);

    let add_positional_body = add_positional_body(&argument);
    let add_long_option_body = add_long_option_body(&option, &argument);
    let add_long_body = add_long_body(&option, &argument);
    let add_short_body = add_short_body(&option, &argument);

    let build_body = build_body(&state);

    quote! {
        #state_definition

        impl #impl_generics ::debate::state::State<#lifetime> for #state_ident #state_type_generics #where_clause {
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
                A: ::debate::parameter::ArgAccess<#lifetime>,
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
                A: ::debate::parameter::ArgAccess<#lifetime>,
                E: ::debate::state::Error<#lifetime, A>
            {
                #add_short_body
            }
        }

        impl #impl_generics ::debate::build::BuildFromArgs<#lifetime> for #ident #type_generics #where_clause {
            type State = #state_ident #state_type_generics;

            fn build<E>(state: Self::State) -> ::core::result::Result<Self, E>
            where
                E: ::debate::build::Error
            {
                #build_body
            }
        }
    }
}
