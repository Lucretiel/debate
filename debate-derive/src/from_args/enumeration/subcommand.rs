use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{Attribute, DataEnum, Generics, Ident, Token, Variant, punctuated::Punctuated};

pub fn derive_args_enum_subcommand(
    name: &Ident,
    variants: &Punctuated<Variant, Token![,]>,
    generics: &Generics,
    attrs: &[Attribute],
) -> syn::Result<TokenStream2> {
    let state_ident = format_ident!("__{name}State");

    Ok(quote! {
        #[doc(hidden)]
        #[derive(::core::default::Default)]
        enum #state_ident<'arg> {
            // TODO
        }

        impl<'arg> ::debate::state::State<'arg> for #state_ident<'arg> {
            fn add_positional<E>(
                &mut self,
                argument: ::debate_parser::Arg<'arg>
            ) -> ::core::result::Result<(), E>
            where
                E: ::debate::state::Error<'arg, ()>
            {}

            fn add_long_option<E>(
                &mut self,
                option: ::debate_parser::Arg<'arg>,
                argument: ::debate_parser::Arg<'arg>
            ) -> ::core::result::Result<(), E>
            where
                E: ::debate::state::Error<'arg, ()>
            {}

            fn add_long<A, E>(
                &mut self,
                option: ::debate_parser::Arg<'arg>,
                argument: A
            ) -> ::core::result::Result<(), E>
            where
                A: ::debate_parser::ArgAccess<'arg>,
                E: ::debate::state::Error<'arg, A>
            {}

            fn add_short<A, E>(
                &mut self,
                option: u8,
                argument: A
            ) -> ::core::result::Result<(), E>
            where
                A: ::debate_parser::ArgAccess<'arg>,
                E: ::debate::state::Error<'arg, A>
            {}
        }

        impl<'arg> ::debate::from_args::BuildFromArgs<'arg> for #name {
            fn build<E>(state: Self::State) -> ::core::result::Result<Self, E>
            where
                E: ::debate::from_args::Error<'arg>
            {}
        }
    })
}
