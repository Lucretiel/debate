mod common;
pub mod enumeration;
pub mod structure;

use itertools::Itertools;
use proc_macro2::TokenStream as TokenStream2;
use syn::{DeriveInput, Fields, spanned::Spanned as _};

use crate::from_args::structure::derive_args_newtype_struct;
use crate::generics::compute_generics;

use self::enumeration::derive_args_enum;
use self::structure::derive_args_struct;

pub fn derive_args_result(item: TokenStream2) -> syn::Result<TokenStream2> {
    let input: DeriveInput = syn::parse2(item)?;
    let (lifetime, type_lifetime) = compute_generics(&input.generics)?;

    match input.data {
        syn::Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => derive_args_struct(
                &input.ident,
                &fields.named,
                &lifetime,
                type_lifetime.as_ref(),
                &input.attrs,
            ),
            Fields::Unnamed(ref fields) => derive_args_newtype_struct(
                &input.ident,
                fields.unnamed.iter().exactly_one().map_err(|_| {
                    syn::Error::new(fields.span(), "tuple structs must be newtype structs")
                })?,
                &lifetime,
                type_lifetime.as_ref(),
            ),
            Fields::Unit => Err(syn::Error::new(
                input.span(),
                "can't derive `FromArgs` on a unit struct",
            )),
        },

        syn::Data::Enum(ref data) => derive_args_enum(
            &input.ident,
            &data.variants,
            &lifetime,
            type_lifetime.as_ref(),
            &input.attrs,
        ),
        syn::Data::Union(_) => Err(syn::Error::new(
            input.span(),
            "can't derive `FromArgs` on a union",
        )),
    }
}
