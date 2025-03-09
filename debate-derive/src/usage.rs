pub mod structure;

use itertools::Itertools as _;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use structure::derive_usage_struct;
use syn::{DeriveInput, spanned::Spanned as _};

use crate::{
    common::{FieldDefault, FlattenFieldInfo, ParsedFieldInfo},
    generics::compute_generics,
};

pub fn derive_usage_result(item: TokenStream2) -> syn::Result<TokenStream2> {
    let input: DeriveInput = syn::parse2(item)?;
    let (_, type_lifetime) = compute_generics(&input.generics)?;

    match input.data {
        syn::Data::Struct(ref data) => derive_usage_struct(
            &input.ident,
            match data.fields {
                syn::Fields::Named(ref fields) => &fields.named,
                syn::Fields::Unnamed(ref fields) => &fields.unnamed,
                syn::Fields::Unit => {
                    return Err(syn::Error::new(
                        input.span(),
                        "can't derive `Usage` on a unit struct",
                    ));
                }
            },
            type_lifetime.as_ref(),
        ),
        syn::Data::Enum(_) => {
            todo!("enum isn't implemented yet")
        }
        syn::Data::Union(_) => {
            return Err(syn::Error::new(
                input.span(),
                "can't derive `FromArgs` on a union",
            ));
        }
    }
}
