mod common;
pub mod enumeration;
pub mod structure;
pub mod value;

use proc_macro2::TokenStream as TokenStream2;
use syn::{DeriveInput, spanned::Spanned as _};

use crate::generics::compute_generics;

use self::enumeration::derive_usage_enum;
use self::structure::derive_usage_struct;

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
        syn::Data::Enum(data) => derive_usage_enum(
            &input.ident,
            &data.variants,
            type_lifetime.as_ref(),
            &input.attrs,
        ),
        syn::Data::Union(_) => Err(syn::Error::new(
            input.span(),
            "can't derive `FromArgs` on a union",
        )),
    }
}
