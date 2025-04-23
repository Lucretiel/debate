mod common;
pub mod enumeration;
pub mod structure;

use proc_macro2::TokenStream as TokenStream2;
use syn::{DeriveInput, Fields, spanned::Spanned as _};

use crate::generics::compute_generics;

use self::enumeration::derive_args_enum;
use self::structure::derive_args_struct;

pub fn derive_args_result(item: TokenStream2) -> syn::Result<TokenStream2> {
    let input: DeriveInput = syn::parse2(item)?;
    let (lifetime, type_lifetime) = compute_generics(&input.generics)?;

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
            &lifetime,
            type_lifetime.as_ref(),
            &input.attrs,
        ),
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
