pub mod common;
pub mod enumeration;
pub mod structure;

use proc_macro2::TokenStream as TokenStream2;
use syn::{DeriveInput, Fields, spanned::Spanned as _};

use self::enumeration::derive_args_enum;
use self::structure::derive_args_struct;

pub fn derive_args_result(item: TokenStream2) -> syn::Result<TokenStream2> {
    let input: DeriveInput = syn::parse2(item)?;

    let no_derive = |msg| Err(syn::Error::new(input.span(), msg));

    match input.data {
        syn::Data::Struct(ref data) => derive_args_struct(
            &input.ident,
            match data.fields {
                Fields::Named(ref fields) => &fields.named,
                Fields::Unnamed(_) => {
                    return no_derive("Can't derive `FromArgs` on a tuple struct");
                }
                Fields::Unit => return no_derive("can't derive `FromArgs` on a unit struct"),
            },
            &input.generics,
            &input.attrs,
        ),
        syn::Data::Enum(ref data) => {
            derive_args_enum(&input.ident, data, &input.generics, &input.attrs)
        }
        syn::Data::Union(_) => no_derive("can't derive `FromArgs` on a union"),
    }
}
