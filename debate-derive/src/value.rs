use itertools::Itertools;
use proc_macro2::{Literal, TokenStream as TokenStream2};
use quote::{ToTokens, format_ident, quote};
use syn::{
    Data, DeriveInput, Ident, Lifetime, Token, Variant, punctuated::Punctuated, spanned::Spanned,
};

use crate::{
    common::value::{AnalyzedEnum, Fallback},
    generics::{AngleBracedLifetime, compute_generics},
};

fn unit_arms<'a>(
    variants: impl Iterator<Item = (&'a str, &'a Ident)>,
    scrutinee: impl Fn(&str) -> Literal,
) -> impl Iterator<Item = impl ToTokens> {
    variants.map(move |(name, variant)| {
        let scrutinee = scrutinee(name);
        quote! {
            #scrutinee => ::core::result::Result::Ok(Self::#variant),
        }
    })
}

fn fallback_arm(
    fallback: Option<&Fallback<'_>>,
    method: &Ident,
    error_method: &Ident,
    error_message: &str,
) -> impl ToTokens {
    match fallback {
        Some(fallback) => {
            let variant = fallback.variant;

            let body = match fallback.field {
                Some(field) => quote! { { #field: value, } },
                None => quote! { ( value ) },
            };

            quote! {
                _ => match ::debate::parameter::Value::#method(argument) {
                    ::core::result::Result::Err(err) => ::core::result::Result::Err(err),
                    ::core::result::Result::Ok(value) => ::core::result::Result::Ok(
                        Self::#variant #body
                    ),
                },
            }
        }
        None => quote! {
            _ => ::core::result::Result::Err(
                ::debate::parameter::Error::#error_method(
                    argument,
                    #error_message
                )
            ),
        },
    }
}

fn derive_value_enum(
    ident: &Ident,
    variants: &Punctuated<Variant, Token![,]>,
    lifetime: &Lifetime,
    type_lifetime: Option<&AngleBracedLifetime>,
) -> syn::Result<TokenStream2> {
    let analyzed = AnalyzedEnum::from_variants(variants)?;

    let normalized_unit_variants = analyzed
        .variants
        .iter()
        .map(|&(ref rename, variant)| (rename.as_str(), variant));

    let unit_byte_arms = unit_arms(normalized_unit_variants.clone(), |rename| {
        Literal::byte_string(rename.as_bytes())
    });

    let from_arg_ident = format_ident!("from_arg");
    let byte_parse_error_ident = format_ident!("byte_parse_error");

    let byte_fallback_arm = fallback_arm(
        analyzed.fallback.as_ref(),
        &from_arg_ident,
        &byte_parse_error_ident,
        "invalid value",
    );

    Ok(quote! {
        impl<#lifetime> ::debate::parameter::Value<#lifetime> for #ident #type_lifetime {
            fn from_arg<E: ::debate::parameter::Error<#lifetime>>(
                argument: & #lifetime ::debate_parser::Arg,
            ) -> Result<Self, E> {
                match argument.bytes() {
                    #(#unit_byte_arms)*
                    #byte_fallback_arm
                }
            }
        }
    })
}

fn derive_value_newtype(
    ident: &Ident,
    field: Option<&Ident>,
    lifetime: &Lifetime,
    type_lifetime: Option<&AngleBracedLifetime>,
) -> TokenStream2 {
    let struct_body = match field {
        Some(field) => quote! { { #field: value} },
        None => quote! { ( value ) },
    };

    quote! {
        impl<#lifetime> ::debate::parameter::Value<#lifetime> for #ident #type_lifetime {
            fn from_arg<E: ::debate::parameter::Error<#lifetime>>(
                argument: & #lifetime ::debate_parser::Arg,
            ) -> Result<Self, E> {
                match ::debate::parameter::Value::from_arg(argument) {
                    ::core::result::Result::Ok(value) => ::core::result::Result::Ok(
                        Self #struct_body
                    ),
                    ::core::result::Result::Err(err) => ::core::result::Result::Err(err),
                }
            }
        }
    }
}

pub fn derive_value_result(item: TokenStream2) -> syn::Result<TokenStream2> {
    let input: DeriveInput = syn::parse2(item)?;
    let (lifetime, type_lifetime) = compute_generics(&input.generics)?;

    match input.data {
        Data::Struct(ref data) => {
            let field = data.fields.iter().exactly_one().map_err(|_| {
                syn::Error::new(
                    input.span(),
                    "can only derive `Value` on structs with exactly one field",
                )
            })?;

            Ok(derive_value_newtype(
                &input.ident,
                field.ident.as_ref(),
                &lifetime,
                type_lifetime.as_ref(),
            ))
        }
        Data::Enum(ref data) => derive_value_enum(
            &input.ident,
            &data.variants,
            &lifetime,
            type_lifetime.as_ref(),
        ),
        Data::Union(_) => Err(syn::Error::new(
            input.span(),
            "can't derive `Value` on a union",
        )),
    }
}
