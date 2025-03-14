use itertools::Itertools;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    Data, DeriveInput, Ident, Token, Type, Variant, punctuated::Punctuated, spanned::Spanned as _,
};

use crate::{
    common::value::AnalyzedEnum,
    generics::{AngleBracedLifetime, compute_generics},
};

fn derive_newtype_usage(
    ident: &Ident,
    field_type: &Type,
    type_lifetime: Option<&AngleBracedLifetime>,
) -> syn::Result<TokenStream2> {
    Ok(quote! {
        impl #type_lifetime ::debate::help::ParameterUsage for #ident $type_lifetime {
            const VALUE: ::debate::help::ParameterValueKind =
                <#field_type as ::debate::help::ParameterUsage>::VALUE;

            const REQUIREMENT: ::debate::help::Requirement =
                <#field_type as ::debate::help::ParameterUsage>::REQUIREMENT;

            const REPETITION: ::debate::help::Repetition =
                <#field_type as ::debate::help::ParameterUsage>::REPETITION;
        }
    })
}

fn derive_value_enum_usage(
    ident: &Ident,
    variants: &Punctuated<Variant, Token![,]>,
    type_lifetime: Option<&AngleBracedLifetime>,
) -> syn::Result<TokenStream2> {
    let analyzed = AnalyzedEnum::from_variants(variants)?;
    let variant_names = analyzed.variants.iter().map(|(value, _)| value.as_str());

    let kind = match analyzed.fallback {
        None => quote! { OneOf(&[#(#variant_names,)*]) },
        Some(_) => quote! { Value },
    };

    Ok(quote! {
        impl #type_lifetime ::debate::help::ParameterUsage for #ident #type_lifetime {
            const VALUE: ::debate::help::ParameterValueKind =
                ::debate::help::ParameterValueKind:: #kind;

            const REQUIREMENT: ::debate::help::Requirement =
                ::debate::help::Requirement::Mandatory;

            const REPETITION: ::debate::help::Repetition =
                ::debate::help::Repetition::Single;
        }
    })
}

pub fn derive_parameter_usage_result(tokens: TokenStream2) -> syn::Result<TokenStream2> {
    let input: DeriveInput = syn::parse2(tokens)?;
    let (_, type_lifetime) = compute_generics(&input.generics)?;

    match input.data {
        Data::Struct(ref data) => {
            let field = data.fields.iter().exactly_one().map_err(|_| {
                syn::Error::new(
                    input.span(),
                    "can only derive `ParameterUsage` on structs with exactly one field",
                )
            })?;

            derive_newtype_usage(&input.ident, &field.ty, type_lifetime.as_ref())
        }
        Data::Enum(ref data) => {
            derive_value_enum_usage(&input.ident, &data.variants, type_lifetime.as_ref())
        }
        Data::Union(_) => Err(syn::Error::new(
            input.span(),
            "can't derive `ParameterUsage` on a union",
        )),
    }
}
