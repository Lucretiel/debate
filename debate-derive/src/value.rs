use heck::ToKebabCase;
use itertools::Itertools;
use proc_macro2::{Literal, TokenStream as TokenStream2};
use quote::{ToTokens, format_ident, quote};
use syn::{
    DeriveInput, FieldsNamed, FieldsUnnamed, Generics, Ident, Token, Variant,
    punctuated::Punctuated, spanned::Spanned,
};

struct Fallback<'a> {
    variant: &'a Ident,
    field: Option<&'a Ident>,
}
struct AnalyzedEnum<'a> {
    variants: Vec<(String, &'a Ident)>,
    fallback: Option<Fallback<'a>>,
}

impl<'a> AnalyzedEnum<'a> {
    pub fn from_variants(variants: impl IntoIterator<Item = &'a Variant>) -> syn::Result<Self> {
        let mut fallback = None;
        let mut collected = Vec::new();

        for variant in variants {
            match variant.fields {
                syn::Fields::Unit => {
                    // TODO: rename attribute
                    collected.push((variant.ident.to_string().to_kebab_case(), &variant.ident));
                }
                syn::Fields::Unnamed(FieldsUnnamed {
                    unnamed: ref fields,
                    ..
                })
                | syn::Fields::Named(FieldsNamed {
                    named: ref fields, ..
                }) => match fields.iter().exactly_one() {
                    Ok(field) => match fallback {
                        Some(_) => {
                            return Err(syn::Error::new(
                                variant.span(),
                                "error: more than one fallback variant",
                            ));
                        }
                        None => {
                            fallback = Some(Fallback {
                                variant: &variant.ident,
                                field: field.ident.as_ref(),
                            })
                        }
                    },
                    Err(_) => {
                        return Err(syn::Error::new(
                            variant.span(),
                            "error: variant with more than one field",
                        ));
                    }
                },
            }
        }

        Ok(AnalyzedEnum {
            variants: collected,
            fallback,
        })
    }
}

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
    generics: &Generics,
    variants: &Punctuated<Variant, Token![,]>,
) -> syn::Result<TokenStream2> {
    let analyzed = AnalyzedEnum::from_variants(variants)?;

    let normalized_unit_variants = analyzed
        .variants
        .iter()
        .map(|&(ref rename, variant)| (rename.as_str(), variant));

    let unit_byte_arms = unit_arms(normalized_unit_variants.clone(), |rename| {
        Literal::byte_string(rename.as_bytes())
    });

    let unit_str_arms = unit_arms(normalized_unit_variants, |rename| Literal::string(rename));

    let from_arg_ident = format_ident!("from_arg");
    let from_arg_str_ident = format_ident!("from_arg_str");

    let byte_parse_error_ident = format_ident!("byte_parse_error");
    let str_parse_error_ident = format_ident!("parse_error");

    let byte_fallback_arm = fallback_arm(
        analyzed.fallback.as_ref(),
        &from_arg_ident,
        &byte_parse_error_ident,
        "invalid value",
    );

    let str_fallback_arm = fallback_arm(
        analyzed.fallback.as_ref(),
        &from_arg_str_ident,
        &str_parse_error_ident,
        "invalid value",
    );

    Ok(quote! {
        impl<'arg> ::debate::parameter::Value<'arg> for #ident {
            fn from_arg<E: ::debate::parameter::Error<'arg>>(
                argument: ::debate_parser::Arg<'arg>,
            ) -> Result<Self, E> {
                match argument.bytes() {
                    #(#unit_byte_arms)*
                    #byte_fallback_arm
                }
            }

            fn from_arg_str<E: ::debate::parameter::Error<'arg>>(
                argument: &str,
            ) -> Result<Self, E> {
                match argument {
                    #(#unit_str_arms)*
                    #str_fallback_arm
                }
            }
        }
    })
}

fn derive_value_newtype(ident: &Ident, field: Option<&Ident>) -> syn::Result<TokenStream2> {
    let struct_body = match field {
        Some(field) => quote! { { #field: value} },
        None => quote! { ( #field ) },
    };

    let match_body = |method| {
        quote! {
            match ::debate::parameter::Value::#method(argument) {
                ::core::result::Result::Ok(value) => ::core::result::Result::Ok(
                    Self #struct_body
                ),
                ::core::result::Result::Err(err) => ::core::result::Result::Err(err),
            }
        }
    };

    let from_arg_body = match_body(format_ident!("from_arg"));
    let from_str_body = match_body(format_ident!("from_arg_str"));

    Ok(quote! {
        impl<'arg> ::debate::parameter::Value<'arg> for #ident {
            fn from_arg<E: ::debate::parameter::Error<'arg>>(
                argument: ::debate_parser::Arg<'arg>,
            ) -> Result<Self, E> {
                #from_arg_body
            }

            fn from_arg_str<E: ::debate::parameter::Error<'arg>>(
                argument: &str,
            ) -> Result<Self, E> {
                #from_str_body
            }
        }
    })
}

pub fn derive_value_result(item: TokenStream2) -> syn::Result<TokenStream2> {
    let input: DeriveInput = syn::parse2(item)?;

    match input.data {
        syn::Data::Struct(ref data) => {
            let field = data.fields.iter().exactly_one().map_err(|_| {
                syn::Error::new(
                    input.span(),
                    "can only derive `Value` on structs with exactly one field",
                )
            })?;

            derive_value_newtype(&input.ident, field.ident.as_ref())
        }
        syn::Data::Enum(ref data) => {
            derive_value_enum(&input.ident, &input.generics, &data.variants)
        }
        syn::Data::Union(_) => Err(syn::Error::new(
            input.span(),
            "can't derive `Value` on a union",
        )),
    }
}
