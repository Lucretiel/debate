use itertools::Itertools;
use proc_macro2::{Literal, TokenStream as TokenStream2};
use quote::quote;
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
                    collected.push((variant.ident.to_string(), &variant.ident));
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

fn derive_value_enum(
    ident: &Ident,
    generics: &Generics,
    variants: &Punctuated<Variant, Token![,]>,
) -> syn::Result<TokenStream2> {
    let analyzed = AnalyzedEnum::from_variants(variants)?;

    let unit_arms = analyzed.variants.iter().map(|(rename, variant)| {
        let rename = Literal::byte_string(rename.as_bytes());

        quote! {
            #rename => ::core::result::Result::Ok(#ident::#variant),
        }
    });

    let fallback_arm = match analyzed.fallback {
        Some(fallback) => {
            let variant = fallback.variant;
            match fallback.field {
                Some(field) => quote! {
                    _ => match ::debate::Value::from_arg(argument) {
                        ::core::result::Result::Err(err) => ::core::result::Result::Err(err),
                        ::core::result::Result::Ok(value) => ::core::result::Result::Ok(
                            #ident::#variant {
                                #field: value
                            }
                        ),
                    }
                },
                None => todo!(),
            }
        }
        None => todo!(),
    };

    quote! {
        impl<'arg> ::debate::Value<'arg> for #ident {
            fn from_arg<E: ::debate::error::ParameterError>(argument: ::debate_parser::Arg<'arg>) -> Result<Self, E> {

            }
        }
    }
}

pub fn derive_value_result(item: TokenStream2) -> syn::Result<TokenStream2> {
    let input: DeriveInput = syn::parse2(item)?;

    let no_derive = |msg| Err(syn::Error::new(input.span(), msg));

    match input.data {
        syn::Data::Struct(data_struct) => todo!(),
        syn::Data::Enum(ref data) => {
            derive_value_enum(&input.ident, &input.generics, &data.variants)
        }
        syn::Data::Union(_) => no_derive("can't derive `Value` on a union"),
    }
}
