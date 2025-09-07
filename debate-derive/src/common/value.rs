use heck::ToKebabCase as _;
use itertools::Itertools as _;
use syn::{Fields, FieldsNamed, FieldsUnnamed, Ident, Variant, spanned::Spanned as _};

pub struct Fallback<'a> {
    pub variant: &'a Ident,
    pub field: Option<&'a Ident>,
}

pub struct AnalyzedEnum<'a> {
    pub variants: Vec<(String, &'a Ident)>,
    pub fallback: Option<Fallback<'a>>,
}

impl<'a> AnalyzedEnum<'a> {
    pub fn from_variants(variants: impl IntoIterator<Item = &'a Variant>) -> syn::Result<Self> {
        let mut fallback = None;
        let mut collected = Vec::new();

        for variant in variants {
            match variant.fields {
                Fields::Unit => {
                    // TODO: rename attribute
                    // TODO: case insensitivity
                    collected.push((variant.ident.to_string().to_kebab_case(), &variant.ident));
                }
                Fields::Unnamed(FieldsUnnamed {
                    unnamed: ref fields,
                    ..
                })
                | Fields::Named(FieldsNamed {
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
