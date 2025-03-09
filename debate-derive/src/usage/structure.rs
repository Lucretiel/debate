use itertools::Itertools as _;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{Field, Ident, Token, punctuated::Punctuated};

use crate::{
    common::{FieldDefault, FlattenFieldInfo, ParsedFieldInfo},
    generics::AngleBracedLifetime,
};

pub fn derive_usage_struct(
    name: &Ident,
    fields: &Punctuated<Field, Token![,]>,
    lifetime: Option<&AngleBracedLifetime>,
) -> syn::Result<TokenStream2> {
    let fields: Vec<ParsedFieldInfo> = fields
        .iter()
        .map(ParsedFieldInfo::from_field)
        .try_collect()?;

    let receiver_method_calls = fields.iter().map(|field| match field {
        ParsedFieldInfo::Positional(field) => {
            let placeholder = field.placeholder.as_str();
            let ty = field.ty;
            let defaulted = matches!(field.default, FieldDefault::Trait | FieldDefault::Expr(_));
            let docs = field.docs.as_str();

            quote! {
                receiver.positional(
                    const {
                        <#ty as ::debate::help::ParameterUsage>::VALUE
                            .as_value_parameter(#placeholder)
                    },
                    const {
                        match #defaulted {
                            true => ::debate::help::Requirement::Optional,
                            false => <#ty as ::debate::help::ParameterUsage>::REQUIREMENT,
                        }
                    },
                    <#ty as ::debate::help::ParameterUsage>::REPETITION,
                    #docs,
                )
            }
        }
        ParsedFieldInfo::Option(field) => {
            let tags = match field.tags {
                crate::common::OptionTag::Long(ref long) => {
                    let long = long.as_str();
                    quote! { Long { long : #long } }
                }
                crate::common::OptionTag::Short(ref short) => {
                    let short: char = **short;
                    quote! { Short { short: #short} }
                }
                crate::common::OptionTag::LongShort {
                    ref long,
                    ref short,
                } => {
                    let long = long.as_str();
                    let short: char = **short;
                    quote! {LongShort { long: #long, short: #short } }
                }
            };
            let placeholder = field.placeholder.as_str();
            let ty = field.ty;
            let defaulted = matches!(field.default, FieldDefault::Trait | FieldDefault::Expr(_));
            let docs = field.docs.as_str();

            quote! {
                receiver.option(
                    ::debate::help::Tags::#tags,
                    const {
                        <#ty as ::debate::help::ParameterUsage>::VALUE
                            .as_maybe_value_parameter(#placeholder)
                    },
                    const {
                        match #defaulted {
                            true => ::debate::help::Requirement::Optional,
                            false => <#ty as ::debate::help::ParameterUsage>::REQUIREMENT,
                        }
                    },
                    <#ty as ::debate::help::ParameterUsage>::REPETITION,
                    #docs,
                )
            }
        }
        ParsedFieldInfo::Flatten(FlattenFieldInfo { ty, ident, .. }) => {
            match ident.as_ref().map(|ident| ident.as_str()) {
                Some(name) => quote! {
                    receiver.group(#name, ::debate::help::UsageHelper::<#ty>)
                },
                None => quote! {
                    receiver.anonymous_group(::debate::help::UsageHelper::<#ty>)
                },
            }
        }
    });

    Ok(quote! {
        impl #lifetime ::debate::help::Usage for #name #lifetime {
            fn describe<R>(receiver: &mut R) -> Result<(), R::Err>
            where
                R: ::debate::help::Receiver
            {
                #( #receiver_method_calls ? ;)*
                Ok(())
            }
        }
    })
}
