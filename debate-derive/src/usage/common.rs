use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::Ident;

use crate::{
    common::{FieldDefault, FlattenFieldInfo, HelpOption, OptionTag, ParsedFieldInfo},
    generics::AngleBracedLifetime,
};

/// Convert a set of tags into an expression suitable for use after
/// `::debate::help::Tags`
fn compute_usage_tags(tags: &OptionTag<&str, char>) -> TokenStream2 {
    match *tags {
        crate::common::OptionTag::Long(ref long) => quote! { Long { long : #long } },

        crate::common::OptionTag::Short(ref short) => quote! { Short { short: #short} },

        crate::common::OptionTag::LongShort {
            ref long,
            ref short,
        } => {
            quote! {LongShort { long: #long, short: #short } }
        }
    }
}

/// Given the description of a struct, produce a `Usage` implementation for
/// that struct. This logic is shared with the enumeration implementation.
pub fn struct_usage_implementation(
    name: &Ident,
    parsed_fields: &[ParsedFieldInfo<'_>],
    help: HelpOption,
    lifetime: Option<&AngleBracedLifetime>,
) -> TokenStream2 {
    let receiver_method_calls = parsed_fields.iter().map(|field| match field {
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
                        match (#defaulted) {
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
            let tags = compute_usage_tags(&field.tags.simplify());
            let placeholder = field.placeholder.as_str();
            let ty = field.ty;
            let defaulted = matches!(field.default, FieldDefault::Trait | FieldDefault::Expr(_));
            let docs = field.docs.as_str();

            quote! {
                receiver.option(
                    const {
                        ::debate::help::Tags::#tags
                    },
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
                    receiver.group(#name, ::debate::help::UsageHelper::<#ty>::new())
                },
                None => quote! {
                    receiver.anonymous_group(::debate::help::UsageHelper::<#ty>::new())
                },
            }
        }
    });

    let help_call = help.as_tags().map(|tags| {
        let tags = compute_usage_tags(&tags);

        quote! {
            match receiver.option(
                const {
                    ::debate::help::Tags:: #tags
                },
                ::core::option::Option::None,
                ::debate::help::Requirement::Optional,
                ::debate::help::Repetition::Single,
                "Show usage information",
            ) {
                ::core::result::Result::Ok(()) => {}
                ::core::result::Result::Err(err) => {
                    return ::core::result::Result::Err(err)
                }
            }
        }
    });

    quote! {
        impl #lifetime ::debate::help::Usage for #name #lifetime {
            fn describe<R>(receiver: &mut R) -> Result<(), R::Err>
            where
                R: ::debate::help::Receiver
            {
                #help_call

                #( match (#receiver_method_calls) {
                    Ok(()) => {}
                    Err(err) => return Err(err),
                })*

                Ok(())
            }
        }
    }
}
