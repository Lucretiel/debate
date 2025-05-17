use proc_macro2::TokenStream as TokenStream2;
use quote::quote;

use crate::common::{FieldDefault, FlattenFieldInfo, HelpOption, OptionTag, ParsedFieldInfo};

/// Convert a set of tags into an expression suitable for use after
/// `::debate::help::Tags`
fn compute_usage_tags(tags: &OptionTag<&str, char>) -> TokenStream2 {
    match *tags {
        OptionTag::Long(ref long) => quote! { Long { long : #long } },
        OptionTag::Short(ref short) => quote! { Short { short: #short} },
        OptionTag::LongShort {
            ref long,
            ref short,
        } => {
            quote! {LongShort { long: #long, short: #short } }
        }
    }
}

/// Given the description of a struct, produce a const expression of type
/// `&[help::Parameter]` that can be used to fill in the items for a struct or subcommand
pub fn struct_usage_items(parsed_fields: &[ParsedFieldInfo<'_>], help: HelpOption) -> TokenStream2 {
    let parameters = parsed_fields.iter().map(|field| match field {
        ParsedFieldInfo::Positional(field) => {
            let placeholder = field.placeholder.as_str();
            let ty = field.ty;
            let defaulted = matches!(field.default, FieldDefault::Trait | FieldDefault::Expr(_));
            let docs = field.docs.as_str();

            quote! {
                ::debate::help::Parameter::Positional {
                    description: ::debate::help::Description::new(#docs),
                    requirement: match (#defaulted) {
                        true => ::debate::help::Requirement::Optional,
                        false => <#ty as ::debate::help::ParameterUsage>::REQUIREMENT,
                    },
                    repetition: <#ty as ::debate::help::ParameterUsage>::REPETITION,
                    argument: <#ty as ::debate::help::ParameterUsage>::VALUE
                        .as_value_parameter(#placeholder),
                }
            }
        }
        ParsedFieldInfo::Option(field) => {
            let tags = compute_usage_tags(&field.tags.simplify());
            let placeholder = field.placeholder.as_str();
            let ty = field.ty;
            let defaulted = matches!(field.default, FieldDefault::Trait | FieldDefault::Expr(_));
            let docs = field.docs.as_str();

            quote! {
                ::debate::help::Parameter::Option {
                    description: ::debate::help::Description::new(#docs),
                    requirement: match #defaulted {
                        true => ::debate::help::Requirement::Optional,
                        false => <#ty as ::debate::help::ParameterUsage>::REQUIREMENT,
                    },
                    repetition: <#ty as ::debate::help::ParameterUsage>::REPETITION,
                    argument: <#ty as ::debate::help::ParameterUsage>::VALUE
                        .as_maybe_value_parameter(#placeholder),
                    tags: ::debate::help::Tags:: #tags,
                }
            }
        }
        ParsedFieldInfo::Flatten(FlattenFieldInfo { ty, ident, docs }) => {
            let name = ident.as_ref().map(|ident| ident.as_str());
            let name = match name {
                None => quote! { ::core::option::Option::None },
                Some(name) => quote! { ::core::option::Option::Some(#name) },
            };

            quote! {
                ::debate::help::Parameter::Group {
                    name: #name,
                    description: ::debate::help::Description::new(#docs),
                    contents: <#ty as ::debate::help::Usage>::ITEMS,
                }
            }
        }
    });

    let help_parameter = help.as_tags().map(|tags| {
        let tags = compute_usage_tags(&tags);

        quote! {
            ::debate::help::Parameter::Option {
                description: ::debate::help::Description::new("Show usage information"),
                requirement: ::debate::help::Requirement::Optional,
                repetition: ::debate::help::Repetition::Single,
                argument: ::core::option::Option::None,
                tags: ::debate::help::Tags:: #tags,
            },
        }
    });

    quote! {
        &[
            #(#parameters,)*
            #help_parameter
        ]
    }
}
