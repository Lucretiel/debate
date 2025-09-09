use proc_macro2::TokenStream as TokenStream2;
use quote::quote;

use crate::common::{Description, FlagTags, FlattenFieldInfo, HelpOption, ParsedFieldInfo};

/// Convert a set of tags into an expression suitable for use after
/// `::debate::help::Tags`
fn compute_usage_tags(tags: &FlagTags<&str, char>) -> TokenStream2 {
    match *tags {
        FlagTags::Long(ref long) => quote! { Long { long : #long } },
        FlagTags::Short(ref short) => quote! { Short { short: #short} },
        FlagTags::LongShort {
            ref long,
            ref short,
        } => {
            quote! {LongShort { long: #long, short: #short } }
        }
    }
}

impl Description {
    /// Convert a description into a `::debate::help::Description` expression
    pub fn quote(&self) -> TokenStream2 {
        let succinct = self.succinct.as_str();
        let full = self.full.as_str();

        quote! {
            ::debate::help::Description {
                succinct: #succinct,
                full: #full,
            }
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
            let defaulted = field.default.is_some();
            let docs = field.docs.quote();

            quote! {
                ::debate::help::Parameter::Positional(::debate::help::ParameterPositional {
                    description: #docs,
                    requirement: match (#defaulted) {
                        true => ::debate::help::Requirement::Optional,
                        false => <#ty as ::debate::help::ParameterUsage>::REQUIREMENT,
                    },
                    repetition: <#ty as ::debate::help::ParameterUsage>::REPETITION,
                    argument: <#ty as ::debate::help::ParameterUsage>::VALUE
                        .as_value_parameter(#placeholder),
                }),
            }
        }
        ParsedFieldInfo::Flag(field) => {
            let tags = compute_usage_tags(&field.tags.simplify());
            let placeholder = field.placeholder.as_str();
            let ty = field.ty;
            let defaulted = field.default.is_some();
            let docs = field.docs.quote();

            let invert = field.invert.as_ref().map(|invert| {
                let long = invert.as_str();

                quote! {
                    ::debate::help::Parameter::Option(::debate::help::ParameterOption {
                        description: ::debate::help::Description::new(""),
                        requirement: ::debate::help::Requirement::Optional,
                        repetition: ::debate::help::Repetition::Single,
                        argument: ::core::option::Option::None,
                        tags: ::debate::help::Tags::Long { long: #long},
                    }),
                }
            });

            quote! {
                #invert
                ::debate::help::Parameter::Option(::debate::help::ParameterOption {
                    description: #docs,
                    requirement: match #defaulted {
                        true => ::debate::help::Requirement::Optional,
                        false => <#ty as ::debate::help::ParameterUsage>::REQUIREMENT,
                    },
                    repetition: <#ty as ::debate::help::ParameterUsage>::REPETITION,
                    argument: <#ty as ::debate::help::ParameterUsage>::VALUE
                        .as_maybe_value_parameter(#placeholder),
                    tags: ::debate::help::Tags:: #tags,
                }),
            }
        }
        ParsedFieldInfo::Flatten(FlattenFieldInfo {
            ty,
            docs,
            title,
            placeholder,
            ident,
        }) => {
            let id = ident.as_str();
            let title = title.as_str();
            let placeholder = placeholder.as_str();
            let docs = docs.quote();

            quote! {
                ::debate::help::Parameter::Group(::debate::help::ParameterSubgroup {
                    id: #id,
                    title: #title,
                    placeholder: #placeholder,
                    description: #docs,
                    contents: <#ty as ::debate::help::Usage>::ITEMS,
                }),
            }
        }
    });

    // TODO: it's annoying maintenence burden to have this separated from the
    // `ParsedFieldInfo::Option(field)` branch, above, because every time
    // `::debate::help::Parameter` changes, we have to fix it there AND here.
    // Need to find a way to add this up there. It's tricky cause the iterator
    // above is for `&ParsedFieldInfo`, and it's difficult to create a
    // `ParsedFieldInfo:` for `--help`
    let help_parameter = help.as_tags().map(|tags| {
        let tags = compute_usage_tags(&tags);

        quote! {
            ::debate::help::Parameter::Option(::debate::help::ParameterOption {
                description: ::debate::help::Description::new("Show usage information"),
                requirement: ::debate::help::Requirement::Optional,
                repetition: ::debate::help::Repetition::Single,
                argument: ::core::option::Option::None,
                tags: ::debate::help::Tags:: #tags,
            }),
        }
    });

    quote! {
        &[
            #(#parameters)*
            #help_parameter
        ]
    }
}
