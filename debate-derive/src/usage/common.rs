use proc_macro2::TokenStream as TokenStream2;
use quote::quote;

use crate::common::{
    Description, FieldDefault, FlattenFieldInfo, HelpOption, OptionTag, ParsedFieldInfo,
};

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

impl Description {
    /// Convert a description into a `::debate::help::Description` expression
    pub fn quote(&self) -> TokenStream2 {
        let short = self.short.as_str();
        let long = self.long.as_str();

        quote! {
            ::debate::help::Description {
                short: #short,
                long: #long,
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
            let defaulted = matches!(field.default, FieldDefault::Trait | FieldDefault::Expr(_));
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
                })
            }
        }
        ParsedFieldInfo::Option(field) => {
            let tags = compute_usage_tags(&field.tags.simplify());
            let placeholder = field.placeholder.as_str();
            let ty = field.ty;
            let defaulted = matches!(field.default, FieldDefault::Trait | FieldDefault::Expr(_));
            let docs = field.docs.quote();

            quote! {
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
                })
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
                })
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
            #(#parameters,)*
            #help_parameter
        ]
    }
}
