use proc_macro2::TokenStream as TokenStream2;
use quote::quote;

use crate::common::{
    Description, FlagFieldInfo, FlagTags, FlattenFieldInfo, HelpFlag, ParsedFieldInfo,
    enumeration::flag_set::FlagType,
};

/// Convert a set of tags into an expression suitable for use after
/// `::debate::Tags`
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

pub trait FlagUsageInfo {
    fn docs(&self) -> &Description;
    fn defaulted(&self) -> bool;
    fn ty(&self) -> FlagType<'_>;
    fn tags(&self) -> FlagTags<&str, char>;
    fn placeholder(&self) -> &str;
}

impl FlagUsageInfo for &FlagFieldInfo<'_> {
    fn docs(&self) -> &Description {
        &self.docs
    }

    fn defaulted(&self) -> bool {
        self.default.is_some()
    }

    fn ty(&self) -> FlagType<'_> {
        FlagType::Typed(self.ty)
    }

    fn tags(&self) -> FlagTags<&str, char> {
        self.tags.simplify()
    }

    fn placeholder(&self) -> &str {
        self.placeholder.as_str()
    }
}

struct Invert<'a> {
    tag: &'a str,
}

impl FlagUsageInfo for Invert<'_> {
    fn docs(&self) -> &Description {
        const {
            &Description {
                succinct: String::new(),
                full: String::new(),
            }
        }
    }

    fn defaulted(&self) -> bool {
        true
    }

    fn ty(&self) -> FlagType<'_> {
        FlagType::Unit
    }

    fn tags(&self) -> FlagTags<&str, char> {
        FlagTags::Long(self.tag)
    }

    fn placeholder(&self) -> &str {
        ""
    }
}

/// Given a description of a flag, produce a const expression of type
/// `:debate::help::ParameterFlag`
pub fn flag_usage(flag: impl FlagUsageInfo) -> TokenStream2 {
    let tags = compute_usage_tags(&flag.tags());
    let placeholder = flag.placeholder();
    let ty = flag.ty();
    let defaulted = flag.defaulted();
    let docs = flag.docs().quote();

    quote! {
        const {
            ::debate::help::ParameterFlag {
                description: #docs,
                requirement: match #defaulted {
                    true => ::debate::help::Requirement::Optional,
                    false => <#ty as ::debate::help::ParameterUsage>::REQUIREMENT,
                },
                repetition: <#ty as ::debate::help::ParameterUsage>::REPETITION,
                argument: <#ty as ::debate::help::ParameterUsage>::VALUE
                    .as_maybe_value_parameter(#placeholder),
                tags: ::debate::Tags:: #tags,
            }
        }
    }
}

/// Given the description of a struct, produce a const expression of type
/// `&[help::Parameter]` that can be used to fill in the items for a struct or subcommand
pub fn struct_usage_items(
    parsed_fields: &[ParsedFieldInfo<'_>],
    help: Option<HelpFlag>,
) -> TokenStream2 {
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
            let invert = field.invert.as_ref().map(|tag| {
                let usage = flag_usage(Invert { tag: tag.as_str() });
                quote! {
                    ::debate::help::Parameter::Flag(#usage),
                }
            });

            let usage = flag_usage(field);

            quote! {
                #invert
                ::debate::help::Parameter::Flag(#usage),
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
    let help_parameter = help.map(|help| {
        let tags = compute_usage_tags(&help.tags);

        quote! {
            ::debate::help::Parameter::Flag(::debate::help::ParameterFlag {
                description: ::debate::help::Description::new("Show usage information"),
                requirement: ::debate::help::Requirement::Optional,
                repetition: ::debate::help::Repetition::Single,
                argument: ::core::option::Option::None,
                tags: ::debate::Tags:: #tags,
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
