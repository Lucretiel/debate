use heck::ToSnakeCase;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{Ident, Token, Variant, punctuated::Punctuated};

use crate::{
    common::{
        HelpOption,
        enumeration::{Fallback, ParsedSubcommandInfo},
    },
    generics::AngleBracedLifetime,
    usage::common::struct_usage_items,
};

pub fn derive_usage_enum_subcommand(
    name: &Ident,
    variants: &Punctuated<Variant, Token![,]>,
    lifetime: Option<&AngleBracedLifetime>,
) -> syn::Result<TokenStream2> {
    let parsed_variants = ParsedSubcommandInfo::from_variants(variants)?;
    let command_name = name.to_string().to_snake_case();
    let requirement = match parsed_variants.fallback {
        Fallback::Explicit(_) => quote! {Optional},
        Fallback::Internal(_) => quote! {Mandatory},
    };

    let subcommands = parsed_variants.variants.iter().map(|variant| {
        let subcommand_name = variant.command.as_str();
        let description = variant.docs.as_str();

        let items_for_subcommand = struct_usage_items(&variant.fields, HelpOption::new());

        quote! {
            ::debate::help::Subcommand {
                command: #subcommand_name,
                description: ::debate::help::Description::new(#description),
                usage: #items_for_subcommand,
            }
        }
    });

    Ok(quote! {
        impl #lifetime ::debate::help::Usage for #name #lifetime {
            const NAME: &'static str = #command_name;
            const DESCRIPTION: ::debate::help::Description<'static> =
                ::debate::help::Description::new("TODO SUBCOMMANDS");
            const ITEMS: ::debate::help::UsageItems<'static> =
                ::debate::help::UsageItems::Subcommands {
                    requirement: ::debate::help::Requirement:: #requirement,
                    commands: &[
                        #(#subcommands,)*
                    ],
                };
        }
    })
}
