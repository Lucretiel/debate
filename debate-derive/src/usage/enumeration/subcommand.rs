use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{Ident, Token, Variant, punctuated::Punctuated};

use crate::{
    common::{HelpOption, enumeration::ParsedSubcommandInfo},
    generics::AngleBracedLifetime,
    usage::common::struct_usage_implementation,
};

pub fn derive_usage_enum_subcommand(
    name: &Ident,
    variants: &Punctuated<Variant, Token![,]>,
    lifetime: Option<&AngleBracedLifetime>,
) -> syn::Result<TokenStream2> {
    let receiver = format_ident!("receiver");

    let parsed_variants = ParsedSubcommandInfo::from_variants(variants)?;

    let subcommand_calls = parsed_variants.variants.iter().map(|variant| {
        let ident = variant.ident.raw();
        let subcommand_name = variant.command.as_str();
        let description = variant.docs.as_str();

        let usage_for_subcommand =
            struct_usage_implementation(ident, &variant.fields, HelpOption::new(), None);

        quote! {
            #receiver . subcommand(
                #subcommand_name,
                const { ::core::option::Option::Some(#description) },
                const {
                    // Local struct serves as the implementation for Usage
                    // for this specific subcommand.
                    // TODO: could we just... reproduce the set of arguments
                    // on this type, then derive Usage on it?
                    struct #ident;

                    #usage_for_subcommand

                    ::debate::help::UsageHelper::<#ident>::new()
                },
            )
        }
    });

    Ok(quote! {
        impl #lifetime ::debate::help::Usage for #name #lifetime {
            fn describe<R>(#receiver: &mut R) -> Result<(), R::Err>
            where
                R: ::debate::help::Receiver
            {
                #(match (#subcommand_calls) {
                    Ok(()) => {},
                    Err(err) => return Err(err),
                })*
                Ok(())
            }
        }
    })
}
