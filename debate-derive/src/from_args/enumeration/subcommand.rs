use std::{collections::HashMap, hash::Hash};

use proc_macro2::{Literal, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{Ident, Lifetime, Token, Variant, punctuated::Punctuated};

use crate::{
    common::{
        OptionTag, ParsedFieldInfo,
        enumeration::{Fallback, ParsedSubcommandInfo, ParsedSubcommandVariant, VariantMode},
    },
    from_args::{
        common::{
            complete_long_body, complete_long_option_body, complete_short_body,
            final_field_initializers, struct_state_block_from_fields,
            struct_state_init_block_from_fields, visit_positional_arms_for_fields,
        },
        from_args_impl,
    },
    generics::AngleBracedLifetime,
};

/// Compute a mapping from tags (such as --field or -f) to commands that are
/// known to use them.
fn compute_recognition_set<'a, T: Hash + Eq>(
    variants: impl IntoIterator<Item = &'a ParsedSubcommandVariant<'a>>,
    get_tag: impl Fn(&'a OptionTag) -> Option<T>,
) -> HashMap<T, Vec<&'a str>> {
    let mut set: HashMap<T, Vec<&str>> = HashMap::new();

    for variant in variants {
        for field in &variant.fields {
            if let ParsedFieldInfo::Option(field) = field {
                if let Some(tag) = get_tag(&field.tags) {
                    set.entry(tag).or_default().push(&variant.command);
                }
            }
        }
    }

    set
}

pub fn derive_args_enum_subcommand(
    name: &Ident,
    variants: &Punctuated<Variant, Token![,]>,
    lifetime: &Lifetime,
    type_lifetime: Option<&AngleBracedLifetime>,
) -> syn::Result<TokenStream2> {
    let arg_ident = format_ident!("arg");
    let add_arg_ident = format_ident!("add_arg");
    let fields_ident = format_ident!("fields");

    let parameter_ident = format_ident!("Parameter");
    let positional_parameter_ident = format_ident!("PositionalParameter");

    let parsed_variants = ParsedSubcommandInfo::from_variants(variants)?;

    let fallback_ident = parsed_variants.fallback.ident();

    let all_option_fields = parsed_variants
        .variants
        .iter()
        .flat_map(|variant| &variant.fields)
        .filter_map(|field| match field {
            ParsedFieldInfo::Option(field) => Some(field),
            ParsedFieldInfo::Positional(_) | ParsedFieldInfo::Flatten(_) => None,
        });

    let long_option_commands = compute_recognition_set(&parsed_variants.variants, |tags| {
        tags.long().as_deref().copied()
    });

    let short_option_commands = compute_recognition_set(&parsed_variants.variants, |tags| {
        tags.short().as_deref().copied()
    });

    let all_commands = parsed_variants
        .variants
        .iter()
        .map(|variant| variant.command.as_str());

    let all_commands_slice = quote! {
        &[#(#all_commands,)*]
    };

    Ok(from_args_impl(
        name,
        lifetime,
        type_lifetime,
        |state_ident| {
            let state_variants = parsed_variants.variants.iter().map(|variant| {
                let variant_ident = variant.ident.raw();
                let variant_state_body = struct_state_block_from_fields(&variant.fields);

                quote! {
                    #variant_ident #variant_state_body
                }
            });

            quote! {
                #[doc(hidden)]
                #[derive(::core::default::Default)]
                enum #state_ident <#lifetime> {
                    #[default]
                    #fallback_ident,
                    #(#state_variants,)*
                }
            }
        },
        |argument| {
            // Arms of the match block that attempts to convert a positional argument
            // into a selected subcommand
            let select_subcommand_arms = parsed_variants.variants.iter().map(|variant| {
                let scrutinee = variant.command.as_bytes();
                let scrutinee = Literal::byte_string(scrutinee);
                let variant_ident = variant.ident.raw();
                let variant_init_block = struct_state_init_block_from_fields(&variant.fields);

                quote! {
                    #scrutinee => Self :: #variant_ident #variant_init_block
                }
            });

            let visit_positional_arms = parsed_variants.variants.iter().map(|variant| {
                let variant_ident = variant.ident.raw();
                let local_positional_arms = visit_positional_arms_for_fields(
                    &fields_ident,
                    argument,
                    &positional_parameter_ident,
                    &arg_ident,
                    &add_arg_ident,
                    &variant.fields,
                );

                quote! {
                    Self :: #variant_ident { ref mut #fields_ident, ref mut position, .. } => {
                        #(#local_positional_arms)*

                        ::core::result::Result::Err(
                            ::debate::state::Error::unrecognized(())
                        )
                    }
                }
            });

            quote! {
                match *self {
                    Self :: #fallback_ident => {
                        *self = match #argument.bytes() {
                            #(#select_subcommand_arms,)*
                            _ => return ::core::result::Result::Err(
                                ::debate::state::Error::unknown_subcommand(
                                    #all_commands_slice,
                                )
                            ),
                        };

                        ::core::result::Result::Ok(())
                    }

                    #(
                        #visit_positional_arms,
                    )*
                }
            }
        },
        |option, argument| {
            let arms = parsed_variants.variants.iter().map(|variant| {
                let variant_ident = variant.ident.raw();
                let body = complete_long_option_body(
                    &fields_ident,
                    argument,
                    option,
                    &parameter_ident,
                    &variant.fields,
                );

                quote! {
                    Self :: #variant_ident { ref mut #fields_ident, .. } => #body,
                }
            });

            quote! {
                // TODO: for now we just say unrecognized, but in the future
                // we need to instead return `rejected` for variants that
                // we do recognize but which are not a part of the current
                // subcommand, or are observed before a subcommand is selected
                match *self {
                    Self :: #fallback_ident => ::core::result::Result::Err(
                        ::debate::state::Error::unrecognized(())
                    ),
                    #(#arms)*
                }
            }
        },
        |option, argument| {
            let arms = parsed_variants.variants.iter().map(|variant| {
                let variant_ident = variant.ident.raw();
                let body = complete_long_body(
                    &fields_ident,
                    argument,
                    option,
                    &parameter_ident,
                    &variant.fields,
                );

                quote! {
                    Self :: #variant_ident { ref mut #fields_ident, .. } => #body,
                }
            });

            quote! {
                match *self {
                    Self :: #fallback_ident => ::core::result::Result::Err(
                        ::debate::state::Error::unrecognized(#argument)
                    ),
                    #(#arms)*
                }
            }
        },
        |option, argument| {
            let arms = parsed_variants.variants.iter().map(|variant| {
                let variant_ident = variant.ident.raw();
                let body = complete_short_body(
                    &fields_ident,
                    argument,
                    option,
                    &parameter_ident,
                    &variant.fields,
                );

                quote! {
                    Self :: #variant_ident { ref mut #fields_ident, .. } => #body,
                }
            });

            quote! {
                match *self {
                    Self :: #fallback_ident => ::core::result::Result::Err(
                        ::debate::state::Error::unrecognized(#argument)
                    ),
                    #(#arms)*
                }
            }
        },
        |state| {
            let fallback_body = match parsed_variants.fallback {
                Fallback::Explicit(ident) => quote! { Self :: #ident },
                Fallback::Internal(_) => quote! {
                    return ::core::result::Result::Err(
                        ::debate::build::Error::required_subcommand(
                            #all_commands_slice,
                        )
                    )
                },
            };

            let variant_match_arms = parsed_variants.variants.iter().map(|variant| {
                let variant_ident = variant.ident.raw();
                let field_initializers = final_field_initializers(&fields_ident, &variant.fields);

                let variant_body = match variant.mode {
                    VariantMode::Unit => quote! {},
                    VariantMode::Tuple => quote! {
                        ( #(#field_initializers,)* )
                    },
                    VariantMode::Struct => quote! {
                        { #(#field_initializers,)* }
                    },
                };

                quote! {
                    Self :: State :: #variant_ident { #fields_ident, ..} => (
                        Self :: #variant_ident #variant_body
                    )
                }
            });

            quote! {
                ::core::result::Result::Ok(
                    match #state {
                        Self :: State :: #fallback_ident => #fallback_body,
                        #(#variant_match_arms,)*
                    }
                )
            }
        },
    ))
}
