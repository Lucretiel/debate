use proc_macro2::{Literal, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{Ident, Lifetime, Token, Variant, punctuated::Punctuated};

use crate::{
    common::enumeration::{Fallback, ParsedSubcommandInfo, SubcommandVariantMode},
    from_args::common::{
        complete_long_body, complete_long_option_body, complete_short_body,
        final_field_initializers, get_subcommand_field_visitor_calls,
        struct_state_block_from_fields, struct_state_init_block_from_fields,
        visit_positional_arms_for_fields,
    },
    generics::AngleBracedLifetime,
};

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

    let argument = format_ident!("argument");
    let option = format_ident!("option");
    let visitor = format_ident!("visitor");

    let parsed_variants = ParsedSubcommandInfo::from_variants(variants)?;

    let fallback_ident = parsed_variants.fallback.ident();

    let state_ident = format_ident!("__{name}State");
    let state_variants = parsed_variants.variants.iter().map(|variant| {
        let variant_ident = variant.ident.raw();

        let variant_state_body = match variant.mode {
            SubcommandVariantMode::Unit => quote! {
                ( ::core::marker::PhantomData<& #lifetime ()> )
            },
            SubcommandVariantMode::Newtype { ty } => quote! {
                ( <#ty as ::debate::build::BuildFromArgs<#lifetime>>::State )
            },
            SubcommandVariantMode::Struct { ref fields } => struct_state_block_from_fields(fields),
        };

        quote! {
            #variant_ident #variant_state_body
        }
    });

    let all_commands = parsed_variants
        .variants
        .iter()
        .map(|variant| variant.command.as_str());

    let all_commands_slice = quote! {
        &[#(#all_commands,)*]
    };

    // Arms of the match block that attempts to convert a positional argument
    // into a selected subcommand
    let select_subcommand_arms = parsed_variants.variants.iter().map(|variant| {
        let scrutinee = variant.command.as_bytes();
        let scrutinee = Literal::byte_string(scrutinee);
        let variant_ident = variant.ident.raw();

        let variant_init_block = match variant.mode {
            SubcommandVariantMode::Unit => quote! {
                ( ::core::marker::PhantomData )
            },
            SubcommandVariantMode::Newtype { .. } => quote! {
                ( ::core::default::Default::default() )
            },
            SubcommandVariantMode::Struct { ref fields } => {
                struct_state_init_block_from_fields(fields)
            }
        };

        quote! {
            #scrutinee => Self :: #variant_ident #variant_init_block
        }
    });

    // TODO: Deduplicate these 4 things

    let visit_positional_arms = parsed_variants.variants.iter().map(|variant| {
        let variant_ident = variant.ident.raw();

        match variant.mode {
            SubcommandVariantMode::Unit => todo!(),
            SubcommandVariantMode::Newtype { ty } => todo!(),
            SubcommandVariantMode::Struct { fields } => todo!(),
        }

        let local_positional_arms = visit_positional_arms_for_fields(
            &fields_ident,
            &argument,
            &positional_parameter_ident,
            &arg_ident,
            &add_arg_ident,
            &variant.fields,
        );

        quote! {
            Self :: #variant_ident #variant_bind => #variant_body
        }

        quote! {
            Self :: #variant_ident { ref mut #fields_ident, ref mut position, .. } => {
                #(#local_positional_arms)*

                ::core::result::Result::Err(
                    ::debate::state::Error::unrecognized(())
                )
            }
        }
    });

    let long_option_arms = parsed_variants.variants.iter().map(|variant| {
        let variant_ident = variant.ident.raw();
        let body = complete_long_option_body(
            &fields_ident,
            &argument,
            &option,
            &parameter_ident,
            &variant.fields,
            None,
        );

        quote! {
            Self :: #variant_ident { ref mut #fields_ident, .. } => #body,
        }
    });

    let long_arms = parsed_variants.variants.iter().map(|variant| {
        let variant_ident = variant.ident.raw();
        let body = complete_long_body(
            &fields_ident,
            &argument,
            &option,
            &parameter_ident,
            &variant.fields,
            None,
        );

        quote! {
            Self :: #variant_ident { ref mut #fields_ident, .. } => #body,
        }
    });

    let short_arms = parsed_variants.variants.iter().map(|variant| {
        let variant_ident = variant.ident.raw();
        let body = complete_short_body(
            &fields_ident,
            &argument,
            &option,
            &parameter_ident,
            &variant.fields,
            None,
        );

        quote! {
            Self :: #variant_ident { ref mut #fields_ident, .. } => #body,
        }
    });

    let get_subcommand_arms = parsed_variants.variants.iter().map(|variant| {
        let variant_ident = variant.ident.raw();
        let command_name = variant.command.as_str();

        let field_visitor_calls =
            get_subcommand_field_visitor_calls(&fields_ident, &visitor, &variant.fields);

        quote! {
            Self :: #variant_ident { ref #fields_ident, .. } => {
                let #visitor = ::debate::state::SubcommandPathVisitorWithItem::new(
                    ::debate::state::SubcommandPathItem::Command(#command_name),
                    #visitor,
                );

                #(#field_visitor_calls)*

                let _ = #fields_ident;

                ::core::result::Result::Ok(#visitor.call())
            }
        }
    });

    let build_body_fallback = match parsed_variants.fallback {
        Fallback::Explicit(ident) => quote! { Self :: #ident },
        Fallback::Internal(_) => quote! {
            return ::core::result::Result::Err(
                ::debate::build::Error::required_subcommand(
                    #all_commands_slice,
                )
            )
        },
    };

    let build_body_variant_arms = parsed_variants.variants.iter().map(|variant| {
        let variant_ident = variant.ident.raw();
        let field_initializers = final_field_initializers(&fields_ident, &variant.fields);

        let variant_body = match variant.mode {
            SubcommandVariantMode::Unit => quote! {},
            SubcommandVariantMode::Tuple => quote! {
                ( #(#field_initializers,)* )
            },
            SubcommandVariantMode::Struct => quote! {
                { #(#field_initializers,)* }
            },
        };

        quote! {
            Self :: State :: #variant_ident { #fields_ident, ..} => (
                Self :: #variant_ident #variant_body
            )
        }
    });

    Ok({
        quote! {
            #[doc(hidden)]
            #[derive(::core::default::Default)]
            enum #state_ident <#lifetime> {
                #[default]
                #fallback_ident,
                #(#state_variants,)*
            }

            impl<#lifetime> ::debate::state::State<#lifetime> for #state_ident <#lifetime> {
                fn add_positional<E>(
                    &mut self,
                    #argument: & #lifetime ::debate_parser::Arg
                ) -> ::core::result::Result<(), E>
                where
                    E: ::debate::state::Error<#lifetime, ()>
                {
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

                fn add_long_option<E>(
                    &mut self,
                    option: & #lifetime ::debate_parser::Arg,
                    argument: & #lifetime ::debate_parser::Arg
                ) -> ::core::result::Result<(), E>
                where
                    E: ::debate::state::Error<#lifetime, ()>
                {
                    // TODO: for now we just say unrecognized, but in the future
                    // we need to instead return `rejected` for variants that
                    // we do recognize but which are not a part of the current
                    // subcommand, or are observed before a subcommand is selected
                    match *self {
                        Self :: #fallback_ident => ::core::result::Result::Err(
                            ::debate::state::Error::unrecognized(())
                        ),
                        #(#long_option_arms)*
                }
                }

                fn add_long<A, E>(
                    &mut self,
                    option: & #lifetime ::debate_parser::Arg,
                    argument: A
                ) -> ::core::result::Result<(), E>
                where
                    A: ::debate::parameter::ArgAccess<#lifetime>,
                    E: ::debate::state::Error<#lifetime, A>
                {
                    match *self {
                        Self :: #fallback_ident => ::core::result::Result::Err(
                            ::debate::state::Error::unrecognized(#argument)
                        ),
                        #(#long_arms)*
                    }
                }

                fn add_short<A, E>(
                    &mut self,
                    option: u8,
                    argument: A
                ) -> ::core::result::Result<(), E>
                where
                    A: ::debate::parameter::ArgAccess<#lifetime>,
                    E: ::debate::state::Error<#lifetime, A>
                {
                    match *self {
                        Self :: #fallback_ident => ::core::result::Result::Err(
                            ::debate::state::Error::unrecognized(#argument)
                        ),
                        #(#short_arms)*
                    }
                }

                fn get_subcommand_path<V: ::debate::state::SubcommandVisitor>(
                    &self,
                    #visitor: V,
                ) -> ::core::result::Result<V::Output, V> {
                    match *self {
                        Self :: #fallback_ident => ::core::result::Result::Err(#visitor),

                        #(#get_subcommand_arms)*
                    }
                }
            }

            impl<#lifetime> ::debate::build::BuildFromArgs<#lifetime> for #name #type_lifetime {
                type State = #state_ident <#lifetime>;

                fn build<E>(state: Self::State) -> Result<Self,E>
                where
                    E: ::debate::build::Error
                {
                    ::core::result::Result::Ok(
                        match state {
                            Self :: State :: #fallback_ident => #build_body_fallback,
                            #(#build_body_variant_arms,)*
                        }
                    )
                }
            }
        }
    })
}
