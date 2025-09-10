use std::collections::HashMap;

use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{Ident, Lifetime, Token, Variant, punctuated::Punctuated};

use crate::{
    common::enumeration::flag_set::{
        FlagSetType, FlagSetVariant, ParsedFlagSetInfo, VariantMode, compute_grouped_flags,
    },
    from_args::common::{complete_long_body, indexed},
    generics::AngleBracedLifetime,
};

use super::ValueEnumAttr;

pub fn derive_args_enum_flag_set(
    name: &Ident,
    variants: &Punctuated<Variant, Token![,]>,
    lifetime: &Lifetime,
    type_lifetime: Option<&AngleBracedLifetime>,
    attr: &ValueEnumAttr,
) -> syn::Result<TokenStream2> {
    let parsed = ParsedFlagSetInfo::from_variants(
        variants,
        attr.long.map(|long| long.span()).as_ref(),
        attr.short.map(|short| short.span()).as_ref(),
    )?;
    let parsed_flags = compute_grouped_flags(&parsed.variants)?;

    let keyed_variants: HashMap<&str, &FlagSetVariant<'_>> = parsed
        .variants
        .iter()
        .map(|variant| (variant.ident.as_str(), variant))
        .collect();

    let superposition_ident = &parsed.superposition;

    /*
    Let's talk algorithm.

    enum Flags {
        Flag,
        Arg(arg),
        Set3 {
            bar: bool,
            baz: bool,
        }
        Set {
            foo: i32,
            bar: bool,
        }
        Set2 {
            foo: i32,
            baz: bool,
        }
    }

    The flags here are --flag, --arg, --foo, --bar, and --baz. Most of them
    are unique (and, in fact, we expect a vast majority of flags using the
    enum to be unique).

    We require that each instance of a tag across the enum be identical.

    Each time we receive a flag, we check if it's unique, and if so,
    unconditionally switch our state to that flag.

    Otherwise, we update the viability set. The viability set is initially that
    all variants are viable, but each flag disables viability for each variant
    it isn't a member of. The idea is that, because flags can be in default
    states, it isn't sufficient to just try each one in sequence; we have to
    actively track which set the user is opting into, and consider only those.

    For each viable variant, we'll attempt to construct it, falling through
    if any of its non-optional fields are absent.

    If ALL of the flags are unique, then we won't even bother tracking the
    viability set; it's a large array of bools and a cost we don't need to pay.

    TODO: figure out error reporting for this.
    */

    // Reuse these everywhere
    let arg_ident = format_ident!("arg");
    let add_arg_ident = format_ident!("add_arg");
    let present_ident = format_ident!("present");
    let add_present_ident = format_ident!("add_present");

    let fields_ident = format_ident!("fields");

    let parameter_ident = format_ident!("Parameter");
    let positional_parameter_ident = format_ident!("PositionalParameter");

    let state_ident = format_ident!("__{name}State");
    let argument = format_ident!("argument");
    let flag = format_ident!("flag");
    let visitor = format_ident!("visitor");

    let state_ident = format_ident!("__{name}State");

    let any_multi_flags = parsed_flags.iter().any(|flag| flag.variants.len() > 1);

    let flag_types = parsed_flags
        .iter()
        .take_while(|_| any_multi_flags)
        .map(|flag| &flag.ty)
        .map(|flag_type| {
            quote! {
                ::core::option::Option< #flag_type >
            }
        });

    let flag_inits = parsed_flags
        .iter()
        .take_while(|_| any_multi_flags)
        .map(|_| {
            quote! {
                ::core::option::Option::None
            }
        });

    let rejection_set_types = parsed
        .variants
        .iter()
        .take_while(|_| any_multi_flags)
        .map(|_| quote! { bool });

    let rejection_set_inits = parsed
        .variants
        .iter()
        .take_while(|_| any_multi_flags)
        .map(|_| quote! { false });

    let state_variants = parsed.variants.iter().map(|variant| {
        let ident = variant.ident.raw();

        match variant.mode {
            // We don't use options for units and newtypes; there's only one
            // possible flag that triggers these states, so it's guaranteed
            // that its argument is real when we arrived to it.
            VariantMode::Plain(flag) => match flag.ty {
                FlagSetType::Unit => quote! { #ident { #fields_ident: ( (), ), } },
                FlagSetType::Typed(ty) => quote! { #ident { #fields_ident: ( #ty, ), } },
            },

            VariantMode::Struct(fields) => {
                let field_types = fields.iter().map(|(_, field)| field.ty).map(|field_type| {
                    quote! {
                        ::core::option::Option< #field_type >
                    }
                });

                quote! {
                    #ident {
                        #fields_ident: ( #(#field_types,)* ),
                    }
                }
            }
        }
    });

    let long_flags = indexed(&parsed_flags)
        .find_map(|(index, flag)| flag.tags.long().map(|long| (index, long, flag)));

    // let add_long_arms = long_flags.clone().map(|(index, long, flag)| {
    //     let scrutinee = long.make_scrutinee();

    //     // Reject each variant that this flag is NOT a member of.
    //     // TODO: make this deteministic (indexmap?)
    //     let update_rejections = flag
    //         .excluded
    //         .values()
    //         .take_while(|_| any_multi_flags)
    //         .map(|&index| Index::from(index))
    //         .map(|index| {
    //             quote! {
    //                 rejection_set.#index = true;
    //             }
    //         });

    //     let variant_arms = flag.variants.iter().map(|&(variant_ident, index)| {
    //         let body = match index {
    //             None => match flag.info.overridable {
    //                 true => apply_new_arg_to_generic_field(
    //                     &fields_ident,
    //                     &argument,
    //                     &Index::from(0),
    //                     &parameter_ident,
    //                     &present_ident,
    //                     |value| quote! {#value},
    //                 ),
    //                 false => quote! {
    //                     ::debate::parameter::Parameter::add_present(
    //                         &mut #fields_ident.0,
    //                         #argument,
    //                     )
    //                 },
    //             },
    //             Some(index) => apply_arg_to_field(
    //                 &fields_ident,
    //                 &argument,
    //                 &Index::from(index),
    //                 &parameter_ident,
    //                 &present_ident,
    //                 match flag.info.overridable {
    //                     true => None,
    //                     false => Some(&add_present_ident),
    //                 },
    //             ),
    //         };

    //         quote! {
    //             #variant_ident { ref mut #fields_ident } => match (#body) {
    //                 ::core::result::Result::Ok(()) => ::core::result::Result::Ok(()),
    //                 ::core::result::Result::Err(err) => ::core::result::Result::Err(
    //                     ::debate::state::Error::parameter("TODO NAME ME", err)
    //                 ),
    //             }
    //         }
    //     });

    //     quote! {
    //         #scrutinee => match *self {
    //             Self :: #superposition_ident { ref mut fields, ref mut rejection_set, .. } => {
    //                 // Step 1: check the rejection set for a conflict
    //                 // Step 2: update the rejection set with all newly rejeced variants
    //                 // Step 3: check if this flag transitions us to a known state.
    //                 //   if so:
    //                 //     transition to that state. Bring any relevant arguments along.
    //                 //     parse the argument with `present`.
    //                 //   otherwise:
    //                 //     parse the argument with `apply_arg_to_field` into the
    //                 //     superposition fields.
    //                 // Steps to compute conflict: keep track of the previously
    //                 // false rejection set items. If the rejection set becomes
    //                 // fully rejected, pick the first previously false one,
    //                 // and use it.
    //                 // Ideally this logic is not repeated in every scrutinee
    //                 // arm; we'd find a way to shunt it down towards the end.

    //                 #(#update_rejections*)

    //             },

    //             #(Self :: #variant_arms,)*

    //             // We recognized the flag, but it's conflicting with a selected
    //             // state
    //             _ => ::core::result::Result::Err(
    //                 // Again: unrecognized is wrong here
    //                 ::debate::state::Error::unrecognized(todo!())
    //             ),
    //         }
    //     }
    // });

    let add_long_arms = parsed.variants.iter().map(|variant| match variant.mode {
        VariantMode::Plain(field) => {
            let ident = variant.ident;
            let arm = field.tags.long().map(|long| {
                quote! {
                    todo!()
                }
            });

            quote! {
                #ident => match #flag {
                    #arm,

                    // We recognized the flag, but it's conflicting with a
                    // selected state
                    _ => ::core::result::Result::Err(
                        // Again: unrecognized is wrong here
                        ::debate::state::Error::unrecognized(todo!())
                    ),
                }
            }
        }

        VariantMode::Struct(fields) => {
            let ident = variant.ident;

            // TODO: find a way to use complete_long_body here. Will certainly
            // involve introducing a trait somewhere to make it more generic.
            // We also need to find a way to switch between unrecognized flags
            // (which need to propagate their argument back to the caller)
            // and conflicting flags (which do not).
            let body = complete_long_body(
                &fields_ident,
                &argument,
                &flag,
                &parameter_ident,
                fields,
                None,
            );

            quote! {
                #ident { ref mut fields } => #body
            }
        }
    });

    Ok(quote! {
        enum #state_ident <#lifetime> {
            // In the generic state, we haven't selected a specific variant yet
            #superposition_ident {
                fields: ( #(#flag_types,)* ),
                rejection_set: ( #(#rejection_set_types,)* ),
                phantom: ::core::marker::PhantomData<& #lifetime ()>,
            },

            #(#state_variants,)*
        }

        impl ::core::default::Default for #state_ident <'_> {
            fn default() -> Self {
                Self :: #superposition_ident {
                    all_flags: ( #(#flag_inits,)* ),
                    rejection_set: ( #(#rejection_set_inits,)* ),
                    phantom: ::core::marker::PhantomData,
                }
            }
        }

        impl<#lifetime> ::debate::state::State<#lifetime> for #state_ident <#lifetime> {
            // Flag sets never accept positionals
            fn add_positional<E>(
                &mut self,
                _arg: & #lifetime ::debate_parser::Arg
            ) -> ::core::result::Result<(), E>
            where
                E: ::debate::state::Error<#lifetime, ()>
            {
                ::core::result::Result::Err(
                    ::debate::state::Error::unrecognized(())
                )
            }

            fn add_long_argument<E>(
                &mut self,
                #flag: & #lifetime ::debate_parser::Arg,
                #argument: & #lifetime ::debate_parser::Arg
            ) -> ::core::result::Result<(), E>
            where
                E: ::debate::state::Error<#lifetime, ()>
            {
                /*
                Current vision for the match

                match state {
                    Superposition => handle_superposition(flag),
                    State1 => match flag {
                        flag1 => handle(),
                        flag1 | flag2 => conflict(),
                        _ => unrecognized,
                    }
                    State2 => match flag {
                        flag1 => handle()
                        flag1 | flag2 => conflict(),
                        _ => unrecognized,
                    }
                    State3 => match flag {
                        flag1 => handle(),
                        flag2 => handle(),
                        flag1 | flag2 => conflict(),
                        _ => unrecognized,
                    }
                }

                This gives us the easiest story when it comes to emitting
                conflict information in the state branches, and the best
                opportunity to potentially reuse `complete_flag_body`.

                We could trust the optimizer to remove the duplicate tags
                in the conflict cases; that'll mostly depend on how easy it
                is to manually compute the reduced set. It will be VERY easy
                to just list them all there along with a suppressed warning.
                 */
                match *self {
                    Self :: #superposition_ident {ref mut #fields_ident, ref mut rejection_set } {

                    }

                    #(#)
                }
            }

            fn add_long<A, E>(
                &mut self,
                #flag: & #lifetime ::debate_parser::Arg,
                #argument: A
            ) -> ::core::result::Result<(), E>
            where
                A: ::debate::parameter::ArgAccess<#lifetime>,
                E: ::debate::state::Error<#lifetime, A>
            {
                let #fields_ident = &mut self.fields;

                #long_body
            }

            fn add_short<A, E>(
                &mut self,
                #flag: u8,
                #argument: A
            ) -> ::core::result::Result<(), E>
            where
                A: ::debate::parameter::ArgAccess<#lifetime>,
                E: ::debate::state::Error<#lifetime, A>
            {
                let #fields_ident = &mut self.fields;

                #short_body
            }
        }
    })
}
