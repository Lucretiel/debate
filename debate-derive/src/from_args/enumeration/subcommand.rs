use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use darling::FromAttributes;
use heck::ToKebabCase;
use itertools::Itertools as _;
use proc_macro2::{Literal, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{
    Fields, FieldsNamed, FieldsUnnamed, Generics, Ident, Token, Variant, punctuated::Punctuated,
    spanned::Spanned,
};

use crate::from_args::{
    common::{
        IdentString, OptionFieldInfo, OptionTag, ParsedFieldInfo, complete_long_body,
        complete_long_option_body, complete_short_body, final_field_initializers,
        struct_state_block_from_fields, struct_state_init_block_from_fields,
        visit_positional_arms_for_fields,
    },
    from_args_impl,
};

/// Ident of the unselected subcommand (that is, the enum variant)
enum Fallback<'a> {
    /// The user explicitly included this variant with `debate(fallback)`
    Explicit(&'a Ident),

    /// There's no user-provided unselected subcommand. This ident isn't a
    /// member of the original enum and has been computed to avoid a collision.
    Internal(Ident),
}

impl Fallback<'_> {
    pub fn ident(&self) -> &Ident {
        match *self {
            Fallback::Explicit(ident) => ident,
            Fallback::Internal(ref ident) => ident,
        }
    }
}

#[derive(darling::FromAttributes)]
#[darling(attributes(debate))]
struct VariantAttr {
    fallback: Option<()>,
}

enum VariantMode {
    Unit,
    Tuple,
    Struct,
}

struct ParsedSubcommandVariant<'a> {
    ident: IdentString<'a>,

    /// Case-converted command name
    command: String,

    /// All of the fields in this variant. Basically agnostic to
    /// struct/unit/tuple distinctions.
    fields: Vec<ParsedFieldInfo<'a>>,

    mode: VariantMode,
}

struct ParsedSubcommandInfo<'a> {
    fallback: Fallback<'a>,
    variants: Vec<ParsedSubcommandVariant<'a>>,
}

impl<'a> ParsedSubcommandInfo<'a> {
    pub fn from_variants(variants: impl IntoIterator<Item = &'a Variant>) -> syn::Result<Self> {
        let mut fallback: Option<&Ident> = None;
        let mut parsed_variants = Vec::new();

        for variant in variants {
            let attr = VariantAttr::from_attributes(&variant.attrs)?;

            if let Some(()) = attr.fallback {
                if let Some(fallback) = fallback {
                    let mut err = syn::Error::new(
                        variant.span(),
                        "can't have more than one fallback variant",
                    );

                    err.combine(syn::Error::new(fallback.span(), "previous fallback here"));

                    return Err(err);
                }

                if !matches!(variant.fields, Fields::Unit) {
                    return Err(syn::Error::new(
                        variant.span(),
                        "fallback variant must be a unit variant",
                    ));
                }

                fallback = Some(&variant.ident);
            } else {
                let ident = IdentString::new(&variant.ident);
                let command = ident.as_str().to_kebab_case();
                let fields = match variant.fields {
                    Fields::Unnamed(FieldsUnnamed {
                        unnamed: ref fields,
                        ..
                    })
                    | Fields::Named(FieldsNamed {
                        named: ref fields, ..
                    }) => fields
                        .iter()
                        .map(|field| ParsedFieldInfo::from_field(field))
                        .try_collect()?,

                    Fields::Unit => Vec::new(),
                };
                let mode = match variant.fields {
                    Fields::Named(_) => VariantMode::Struct,
                    Fields::Unnamed(_) => VariantMode::Tuple,
                    Fields::Unit => VariantMode::Unit,
                };

                parsed_variants.push(ParsedSubcommandVariant {
                    ident,
                    command,
                    fields,
                    mode,
                });
            }
        }

        let fallback = match fallback {
            Some(fallback) => Fallback::Explicit(fallback),
            None => {
                let mut base_ident = format_ident!("Fallback");

                // Performance nitpick: == on an identifier is internally doing
                // to_string
                while parsed_variants
                    .iter()
                    .any(|variant| *variant.ident.raw() == base_ident)
                {
                    base_ident = format_ident!("_{base_ident}");
                }

                Fallback::Internal(base_ident)
            }
        };

        Ok(Self {
            fallback,
            variants: parsed_variants,
        })
    }
}

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
    generics: &Generics,
) -> syn::Result<TokenStream2> {
    // Reuse these everywhere
    let arg_ident = format_ident!("arg");
    let add_arg_ident = format_ident!("add_arg");
    let fields_ident = format_ident!("fields");

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
        |state_ident, lifetime| {
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
                enum #state_ident<#lifetime> {
                    #[default]
                    #fallback_ident,
                    #(#state_variants,)*
                }
            }
        },
        |state_ident, lifetime| quote! { #state_ident<#lifetime> },
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
                let body =
                    complete_long_option_body(&fields_ident, argument, option, &variant.fields);

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
                let body = complete_long_body(&fields_ident, argument, option, &variant.fields);

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
                let body = complete_short_body(&fields_ident, argument, option, &variant.fields);

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
                        ::debate::from_args::Error::required_subcommand(
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
