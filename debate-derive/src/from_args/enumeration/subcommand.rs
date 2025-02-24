use darling::FromAttributes;
use heck::ToKebabCase;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{
    Attribute, DataEnum, Fields, Generics, Ident, Token, Variant, punctuated::Punctuated,
    spanned::Spanned,
};

use crate::from_args::common::IdentString;

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

struct ParsedSubcommandVariant<'a> {
    ident: IdentString<'a>,

    /// Case-converted command name
    command: String,
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

                parsed_variants.push(ParsedSubcommandVariant { ident, command });
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

pub fn derive_args_enum_subcommand(
    name: &Ident,
    variants: &Punctuated<Variant, Token![,]>,
    generics: &Generics,
    attrs: &[Attribute],
) -> syn::Result<TokenStream2> {
    let state_ident = format_ident!("__{name}State");

    let parsed_variants = ParsedSubcommandInfo::from_variants(variants);

    Ok(quote! {
        #[doc(hidden)]
        #[derive(::core::default::Default)]
        enum #state_ident<'arg> {
            // TODO
        }

        impl<'arg> ::debate::state::State<'arg> for #state_ident<'arg> {
            fn add_positional<E>(
                &mut self,
                argument: ::debate_parser::Arg<'arg>
            ) -> ::core::result::Result<(), E>
            where
                E: ::debate::state::Error<'arg, ()>
            {}

            fn add_long_option<E>(
                &mut self,
                option: ::debate_parser::Arg<'arg>,
                argument: ::debate_parser::Arg<'arg>
            ) -> ::core::result::Result<(), E>
            where
                E: ::debate::state::Error<'arg, ()>
            {}

            fn add_long<A, E>(
                &mut self,
                option: ::debate_parser::Arg<'arg>,
                argument: A
            ) -> ::core::result::Result<(), E>
            where
                A: ::debate_parser::ArgAccess<'arg>,
                E: ::debate::state::Error<'arg, A>
            {}

            fn add_short<A, E>(
                &mut self,
                option: u8,
                argument: A
            ) -> ::core::result::Result<(), E>
            where
                A: ::debate_parser::ArgAccess<'arg>,
                E: ::debate::state::Error<'arg, A>
            {}
        }

        impl<'arg> ::debate::from_args::BuildFromArgs<'arg> for #name {
            fn build<E>(state: Self::State) -> ::core::result::Result<Self, E>
            where
                E: ::debate::from_args::Error<'arg>
            {}
        }
    })
}
