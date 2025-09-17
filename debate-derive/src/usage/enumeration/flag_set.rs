use heck::ToSnakeCase;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{Ident, Token, Variant, punctuated::Punctuated};

use crate::{
    common::{
        FlagTags,
        enumeration::{
            ValueEnumAttr,
            flag_set::{
                FlagSetFlag, FlagSetFlagExtra, FlagSetFlagInfo, FlagType, ParsedFlagSetInfo,
                VariantFieldSourcesMode, compute_grouped_flags,
            },
        },
    },
    generics::AngleBracedLifetime,
    usage::common::{FlagUsageInfo, flag_usage},
};

impl<'a, T: FlagSetFlagExtra<'a>> FlagUsageInfo for &'a FlagSetFlagInfo<T> {
    fn docs(&self) -> &crate::common::Description {
        &self.docs
    }

    fn defaulted(&self) -> bool {
        self.default.is_some()
    }

    fn ty(&self) -> FlagType<'_> {
        self.info.as_flag_set_type()
    }

    fn tags(&self) -> FlagTags<&str, char> {
        self.tags.simplify()
    }

    fn placeholder(&self) -> &str {
        self.placeholder.as_str()
    }
}

impl FlagUsageInfo for &FlagSetFlag<'_> {
    fn docs(&self) -> &crate::common::Description {
        self.docs
    }

    fn defaulted(&self) -> bool {
        true
    }

    fn ty(&self) -> FlagType<'_> {
        self.ty
    }

    fn tags(&self) -> FlagTags<&str, char> {
        self.tags
    }

    fn placeholder(&self) -> &str {
        self.placeholder
    }
}

pub fn derive_usage_enum_flag_set(
    name: &Ident,
    variants: &Punctuated<Variant, Token![,]>,
    lifetime: Option<&AngleBracedLifetime>,
    attr: &ValueEnumAttr,
) -> syn::Result<TokenStream2> {
    let parsed = ParsedFlagSetInfo::from_variants(
        variants,
        attr.long.map(|long| long.span()).as_ref(),
        attr.short.map(|short| short.span()).as_ref(),
    )?;
    let (variant_sources, parsed_flags) = compute_grouped_flags(&parsed.variants)?;
    let command_name = name.to_string().to_snake_case();

    let groups = variant_sources.values().map(|variant| match variant.mode {
        VariantFieldSourcesMode::Plain(ref source) => {
            let flag = flag_usage(source.field);
            quote! { &[ #flag ] }
        }
        VariantFieldSourcesMode::Struct(ref sources) => {
            let flags = sources.iter().map(|source| flag_usage(source.field));
            quote! { &[ #(#flags,)* ] }
        }
    });

    let all_flags = parsed_flags.iter().map(flag_usage);

    Ok(quote! {
        impl #lifetime ::debate::help::Usage for #name #lifetime {
            const NAME: &'static str = #command_name;
            const DESCRIPTION: ::debate::help::Description<'static> =
                ::debate::help::Description::new("TODO FLAG SETS");
            const ITEMS: ::debate::help::UsageItems<'static> =
                ::debate::help::UsageItems::Exclusive {
                    requirement: ::debate::help::Requirement::Mandatory,
                    groups: &[ #(#groups,)* ],
                    all_flags: &[ #(#all_flags,)* ],
                };
        }
    })
}
