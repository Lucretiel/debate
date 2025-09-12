use std::collections::HashMap;

use itertools::Itertools;
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, format_ident, quote};
use syn::{Ident, Index, Lifetime, Token, Variant, punctuated::Punctuated};

use crate::{
    common::{
        FlagTags, IdentString,
        enumeration::flag_set::{
            FlagFieldInfo, FlagSetFlag, FlagSetFlagInfo, FlagSetVariant, ParsedFlagSetInfo,
            PlainFlagInfo, VariantMode, compute_grouped_flags,
        },
    },
    from_args::common::{FieldNature, FlagField, MakeScrutinee, complete_flag_body, indexed},
    generics::AngleBracedLifetime,
};

use super::ValueEnumAttr;

// There doesn't exist a rejection set or superposition field states. Just
// transition immediately.
fn handle_simple_long_superposition_argument(
    fields_ident: &Ident,
    flags: &[FlagSetFlag<'_>],
) -> TokenStream2 {
    let arms = indexed(flags)
        .filter_map(|(index, flag)| flag.tags.long().map(move |long| (long, index, flag)))
        .map(|(long, index, flag): (&str, Index, &FlagSetFlag<'_>)| {
            let scrutinee = long.make_scrutinee();

            // TODO: using `exactly_one` here is probably going to be the
            // secret to generalizing between this and regular superpositions.
            let destination = flag.variants.first().unwrap();

            quote! {
                #scrutinee => {

                }
            }
        });

    quote! {
        match flag.bytes() {

        }
    }
}

fn handle_long_superposition_argument(
    fields_ident: &Ident,
    flags: &[FlagSetFlag<'_>],
) -> TokenStream2 {
    // Step 1: check the rejection set for a conflict
    // Step 2: update the rejection set with all newly rejeced variants
    // Step 3: check if this flag transitions us to a known state.
    //   when doing this check, check the flag itself first (it might
    //   be unique), and fall back to the rejection set.
    //   if so:
    //     transition to that state. Bring any relevant arguments along.
    //     parse the argument with `present`.
    //   otherwise:
    //     parse the argument with `apply_arg_to_field` into the
    //     superposition fields.
    // Steps to compute conflict: keep track of the previously
    // false rejection set items. If the rejection set becomes
    // fully rejected, pick the first previously false one,
    // and use it.

    let arms = indexed(flags)
        .filter_map(|(index, flag)| flag.tags.long().map(move |long| (long, index, flag)))
        .map(|(long, index, flag): (&str, Index, &FlagSetFlag<'_>)| {
            let scrutinee = long.make_scrutinee();

            let update_exclusions = flag
                .excluded
                .values()
                .map(|&i| Index::from(i))
                .map(|index| quote! { rejection_set.#index = true; });

            quote! {
                #scrutinee => {
                    #update_exclusions

                    if Self::total_rejection(&rejection_set) {
                        todo!("conflict")
                    }

                    // TODO: transition to a known state


                }
            }
        });

    quote! {
        match flag.bytes() {

        }
    }
}

impl<'a> FlagField<'a> for &'a FlagSetFlagInfo<FlagFieldInfo<'a>> {
    #[inline(always)]
    fn name(&self) -> &str {
        self.info.ident.as_str()
    }

    #[inline(always)]
    fn tags(&self) -> FlagTags<&'a str, char> {
        self.tags.simplify()
    }

    #[inline(always)]
    fn overridable(&self) -> bool {
        self.overridable.is_some()
    }

    #[inline(always)]
    fn nature(&self) -> FieldNature<'a> {
        FieldNature::Optional
    }
}

/// For plain flags, we pair them with the variant they came from.
impl<'a> FlagField<'a> for &(&'a IdentString<'a>, &'a FlagSetFlagInfo<PlainFlagInfo<'a>>) {
    #[inline(always)]
    fn name(&self) -> &str {
        self.0.as_str()
    }

    #[inline(always)]
    fn tags(&self) -> FlagTags<&'a str, char> {
        self.1.tags.simplify()
    }

    #[inline(always)]
    fn overridable(&self) -> bool {
        self.1.overridable.is_some()
    }

    #[inline(always)]
    fn nature(&self) -> FieldNature<'a> {
        FieldNature::Extant
    }
}

fn conflict_detection_arm(
    recognized_tags: impl IntoIterator<Item = impl MakeScrutinee>,
) -> TokenStream2 {
    let mut tags = recognized_tags.into_iter().map(|tag| tag.make_scrutinee());

    match tags.next() {
        None => quote! {},
        Some(recognize) => quote! {
            #recognize #(| #tags)* => todo!("handle conflict")
        },
    }
}

// We use slices everywhere here because we call `indexed` on them, and we
// want to enforce (more or less) that we're using the complete set, from
// a slice, rather than a filtered set from an iterator.

fn complete_flagset_flag_body<'a, Tag: MakeScrutinee, T>(
    fields_ident: &Ident,
    argument_ident: &Ident,
    option_expr: impl ToTokens,

    fields: &'a [T],
    all_tag_scrutinees: impl IntoIterator<Item = Tag>,
    get_tag: impl Fn(FlagTags<&'a str, char>) -> Option<Tag>,

    parameter_method: &Ident,
    add_parameter_method: &Ident,
) -> TokenStream2
where
    &'a T: FlagField<'a>,
{
    complete_flag_body(
        fields_ident,
        argument_ident,
        option_expr,
        None,
        indexed(fields),
        get_tag,
        parameter_method,
        add_parameter_method,
        conflict_detection_arm(all_tag_scrutinees),
        [],
        argument_ident,
    )
}

fn complete_long_arg_body<'a, T>(
    fields_ident: &Ident,
    argument_ident: &Ident,
    option_ident: &Ident,

    fields: &'a [T],
    all_tag_scrutinees: impl IntoIterator<Item = &'a str>,
) -> TokenStream2
where
    &'a T: FlagField<'a>,
{
    complete_flagset_flag_body(
        fields_ident,
        argument_ident,
        quote! { #option_ident.bytes() },
        fields,
        all_tag_scrutinees,
        |tags| tags.long(),
        &format_ident!("arg"),
        &format_ident!("add_arg"),
    )
}

fn complete_long_body<'a, T>(
    fields_ident: &Ident,
    argument_ident: &Ident,
    option_ident: &Ident,

    fields: &'a [T],
    all_tag_scrutinees: impl IntoIterator<Item = &'a str>,
) -> TokenStream2
where
    &'a T: FlagField<'a>,
{
    complete_flagset_flag_body(
        fields_ident,
        argument_ident,
        quote! { #option_ident.bytes() },
        fields,
        all_tag_scrutinees,
        |tags| tags.long(),
        &format_ident!("present"),
        &format_ident!("add_present"),
    )
}

fn complete_short_body<'a, T>(
    fields_ident: &Ident,
    argument_ident: &Ident,
    option_ident: &Ident,

    fields: &'a [T],
    all_tag_scrutinees: impl IntoIterator<Item = char>,
) -> TokenStream2
where
    &'a T: FlagField<'a>,
{
    complete_flagset_flag_body(
        fields_ident,
        argument_ident,
        option_ident,
        fields,
        all_tag_scrutinees,
        |tags| tags.short(),
        &format_ident!("present"),
        &format_ident!("add_present"),
    )
}

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

    After we're done receiving new arguments, we'll attempt to construct the
    final enum. We do this by attempting to construct each viable variant in
    sequence, falling through to each next viable variant when there are
    Missing Required Value errors.

    If ALL of the flags are unique, then we won't even bother tracking the
    viability set; it's a large array of bools and a cost we don't need to pay.
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

    let any_multi_flags = parsed_flags.iter().any(|flag| flag.variants.len() > 1);

    // Note that anything that interacts with the flag flags list or with the
    // viability set is conditional on `any_multi_flags`.
    let flag_types = parsed_flags.iter().map(|flag| {
        let ty = &flag.ty;

        match flag.variants.len() {
            1 => quote! { () },
            _ => quote! { ::core::option::Option< #ty > },
        }
    });

    let flag_inits = parsed_flags.iter().map(|flag| match flag.variants.len() {
        1 => quote! { () },
        _ => quote! { ::core::option::Option::None },
    });

    // TODO: if there are at most 64 variants (lol), use a u64 for the
    // rejection set.
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

    // We could try to filter out state variants that are never reachable
    // during argument parsing (for instance, their set of flags is a subset
    // of another flag). In practice I expect this to be radically uncommon
    // and not worth going out of our way for.
    let state_variants = parsed.variants.iter().map(|variant| {
        let ident = variant.ident.raw();

        match variant.mode {
            // We don't use options for units and newtypes; there's only one
            // possible flag that triggers these states, so it's guaranteed
            // that its argument is real when we arrived to it.
            VariantMode::Plain(ref flag) => {
                let ty = flag.info.ty();

                // Don't forget that the `ty` for a unit variant is `()`,
                // which has a correct implementation of Parameter for our
                // needs
                quote! { #ident { #fields_ident: ( #ty, ), } }
            }

            VariantMode::Struct(ref fields) => {
                let field_types = fields.iter().map(|field| &field.info.ty);

                quote! {
                    #ident {
                        #fields_ident: ( #(
                            ::core::option::Option< #field_types >,
                        )* ),
                    }
                }
            }
        }
    });

    let long_flags = indexed(&parsed_flags)
        .find_map(|(index, flag)| flag.tags.long().map(|long| (index, long, flag)));

    // We do a rare pre-emptive collect because this operation is nakedly
    // quadratic so we want to minimize repeated computation. In the future
    // we'd even like to pre-compute the scrutinees directly, but for now we
    // still like the type safety
    let all_long_scrutinees = parsed_flags
        .iter()
        .filter_map(|flag| flag.tags.long())
        .collect_vec();

    let all_short_scrutinees = parsed_flags
        .iter()
        .filter_map(|flag| flag.tags.short())
        .collect_vec();

    // TODO: deduplicate these. The trouble is figuring out how to abscract
    // `complete_long_arg_body`, which has two generic instantiations.
    let add_long_arg_arms = parsed.variants.iter().map(|variant| {
        let body = match variant.mode {
            VariantMode::Plain(ref field) => complete_long_arg_body(
                &fields_ident,
                &argument,
                &flag,
                &[(&variant.ident, field)],
                all_long_scrutinees.iter().copied(),
            ),
            VariantMode::Struct(ref fields) => complete_long_arg_body(
                &fields_ident,
                &argument,
                &flag,
                &fields,
                all_long_scrutinees.iter().copied(),
            ),
        };

        let ident = &variant.ident;
        quote! {
            #ident { ref mut #fields_ident } => #body
        }
    });

    let add_long_arms = parsed.variants.iter().map(|variant| {
        let body = match variant.mode {
            VariantMode::Plain(ref field) => complete_long_body(
                &fields_ident,
                &argument,
                &flag,
                &[(&variant.ident, field)],
                all_long_scrutinees.iter().copied(),
            ),
            VariantMode::Struct(ref fields) => complete_long_body(
                &fields_ident,
                &argument,
                &flag,
                &fields,
                all_long_scrutinees.iter().copied(),
            ),
        };

        let ident = &variant.ident;
        quote! {
            #ident { ref mut #fields_ident } => #body
        }
    });

    let add_short_arms = parsed.variants.iter().map(|variant| {
        let body = match variant.mode {
            VariantMode::Plain(ref field) => complete_short_body(
                &fields_ident,
                &argument,
                &flag,
                &[(&variant.ident, field)],
                all_short_scrutinees.iter().copied(),
            ),
            VariantMode::Struct(ref fields) => complete_short_body(
                &fields_ident,
                &argument,
                &flag,
                fields,
                all_short_scrutinees.iter().copied(),
            ),
        };

        let ident = &variant.ident;
        quote! {
            #ident { ref mut #fields_ident } => #body
        }
    });

    let rejection_set_type = quote! {
         ( #(#rejection_set_types,)* )
    };

    Ok(quote! {
        enum #state_ident <#lifetime> {
            // In the generic state, we haven't selected a specific variant yet
            #superposition_ident {
                #fields_ident: ( #(#flag_types,)* ),
                rejection_set: #rejection_set_type,
                phantom: ::core::marker::PhantomData<& #lifetime ()>,
            },

            #(#state_variants,)*
        }

        impl #state_ident<'_> {
            fn total_rejection(set: &#rejection_set_type) -> bool {
                todo!()
            }
        }

        impl ::core::default::Default for #state_ident <'_> {
            fn default() -> Self {
                Self :: #superposition_ident {
                    #fields_ident: ( #(#flag_inits,)* ),
                    rejection_set: ( #(#rejection_set_inits,)* ),
                    phantom: ::core::marker::PhantomData,
                }
            }
        }

        impl <#lifetime> #state_ident < #lifetime {
            fn handle_long_argument_during_superposition<A, EA, E>(
                &mut self,
                #flag: & #lifetime ::debate_parser::Arg,
                #argument: A,
            ) -> ::core::result::Result<(), E>
            where
                E: ::debate::state::Error<#lifetime, EA>
            {

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
                match *self {
                    Self :: #superposition_ident {ref mut #fields_ident, ref mut rejection_set, .. } => {

                    }
                    #(#add_long_arg_arms,)*
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
                match *self {
                    Self :: #superposition_ident {ref mut #fields_ident, ref mut rejection_set, .. } => {

                    }
                    #(#add_long_arms,)*
                }
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
                match *self {
                    Self :: #superposition_ident {ref mut #fields_ident, ref mut rejection_set, .. } => {

                    }
                    #(#add_short_arms,)*
                }
            }
        }
    })
}
