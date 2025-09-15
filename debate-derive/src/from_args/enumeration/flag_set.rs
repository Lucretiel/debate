use std::{collections::HashMap, f32::consts::E, iter::Copied, slice::Iter as SliceIter};

use indexmap::IndexMap;
use itertools::Itertools;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{ToTokens, format_ident, quote};
use syn::{Ident, Index, Lifetime, Token, Variant, punctuated::Punctuated, spanned::Spanned};

use crate::{
    common::{
        FlagTags, IdentString,
        enumeration::flag_set::{
            FlagFieldInfo, FlagSetFlag, FlagSetFlagInfo, FlagSetFlagSite, FlagSetFlagStateSite,
            ParsedFlagSetInfo, PlainFlagInfo, VariantFieldSource, VariantFieldSources,
            VariantFieldSourcesMode, compute_grouped_flags,
        },
    },
    from_args::common::{
        FieldNature, FlagField, MakeScrutinee, NormalFieldInfo, apply_arg_to_field,
        complete_flag_body, indexed, struct_field_initializer,
    },
    generics::AngleBracedLifetime,
};

use super::ValueEnumAttr;

fn variant_bit(i: usize) -> TokenStream2 {
    quote! { const { 1u64 << #i } }
}

fn handle_superposition_argument<'a, Tag: MakeScrutinee>(
    fields_ident: &Ident,
    viable_ident: &Ident,
    argument_ident: &Ident,

    flag_expr: impl ToTokens,
    flags: &[FlagSetFlag<'a>],
    get_tag: impl Fn(FlagTags<&'a str, char>) -> Option<Tag>,

    sources: &IndexMap<&IdentString<'_>, VariantFieldSources<'_>>,
    any_multi_flags: bool,
    parameter_method: &Ident,
    add_parameter_method: &Ident,
    flatten_rebind_argument: impl ToTokens,
) -> TokenStream2 {
    // Step 1: update the rejection set with all newly rejeced variants
    // Step 2: check for a conflict (are all variants now rejected)?
    // Step 3: check if this flag transitions us to a known state.
    //   if so:
    //     transition to that state. Bring any relevant arguments along.
    //     parse the argument with `present`.
    //   otherwise:
    //     parse the argument with `apply_arg_to_field` into the
    //     superposition fields.
    // When there's a conflict, we'll use all of the `Some` fields to indicate
    // which flags we got to the user

    let arms = indexed(flags)
        .filter_map(|(index, flag)| get_tag(flag.tags).map(move |long| (long, index, flag)))
        .map(|(tag, index, flag)| {
            let scrutinee = tag.make_scrutinee();

            let (expr, name) = if let Some(destination) = flag.unique_variant() {
                let variant = destination.ident;
                let source = sources.get(variant).expect("source must exist");
                let name = destination.site.field().unwrap_or(variant).as_str();

                let state_init = match source.mode {
                    VariantFieldSourcesMode::Plain(_) => quote! { ( value, ) },
                    VariantFieldSourcesMode::Struct(ref sources) => {
                        let inits = sources
                            .iter()
                            .enumerate()
                            .map(|(index_in_variant, source)| {
                                if index_in_variant == destination.state_site.index {
                                    quote! { ::core::option::Option::Some(value) }
                                } else {
                                    let source_index = Index::from(source.index);
                                    quote! { #fields_ident.#source_index.take() }
                                }
                            });

                        quote! { ( #(#inits,)* ) }
                    }
                };

                // Because we're in the superposition state, AND this flag has
                // a unique variant, we're guarnateed the flag hasn't been seen
                // yet, so we can use the `parameter_method` and transition
                // directly into the new state
                let expr = quote! {
                    match ::debate::parameter::Parameter::#parameter_method(#argument_ident) {
                        ::core::result::Result::Err(err) => ::core::result::Result::Err(err),
                        ::core::result::Result::Ok(value) => {
                            *self = Self :: #variant { #fields_ident: #state_init };
                            ::core::result::Result::Ok(())
                        },
                    }
                };

                (expr, name)
            } else {
                let expr = apply_arg_to_field(
                    fields_ident,
                    argument_ident,
                    &index,
                    &format_ident!("Parameter"),
                    parameter_method,
                    match flag.overridable {
                        true => None,
                        false => Some(add_parameter_method),
                    },
                    &FieldNature::Optional,
                );

                (expr, flag.origin.as_str())
            };

            let viability_check = any_multi_flags.then(|| {
                let viable_bits = flag
                    .variants
                    .iter()
                    .map(|viable_variant| viable_variant.index)
                    .map(variant_bit);

                quote! {
                    *#viable_ident &= const { 0 #( | #viable_bits )* };

                    if *#viable_ident == 0 {
                        todo!("conflict")
                    }
                }
            });

            quote! {
                #scrutinee => {
                    #viability_check

                    match (#expr) {
                        ::core::result::Result::Ok(()) => ::core::result::Result::Ok(()),
                        ::core::result::Result::Err(err) => ::core::result::Result::Err(
                            ::debate::state::Error::parameter(#name, err)
                        ),
                    }
                }
            }
        });

    quote! {
        match (#flag_expr) {
            #(#arms,)*

            _ => ::core::result::Result::Err(
                ::debate::state::Error::unrecognized(
                    #flatten_rebind_argument
                )
            )
        }
    }
}

impl<'a> FlagField<'a> for &VariantFieldSource<'a, FlagFieldInfo<'a>> {
    #[inline(always)]
    fn name(&self) -> &str {
        self.field.info.ident.as_str()
    }

    #[inline(always)]
    fn tags(&self) -> FlagTags<&'a str, char> {
        self.field.tags.simplify()
    }

    #[inline(always)]
    fn overridable(&self) -> bool {
        self.field.overridable.is_some()
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
            #recognize #(| #tags)* => todo!("handle conflict"),
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
    flatten_rebind_argument: impl ToTokens,
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
        flatten_rebind_argument,
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
        quote! { () },
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
        argument_ident,
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
        argument_ident,
    )
}

pub fn derive_args_enum_flag_set(
    name: &Ident,
    variants: &Punctuated<Variant, Token![,]>,
    lifetime: &Lifetime,
    type_lifetime: Option<&AngleBracedLifetime>,
    attr: &ValueEnumAttr,
) -> syn::Result<TokenStream2> {
    let mut parsed = ParsedFlagSetInfo::from_variants(
        variants,
        attr.long.map(|long| long.span()).as_ref(),
        attr.short.map(|short| short.span()).as_ref(),
    )?;
    if parsed.variants.len() > 64 {
        return Err(syn::Error::new(
            variants.span(),
            "currently support a max of 64 variants of a flag set",
        ));
    }
    let (variant_sources, parsed_flags) = compute_grouped_flags(&parsed.variants)?;
    let superposition_ident = &parsed.superposition;

    // Reuse these everywhere
    let arg_ident = format_ident!("arg");
    let add_arg_ident = format_ident!("add_arg");
    let present_ident = format_ident!("present");
    let add_present_ident = format_ident!("add_present");

    let viable_ident = format_ident!("viable");
    let fields_ident = format_ident!("fields");

    let state_ident = format_ident!("__{name}State");
    let argument = format_ident!("argument");
    let flag = format_ident!("flag");

    // Data model for the state:
    // - Fields: tuple of all of the flags, in a normalized set. Each field is
    //   identified by index. Each field is replaced in the tuple with a unit
    //   if it only exists in a single variant.
    // - Viability set: a set of bools (we use a bit-shifted u64) indicating
    //   which variants are currently viable. At the beginning, all variants
    //   are viable (1), but with each incoming flag, variants outside of those
    //   flags might become non-viable (0). The viability set is replaced with
    //   a unit only in the case that ALL fields are exclusive to a single
    //   variants. We might make a fancy unit that supports bitwise ops and
    //   integer comparisons to simplify codegen.
    // - State variants: A state variant marks that a given variant has been
    //   unconditionally selected but is still being parsed. Contains all of
    //   the fields for that variant. A state variant doesn't exist if it
    //   has no exclusive flags, because there's no mechansism for us to ever
    //   switch over to it during parsing.

    let any_multi_flags = parsed_flags.iter().any(|flag| flag.variants.len() > 1);

    // Note that anything that interacts with the flag flags list or with the
    // viability set is conditional on `any_multi_flags`.
    let flag_types = parsed_flags.iter().map(|flag| {
        let ty = &flag.ty;

        match flag.unique_variant() {
            Some(_) => quote! { () },
            None => quote! { ::core::option::Option< #ty > },
        }
    });

    let flag_inits = parsed_flags.iter().map(|flag| match flag.unique_variant() {
        Some(_) => quote! { () },
        None => quote! { ::core::option::Option::None },
    });

    let viable_set_type = match any_multi_flags {
        true => quote! { u64 },
        false => quote! { () },
    };

    let viable_set_values = parsed
        .variants
        .iter()
        .enumerate()
        .map(|(i, _)| variant_bit(i));

    let viable_set_init = match any_multi_flags {
        true => quote! { const { 0u64 #( | #viable_set_values)* } },
        false => quote! { () },
    };

    // TODO: we ONLY can transition directly into states via flags that are
    // unique to that state. Any variant that has no such flags can be
    // omitted.
    let state_variants = variant_sources
        .iter()
        .filter(|(_, variant)| variant.reachable)
        .map(|(&ident, variant)| {
            match variant.mode {
                VariantFieldSourcesMode::Plain(ref source) => {
                    let ty = source.field.info.ty();

                    // Don't forget that the `ty` for a unit variant is `()`,
                    // which has a correct implementation of Parameter for our
                    // needs
                    quote! { #ident { #fields_ident: ( #ty, ), } }
                }
                VariantFieldSourcesMode::Struct(ref sources) => {
                    let field_types = sources.iter().map(|field| &field.field.info.ty);

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

    let long_arg_superposition = handle_superposition_argument(
        &fields_ident,
        &viable_ident,
        &argument,
        quote! { #flag.bytes() },
        &parsed_flags,
        |tags| tags.long(),
        &variant_sources,
        any_multi_flags,
        &arg_ident,
        &add_arg_ident,
        quote! {()},
    );

    let long_superposition = handle_superposition_argument(
        &fields_ident,
        &viable_ident,
        &argument,
        quote! { #flag.bytes() },
        &parsed_flags,
        |tags| tags.long(),
        &variant_sources,
        any_multi_flags,
        &present_ident,
        &add_present_ident,
        &argument,
    );

    let short_superposition = handle_superposition_argument(
        &fields_ident,
        &viable_ident,
        &argument,
        &flag,
        &parsed_flags,
        |tags| tags.short(),
        &variant_sources,
        any_multi_flags,
        &present_ident,
        &add_present_ident,
        &argument,
    );

    macro_rules! make_arms {
        (
            builder: $builder:ident,
            scrutinees: $scrutinees:expr,
        ) => {
            variant_sources
                .iter()
                .filter(|(_, variant)| variant.reachable)
                .map(|(&ident, variant)| {
                    let body = match variant.mode {
                        VariantFieldSourcesMode::Plain(ref source) => $builder(
                            &fields_ident,
                            &argument,
                            &flag,
                            &[(ident, source.flag)],
                            ($scrutinees).iter().copied(),
                        ),
                        VariantFieldSourcesMode::Struct(ref sources) => $builder(
                            &fields_ident,
                            &argument,
                            &flag,
                            sources,
                            ($scrutinees).iter().copied(),
                        ),
                    };

                    quote! {
                        #ident { ref mut #fields_ident } => #body
                    }
                })
        };
    }

    let add_long_arg_arms = make_arms!(
        builder: complete_long_arg_body,
        scrutinees: &all_long_scrutinees,
    );

    let add_long_arms = make_arms!(
        builder: complete_long_body,
        scrutinees: &all_long_scrutinees,
    );

    let add_short_arms = make_arms!(
        builder: complete_short_body,
        scrutinees: &all_short_scrutinees,
    );

    let local_value_ident = Ident::new("value", Span::mixed_site());
    let build_from_superposition = {
        let fields_reinits = indexed(&parsed_flags).map(|(index, flag)| {
            quote! { ::core::result::Result::Ok(#fields_ident.#index), }
        });

        let variant_attempts =
            variant_sources
                .iter()
                .enumerate()
                .map(|(index, (ident, variant))| {
                    let bit = variant_bit(index);

                    // Step 2a: for each field in this variant, attempt to instantiate
                    // it. Then, if they all really do exist, return success. Use
                    // clever juggling of mutable references to allow our `if let`
                    // binding to work withount unconditionally moving anything.

                    quote! {
                        'block : {
                            if (#viable_ident & #bit) != 0 {
                                // Instantiate stuff that doesn't live in #fields_ident.
                                // Each instantiated local will `break` if it encounters
                                // an error. At the end of this we'll have a tuples
                                // of high-quality `T` values.
                                let #local_value_ident = ( #(#instantiate_locals, )* );

                                // Instantiate stuff that DOES live in #fields_ident.
                                // this won't create a new variable, but it will
                                // still `break` if any errors are encountered. After
                                // this chunk, all the values are guaranteed to be
                                // present
                                #(#instantiate_globals)*

                                break ::core::result::Result::Ok(
                                    Self :: #ident { ...inits }
                                )
                            }
                        }
                    }
                });

        quote! {
            {
                // Variant instantiator functions. Each one of these attempts
                // to construct

                // Step 1: replace each field with a Result<Option<T>, RequiredError> or
                // Result<(), RequiredError>. This will allow us to lazily compute
                // the errors for absent fields.
                let #fields_ident = ( #(#fields_reinits,)* );

                // Step 2: for each viable variant: instantiate all of the fields
                // it needs, then attempt to match them all.

                // Step 3: if we haven't returned yet, all of the variants
                // we tried to instantiate were missing required fields. Emit
                // a required field missing error with, ideally, information
                // about all the fields.
            }
        }
    };

    let build_from_variant_arms = variant_sources
        .iter()
        .filter(|(_, variant)| variant.reachable)
        .map(|(ident, variant)| {
            let body = match variant.mode {
                VariantFieldSourcesMode::Plain(ref source) => match source.field.info {
                    PlainFlagInfo::Unit => quote! {},
                    PlainFlagInfo::Newtype(_) => quote! { (#fields_ident.0) },
                    PlainFlagInfo::Struct(field) => {
                        let ident = &field.ident;
                        quote! { { #ident : #fields_ident.0 } }
                    }
                },
                VariantFieldSourcesMode::Struct(ref sources) => {
                    let field_inits = indexed(sources).map(|(index, source)| {
                        let tags = source.field.tags.simplify();
                        struct_field_initializer(
                            &fields_ident,
                            index,
                            NormalFieldInfo {
                                long: tags.long(),
                                short: tags.short(),
                                placeholder: &source.field.placeholder,
                                ident: &source.field.info.ident,
                                default: source.field.default.as_ref(),
                            },
                        )
                    });

                    quote! { { #( #field_inits, )* } }
                }
            };

            quote! {
                Self :: State :: #ident { #fields_ident } => Self :: #ident #body
            }
        });

    Ok(quote! {
        enum #state_ident <#lifetime> {
            // In the generic state, we haven't selected a specific variant yet
            #superposition_ident {
                #fields_ident: ( #(#flag_types,)* ),
                #viable_ident: #viable_set_type,
                phantom: ::core::marker::PhantomData<& #lifetime ()>,
            },

            #(#state_variants,)*
        }

        impl ::core::default::Default for #state_ident <'_> {
            fn default() -> Self {
                Self :: #superposition_ident {
                    #fields_ident: ( #(#flag_inits,)* ),
                    #viable_ident: #viable_set_init,
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
                match *self {
                    Self :: #superposition_ident {ref mut #fields_ident, ref mut #viable_ident, .. } => #long_arg_superposition,
                    #(Self :: #add_long_arg_arms,)*
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
                    Self :: #superposition_ident {ref mut #fields_ident, ref mut #viable_ident, .. } => #long_superposition,
                    #(Self :: #add_long_arms,)*
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
                    Self :: #superposition_ident {ref mut #fields_ident, ref mut #viable_ident, .. } => #short_superposition,
                    #(Self :: #add_short_arms,)*
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
                            Self :: State :: #superposition_ident { #viable_ident, #fields_ident } => #build_from_superposition,
                            #(#build_from_variant_arms,)*
                        }
                    )
                }
        }
    })
}
