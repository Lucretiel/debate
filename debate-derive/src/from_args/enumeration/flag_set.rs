use indexmap::IndexMap;
use itertools::Itertools;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{ToTokens, format_ident, quote};
use syn::{Ident, Index, Lifetime, Token, Variant, punctuated::Punctuated, spanned::Spanned};

use crate::{
    common::{
        FlagTags, IdentString,
        enumeration::flag_set::{
            FlagFieldInfo, FlagSetFlag, FlagSetFlagInfo, ParsedFlagSetInfo, PlainFlagInfo,
            VariantFieldSource, VariantFieldSources, VariantFieldSourcesMode,
            compute_grouped_flags,
        },
    },
    from_args::common::{
        FieldNature, FlagField, MakeScrutinee, NormalFieldInfo, absent_field_initializer,
        apply_arg_to_field, complete_flag_body, indexed, quoted_tags, struct_field_initializer,
    },
    generics::AngleBracedLifetime,
};

use super::ValueEnumAttr;

fn variant_bit(i: usize) -> TokenStream2 {
    quote! { const { 1u64 << #i } }
}

#[expect(clippy::too_many_arguments)]
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
                                if index_in_variant == destination.field_index {
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

            let expr = quote! {
                match (#expr) {
                    ::core::result::Result::Ok(()) => ::core::result::Result::Ok(()),
                    ::core::result::Result::Err(err) => ::core::result::Result::Err(
                        ::debate::state::Error::parameter(#name, err)
                    ),
                }
            };

            let expr = match any_multi_flags {
                false => expr,
                true => {
                    let viable_bits = flag
                        .variants
                        .iter()
                        .map(|viable_variant| viable_variant.index)
                        .map(variant_bit);

                    let update_viability = match flag.unique_variant() {
                        Some(_) => quote! {},
                        None => quote! { *#viable_ident = updated_viability; }
                    };

                    quote! {
                        match *#viable_ident & const { 0 #( | #viable_bits )* } {
                            0 => ::core::result::Result::Err(
                                Self::create_superposition_conflict_error(&*#fields_ident)
                            ),
                            updated_viability => match (#expr) {
                                ::core::result::Result::Err(err) => ::core::result::Result::Err(err),
                                ::core::result::Result::Ok(()) => {
                                    #update_viability
                                    ::core::result::Result::Ok(())
                                }
                            }
                        }
                    }
                }
            };

            quote! { #scrutinee => #expr }
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

// We use slices everywhere here because we call `indexed` on them, and we
// want to enforce (more or less) that we're using the complete set, from
// a slice, rather than a filtered set from an iterator.
#[expect(clippy::too_many_arguments)]
fn complete_flagset_flag_body<'a, Tag, T>(
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
    T: BasicFlagProperties,
    Tag: MakeScrutinee,
{
    let conflict = {
        let mut tags = all_tag_scrutinees
            .into_iter()
            .map(|tag| tag.make_scrutinee());

        let conflicting_flag_set = match fields {
            // If there's only one field, there's no option, so we can just
            // directly report the FlagSet
            [field] => {
                let tags = field.tags();
                let tags = quoted_tags(&tags);
                let placeholder = field.placeholder();

                quote! {
                    ::debate::errors::FlagsList::new(#tags, #placeholder)
                }
            }
            fields => conflict_flagset_builder_expression(
                fields_ident,
                indexed(fields),
                quote! { ::core::option::Option::None },
                quote! { ::core::option::Option::Some(_) },
            ),
        };

        match tags.next() {
            None => quote! {},
            Some(recognize) => quote! {
                #recognize #(| #tags)* => ::core::result::Result::Err(
                    ::debate::state::Error::conflicts_with_flags(
                        #conflicting_flag_set
                    )
                ),
            },
        }
    };

    complete_flag_body(
        fields_ident,
        argument_ident,
        option_expr,
        None,
        indexed(fields),
        get_tag,
        parameter_method,
        add_parameter_method,
        conflict,
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
    T: BasicFlagProperties,
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
    T: BasicFlagProperties,
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
    T: BasicFlagProperties,
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

trait BasicFlagProperties {
    fn tags(&self) -> FlagTags<&str, char>;
    fn placeholder(&self) -> &str;
}

impl BasicFlagProperties for FlagSetFlag<'_> {
    fn tags(&self) -> FlagTags<&str, char> {
        self.tags
    }

    fn placeholder(&self) -> &str {
        self.placeholder
    }
}

impl<T> BasicFlagProperties for VariantFieldSource<'_, T> {
    fn tags(&self) -> FlagTags<&str, char> {
        self.field.tags.simplify()
    }

    fn placeholder(&self) -> &str {
        &self.field.placeholder
    }
}

impl BasicFlagProperties for (&IdentString<'_>, &FlagSetFlagInfo<PlainFlagInfo<'_>>) {
    fn tags(&self) -> FlagTags<&str, char> {
        self.1.tags.simplify()
    }

    fn placeholder(&self) -> &str {
        &self.1.placeholder
    }
}

fn conflict_flagset_builder_expression<'a, T>(
    tuple: &Ident,
    fields: impl IntoIterator<Item = (Index, &'a T)>,
    skip: TokenStream2,
    keep: TokenStream2,
) -> TokenStream2
where
    T: BasicFlagProperties + 'a,
{
    let inits = fields.into_iter().map(|(index, flag)| {
        let tags = quoted_tags(&flag.tags());
        let placeholder = flag.placeholder();

        quote! {
            match #tuple.#index {
                #skip => ::core::option::Option::None,
                #keep => ::core::option::Option::Some((
                    #tags, #placeholder,
                ))
            }
        }
    });

    quote! {{
        let flags = [ #(#inits,)* ];
        let flags = &flags;
        let flags = ::core::iter::IntoIterator::into_iter(flags);
        let mut flags = ::core::iter::Iterator::filter_map(
            flags,
            ::core::option::Option::as_ref
        );

        let &(tags, placeholder) = ::core::iter::Iterator::next(&mut flags)
            .expect("there must be something to conflict with");

        let mut set = ::debate::errors::FlagsList::new(tags, placeholder);

        for &(tags, placeholder) in flags {
            ::debate::errors::FlagsList::add(&mut set, tags, placeholder);
        }

        set
    }}
}

fn variant_field_superposition_initializer<T>(
    fields_ident: &Ident,
    requirements_ident: &Ident,
    source: &VariantFieldSource<'_, T>,
) -> TokenStream2 {
    let flag_index = Index::from(source.index);
    let default = absent_field_initializer(source.field.default.as_ref(), || {
        quote! {{
            #requirements_ident.#flag_index = ::core::result::Result::Err(
                ::debate::parameter::RequiredError
            );

            break 'block;
        }}
    });

    match source.unique {
        // If the source is unique, and we're in a superposition, we KNOW that
        // the flag wasn't present on the command line (because it would have
        // taken us out of a superposition), so we unconditionally use
        // `absent_field_initializer`
        true => default,
        false => {
            quote! {
                match #fields_ident.#flag_index {
                    ::core::option::Option::Some(_) => ::core::option::Option::None,
                    ::core::option::Option::None => ::core::option::Option::Some(#default),
                }
            }
        }
    }
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

    let flag_types_tuple = quote! { ( #(#flag_types,)* ) };

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

    let superposition_conflict_set = conflict_flagset_builder_expression(
        &fields_ident,
        indexed(&parsed_flags).filter(|(_, flag)| flag.unique_variant().is_none()),
        quote! { ::core::option::Option::None },
        quote! { ::core::option::Option::Some(_) },
    );

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
                            &[(ident, source.field)],
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

    let local_requirements = Ident::new("requirements", Span::mixed_site());
    let local_fields_ident = Ident::new("local_fields", Span::mixed_site());

    let build_from_superposition = {
        let requirement_tracking_inits = parsed_flags.iter().map(|flag| match flag.ever_required {
            true => {
                quote! { ::core::result::Result::<(), ::debate::parameter::RequiredError>::Ok(()) }
            }
            false => quote! { ::core::result::Result::<(), ::core::convert::Infallible>::Ok(()) },
        });

        let variant_attempts =
            variant_sources
                .iter()
                .enumerate()
                .map(|(variant_index, (ident, variant))| {
                    let bit = variant_bit(variant_index);

                    let local_field_inits = match variant.mode {
                        VariantFieldSourcesMode::Plain(ref source) => {
                            variant_field_superposition_initializer(
                                &fields_ident,
                                &local_requirements,
                                source,
                            )
                        }
                        VariantFieldSourcesMode::Struct(ref sources) => {
                            let inits = sources.iter().map(|source| {
                                variant_field_superposition_initializer(
                                    &fields_ident,
                                    &local_requirements,
                                    source,
                                )
                            });

                            quote! { ( #(#inits,)* ) }
                        }
                    };

                    let final_init = match variant.mode {
                        VariantFieldSourcesMode::Plain(ref source) => {
                            let index = Index::from(source.index);

                            let init = match source.unique {
                                true => quote! { #local_fields_ident },
                                false => quote! {
                                    match #local_fields_ident {
                                        None => #fields_ident.#index.unwrap(),
                                        Some(value) => value,
                                    }
                                },
                            };

                            match source.field.info {
                                PlainFlagInfo::Unit => {
                                    quote! {
                                        {
                                            let () = #init;
                                            Self :: #ident
                                        }
                                    }
                                }
                                PlainFlagInfo::Newtype(_) => {
                                    quote! { Self :: #ident ( #init ) }
                                }
                                PlainFlagInfo::Struct(ref info) => {
                                    let field = &info.ident.raw();
                                    quote! { Self :: #ident { #field : #init } }
                                }
                            }
                        }
                        VariantFieldSourcesMode::Struct(ref sources) => {
                            let field_inits = indexed(sources).map(|(field_index, source)| {
                                let flag_index = Index::from(source.index);
                                let field = source.field.info.ident.raw();

                                let init = match source.unique {
                                    true => quote! { #local_fields_ident.#field_index },
                                    false => quote! {
                                        match #local_fields_ident.#field_index {
                                            None => #fields_ident.#flag_index.unwrap(),
                                            Some(value) => value,
                                        }
                                    },
                                };

                                quote! {
                                    #field : #init
                                }
                            });

                            quote! {
                                Self :: #ident { #(#field_inits,)* }
                            }
                        }
                    };

                    let viability_check = match any_multi_flags {
                        true => quote! { ((#viable_ident & #bit) != 0) },
                        false => quote! { true },
                    };

                    quote! {
                        'block : {
                            if #viability_check {
                                let #local_fields_ident = #local_field_inits;

                                // At this point, we've guaranteed the presence
                                // of everything we need to create the variant,
                                // so we can unwrap freely.
                                return ::core::result::Result::Ok(
                                    #final_init
                                )
                            }
                        }
                    }
                });

        let absent_requirements_flagset = conflict_flagset_builder_expression(
            &local_requirements,
            indexed(&parsed_flags),
            quote! { ::core::result::Result::Ok(()) },
            quote! { ::core::result::Result::Err(_) },
        );

        quote! {
            {
                // Step 1: Create a tuple of results that will track which
                // flags we end up requiring
                let mut #local_requirements = ( #(#requirement_tracking_inits,)* );

                // Step 2: for each viable variant: attempt to construct that
                // variant, and perform an early return with that variant
                #(#variant_attempts)*

                // Step 3: if we haven't returned yet, all of the variants
                // we tried to instantiate were missing required fields. Emit
                // a required field missing error with, ideally, information
                // about all the fields.
                ::core::result::Result::Err(
                    ::debate::build::Error::any_required_flag(
                        #absent_requirements_flagset
                    )
                )
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
                    PlainFlagInfo::Struct(ref field) => {
                        let ident = &field.ident;
                        quote! { { #ident : #fields_ident.0 } }
                    }
                },
                VariantFieldSourcesMode::Struct(ref sources) => {
                    let field_inits = indexed(sources).map(|(index, source)| {
                        struct_field_initializer(
                            &fields_ident,
                            index,
                            NormalFieldInfo {
                                tags: Some(source.field.tags.simplify()),
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
                #state_ident :: #ident { #fields_ident } => ::core::result::Result::Ok(
                    Self :: #ident #body
                )
            }
        });

    Ok(quote! {
        enum #state_ident <#lifetime> {
            // In the generic state, we haven't selected a specific variant yet
            #superposition_ident {
                #fields_ident: #flag_types_tuple,
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

        impl<#lifetime> #state_ident <#lifetime> {
            fn create_superposition_conflict_error<E, A>(#fields_ident: &#flag_types_tuple) -> E
            where
                E: ::debate::state::Error<#lifetime, A>
            {
                ::debate::state::Error::conflicts_with_flags(#superposition_conflict_set)
            }
        }

        impl<#lifetime> ::debate::state::State<#lifetime> for #state_ident <#lifetime> {
            // Flag sets never accept positionals
            fn add_positional<E>(
                &mut self,
                _arg: & #lifetime ::debate::Arg
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
                #flag: & #lifetime ::debate::Arg,
                #argument: & #lifetime ::debate::Arg
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
                #flag: & #lifetime ::debate::Arg,
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
                match state {
                    #state_ident :: #superposition_ident { #viable_ident, #fields_ident, .. } => #build_from_superposition,
                    #(#build_from_variant_arms,)*
                }
            }
        }
    })
}
