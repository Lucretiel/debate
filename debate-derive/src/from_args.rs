use std::collections::{HashMap, hash_map::Entry};
use std::fmt::Display;
use std::hash::Hash;

use darling::{
    FromAttributes,
    util::{Override, SpannedValue},
};
use heck::ToKebabCase as _;
use itertools::Itertools as _;
use lazy_format::lazy_format;
use proc_macro2::{Literal, Span, TokenStream as TokenStream2};
use quote::{ToTokens, format_ident, quote};
use syn::{
    Attribute, DataEnum, DeriveInput, Expr, Field, Fields, Generics, Ident, Token, Type,
    punctuated::Punctuated, spanned::Spanned,
};

#[derive(darling::FromAttributes, Debug)]
#[darling(attributes(debate))]
struct RawParsedAttr {
    long: Option<Override<SpannedValue<String>>>,
    short: Option<Override<SpannedValue<char>>>,
    default: Option<Override<Expr>>,
    // clear = "no-verbose"
    // placeholder = "VALUE"
    // TODO: add a parse step where `flatten` must not coexist with the other
    // variants. Consider switching from `darling` to `deluxe`, which apparently
    // handles this.
    flatten: Option<()>,
}

enum FieldDefault {
    None,
    Trait,
    Expr(Expr),
}

impl FieldDefault {
    pub fn new(default: Option<Override<Expr>>) -> Self {
        match default {
            Some(Override::Explicit(default)) => Self::Expr(default),
            Some(Override::Inherit) => Self::Trait,
            None => Self::None,
        }
    }
}

struct BasicFieldInfo<'a> {
    ident: &'a Ident,
    ident_str: String,
    ty: &'a Type,
}

struct PositionalFieldInfo<'a> {
    basics: BasicFieldInfo<'a>,
    default: FieldDefault,
    docs: String,
}

struct OptionFieldInfo<'a> {
    basics: BasicFieldInfo<'a>,
    default: FieldDefault,
    docs: String,
    tags: OptionTag,
}

struct FlattenFieldInfo<'a> {
    basics: BasicFieldInfo<'a>,
}

enum ParsedFieldInfo<'a> {
    Positional(PositionalFieldInfo<'a>),
    Option(OptionFieldInfo<'a>),
    Flatten(FlattenFieldInfo<'a>),
}

impl<'a> ParsedFieldInfo<'a> {
    pub fn from_field(field: &'a Field) -> syn::Result<Self> {
        let ident = field
            .ident
            .as_ref()
            .expect("already checked that this isn't a tuple struct");

        let basics = BasicFieldInfo {
            ident,
            ident_str: ident.to_string(),
            ty: &field.ty,
        };

        let parsed = RawParsedAttr::from_attributes(&field.attrs)?;

        // TODO: enforce that flatten doesn't coexist with other variants.
        if let Some(()) = parsed.flatten {
            return Ok(Self::Flatten(FlattenFieldInfo { basics }));
        }

        let long = parsed
            .long
            .map(|long| compute_long(long.explicit(), ident))
            .transpose()?;

        let short = parsed
            .short
            .map(|short| compute_short(short.explicit(), ident))
            .transpose()?;

        let default = FieldDefault::new(parsed.default);

        Ok(
            match match (long, short) {
                (None, None) => None,
                (Some(long), None) => Some(OptionTag::Long(long)),
                (None, Some(short)) => Some(OptionTag::Short(short)),
                (Some(long), Some(short)) => Some(OptionTag::LongShort(long, short)),
            } {
                None => Self::Positional(PositionalFieldInfo {
                    basics,
                    default,
                    docs: String::new(),
                }),
                Some(tags) => Self::Option(OptionFieldInfo {
                    basics,
                    default,
                    docs: String::new(),
                    tags,
                }),
            },
        )
    }

    pub fn get_positional(&self) -> Option<FlattenOr<&FlattenFieldInfo, &PositionalFieldInfo>> {
        match self {
            ParsedFieldInfo::Positional(info) => Some(FlattenOr::Normal(info)),
            ParsedFieldInfo::Flatten(info) => Some(FlattenOr::Flatten(info)),
            ParsedFieldInfo::Option(_) => None,
        }
    }
}

enum FlattenOr<F, T> {
    Flatten(F),
    Normal(T),
}

fn compute_long(
    long: Option<SpannedValue<String>>,
    field_name: &Ident,
) -> syn::Result<SpannedValue<String>> {
    // Currently this can never fail, but we want to leave open the possibility
    let long = long.unwrap_or_else(|| {
        SpannedValue::new(field_name.to_string().to_kebab_case(), field_name.span())
    });

    if long.starts_with("--") {
        Err(syn::Error::new(
            long.span(),
            "long parameters don't need to start with --; this is handled automatically",
        ))
    } else if long.starts_with('-') {
        Err(syn::Error::new(
            long.span(),
            "long parameters don't start with '-'",
        ))
    } else if !long.starts_with(|c: char| c.is_alphabetic()) {
        Err(syn::Error::new(
            long.span(),
            "long parameters should start with something alphabetic. This might be relaxed later.",
        ))
    } else if long.contains('=') {
        Err(syn::Error::new(
            long.span(),
            "long parameters must not include an '=', as it is the argument separator",
        ))
    } else {
        Ok(long)
    }
}

fn compute_short(
    short: Option<SpannedValue<char>>,
    field_name: &Ident,
) -> syn::Result<SpannedValue<char>> {
    let c = short.unwrap_or_else(|| {
        SpannedValue::new(
            field_name
                .to_string()
                .chars()
                .next()
                .expect("Identifiers can't be empty"),
            field_name.span(),
        )
    });

    if *c == '-' {
        Err(syn::Error::new(c.span(), "short parameter must not be '-'"))
    } else if !c.is_ascii_graphic() {
        Err(syn::Error::new(
            c.span(),
            "short parameter should be an ascii printable",
        ))
    } else {
        Ok(c)
    }
}

enum OptionTag {
    Long(SpannedValue<String>),
    Short(SpannedValue<char>),
    LongShort(SpannedValue<String>, SpannedValue<char>),
}

impl OptionTag {
    pub fn long(&self) -> Option<SpannedValue<&str>> {
        match *self {
            OptionTag::Long(ref long) | OptionTag::LongShort(ref long, _) => {
                Some(SpannedValue::new(long.as_str(), long.span()))
            }
            OptionTag::Short(_) => None,
        }
    }

    pub fn short(&self) -> Option<SpannedValue<char>> {
        match *self {
            OptionTag::Short(short) | OptionTag::LongShort(_, short) => Some(short),
            OptionTag::Long(_) => None,
        }
    }
}

fn apply_arg_to_field(
    field_ident: &Ident,
    initial_method: &Ident,
    follow_up_method: &Ident,
) -> impl ToTokens {
    quote! {
        match self.fields.#field_ident {
            ::core::option::Option::None => match ::debate::parameter::Parameter::#initial_method(argument) {
                ::core::result::Result::Err(err) => ::core::result::Result::Err(err),
                ::core::result::Result::Ok(value) => {
                    self.fields.#field_ident = ::core::option::Option::Some(value);
                    ::core::result::Result::Ok(())
                }
            }
            ::core::option::Option::Some(ref mut old) => ::debate::parameter::Parameter::#follow_up_method(
                old,
                argument
            ),
        }
    }
}

/// Create match arms for long_option, long, and short
fn create_local_option_arms<'a>(
    fields: impl IntoIterator<Item = &'a OptionFieldInfo<'a>>,
    make_scrutinee: impl Fn(&'a OptionFieldInfo<'a>) -> Option<Literal>,
    initial_method: &Ident,
    follow_up_method: &Ident,
) -> impl Iterator<Item = impl ToTokens> {
    fields
        .into_iter()
        .filter_map(move |field| make_scrutinee(field).map(|scrutinee| (field, scrutinee)))
        .map(move |(info, scrutinee)| {
            let field_ident = info.basics.ident;
            let field_name: &str = &info.basics.ident_str;

            let expr = apply_arg_to_field(field_ident, initial_method, follow_up_method);

            quote! {
                #scrutinee => match (#expr) {
                    ::core::result::Result::Ok(()) => ::core::result::Result::Ok(()),
                    ::core::result::Result::Err(err) => ::core::result::Result::Err(
                        ::debate::state::Error::parameter(#field_name, err)
                    ),
                },
            }
        })
}

enum FlattenMode {
    Positional,
    Option,
}

fn handle_flatten(
    field_ident: &Ident,
    method: &Ident,
    mode: FlattenMode,
    unrecognized_bind: impl ToTokens,
    unrecognized: impl ToTokens,
) -> impl ToTokens {
    let expr = match mode {
        FlattenMode::Positional => quote! {
            ::debate::state::State::#method(
                &mut self.fields.#field_ident,
                argument,
            )
        },
        FlattenMode::Option => quote! {
            ::debate::state::State::#method(
                &mut self.fields.#field_ident,
                option,
                argument,
            )
        },
    };

    quote! {
        match #expr {
            ::core::result::Result::Ok(()) => return ::core::result::Result::Ok(()),
            ::core::result::Result::Err(err) => match err {
                ::debate::util::DetectUnrecognized::Unrecognized(#unrecognized_bind) => #unrecognized,
                ::debate::util::DetectUnrecognized::Error(err) => {
                    return ::core::result::Result::Err(err);
                }
            }
        }
    }
}

fn detect_collision<T: Hash + Eq + Copy, M: Display>(
    known_tags: &mut HashMap<T, Span>,
    new_tag: Option<SpannedValue<T>>,
    message: impl Fn(T) -> M,
) -> syn::Result<()> {
    match new_tag {
        Some(tag) => match known_tags.entry(*tag) {
            Entry::Occupied(entry) => {
                let mut err1 = syn::Error::new(tag.span(), message(*tag));
                let err2 = syn::Error::new(*entry.get(), "original use here");

                err1.combine(err2);
                Err(err1)
            }
            Entry::Vacant(entry) => {
                entry.insert(tag.span());
                Ok(())
            }
        },
        None => Ok(()),
    }
}

fn derive_args_struct(
    name: &Ident,
    fields: &Punctuated<Field, Token![,]>,
    generics: &Generics,
    attrs: &[Attribute],
) -> syn::Result<TokenStream2> {
    let fields: Vec<ParsedFieldInfo> = fields
        .iter()
        .map(ParsedFieldInfo::from_field)
        .try_collect()?;

    // Collision detection
    {
        let mut long_tags = HashMap::new();
        let mut short_tags = HashMap::new();

        for tags in fields.iter().filter_map(|field| match field {
            ParsedFieldInfo::Option(option) => Some(&option.tags),
            ParsedFieldInfo::Positional(_) | ParsedFieldInfo::Flatten(_) => None,
        }) {
            detect_collision(&mut long_tags, tags.long(), |tag| {
                lazy_format!("duplicate option --{tag}")
            })?;
            detect_collision(&mut short_tags, tags.short(), |tag| {
                lazy_format!("duplicate option -{tag}")
            })?;
        }
    }

    let field_state_contents = fields.iter().map(|info| match info {
        // For both positional and optional parameters, emit an option
        // containing the value
        ParsedFieldInfo::Positional(PositionalFieldInfo {
            basics: BasicFieldInfo { ident, ty, .. },
            ..
        })
        | ParsedFieldInfo::Option(OptionFieldInfo {
            basics: BasicFieldInfo { ident, ty, .. },
            ..
        }) => quote! { #ident : ::core::option::Option<#ty>, },

        // For flattened fields, emit the state
        ParsedFieldInfo::Flatten(FlattenFieldInfo {
            basics: BasicFieldInfo { ident, ty, .. },
        }) => quote! {
            #ident: <#ty as ::debate::from_args::BuildFromArgs<'arg>>::State,
        },
    });

    // Reuse these everywhere
    let arg_ident = format_ident!("arg");
    let present_ident = format_ident!("present");
    let add_arg_ident = format_ident!("add_arg");
    let add_present_ident = format_ident!("add_present");

    let add_positional_ident = format_ident!("add_positional");
    let add_long_option_ident = format_ident!("add_long_option");
    let add_long_ident = format_ident!("add_long");
    let add_short_ident = format_ident!("add_short");

    let argument_ident = format_ident!("argument");

    let visit_positional_arms = fields
        .iter()
        .filter_map(|info| info.get_positional())
        .enumerate()
        .map(|(idx, field)| (Literal::usize_unsuffixed(idx), field))
        .map(|(idx, info)| match info {
            // This is a regular positional field
            FlattenOr::Normal(info) => {
                let field_ident = info.basics.ident;
                let field_str = info.basics.ident_str.as_str();
                let apply_argument = apply_arg_to_field(&field_ident, &arg_ident, &add_arg_ident);

                // TODO: adapt handle_flatten so that it can be used here
                // NOTE: in principle we don't need a position, we could just
                // repeatedly apply each positional arg to each positional
                // field in a waterfall. We're making a gesticulation towards
                // subquadratic performance, though, espeically if the compiler
                // can figure out how to make this into a jump table.
                // TODO: consider `loop { match { } }` instead of a waterfall
                // of `if`
                quote! {
                    if self.position == #idx {
                        self.position = match (#apply_argument) {
                            ::core::result::Result::Ok(()) => return ::core::result::Result::Ok(()),
                            ::core::result::Result::Err(err) => match err {
                                ::debate::util::DetectUnrecognized::Unrecognized(()) => {
                                    #idx + 1
                                },
                                ::debate::util::DetectUnrecognized::Error(err) => return (
                                    ::core::result::Result::Err(
                                        ::debate::state::Error::parameter(
                                            #field_str,
                                            err
                                        )
                                    )
                                ),
                            }
                        }
                    }
                }
            }
            // This is a flattened field
            FlattenOr::Flatten(info) => {
                let body = handle_flatten(
                    info.basics.ident,
                    &add_positional_ident,
                    FlattenMode::Positional,
                    quote! { () },
                    quote! { #idx + 1 },
                );

                quote! {
                    if self.position == #idx {
                        self.position = #body;
                    }
                }
            }
        });

    let option_fields = fields.iter().filter_map(|info| match info {
        ParsedFieldInfo::Option(info) => Some(info),
        ParsedFieldInfo::Positional(_) | ParsedFieldInfo::Flatten(_) => None,
    });

    let flatten_fields = fields.iter().filter_map(|info| match info {
        ParsedFieldInfo::Flatten(info) => Some(info),
        ParsedFieldInfo::Option(_) | ParsedFieldInfo::Positional(_) => None,
    });

    let visit_long_option_arms = create_local_option_arms(
        option_fields.clone(),
        |info| {
            info.tags
                .long()
                .map(|long| Literal::byte_string(long.as_bytes()))
        },
        &arg_ident,
        &add_arg_ident,
    );

    let visit_long_option_flattened_arms = flatten_fields.clone().map(|info| {
        handle_flatten(
            info.basics.ident,
            &add_long_option_ident,
            FlattenMode::Option,
            quote! { () },
            quote! { {} },
        )
    });

    let visit_long_arms = create_local_option_arms(
        option_fields.clone(),
        |info| {
            info.tags
                .long()
                .map(|long| Literal::byte_string(long.as_bytes()))
        },
        &present_ident,
        &add_present_ident,
    );

    let visit_long_flattened_arms = flatten_fields.clone().map(|info| {
        let body = handle_flatten(
            info.basics.ident,
            &add_long_ident,
            FlattenMode::Option,
            &argument_ident,
            &argument_ident,
        );

        quote! {
            let #argument_ident = #body;
        }
    });

    let visit_short_arms = create_local_option_arms(
        option_fields.clone(),
        |info| {
            info.tags
                .short()
                .map(|short| Literal::byte_character(*short as u8))
        },
        &present_ident,
        &add_present_ident,
    );

    let visit_short_flattened_arms = flatten_fields.clone().map(|info| {
        let body = handle_flatten(
            info.basics.ident,
            &add_short_ident,
            FlattenMode::Option,
            &argument_ident,
            &argument_ident,
        );

        quote! {
            let #argument_ident = #body;
        }
    });

    let final_field_initializers = fields
        .iter()
        .map(|info| match info {
            ParsedFieldInfo::Positional(info) => {
                FlattenOr::Normal((None, None, &info.basics, &info.default))
            }
            ParsedFieldInfo::Option(info) => FlattenOr::Normal((
                info.tags.long(),
                info.tags.short(),
                &info.basics,
                &info.default,
            )),
            ParsedFieldInfo::Flatten(flatten_field_info) => FlattenOr::Flatten(flatten_field_info),
        })
        .map(|field| match field {
            FlattenOr::Normal((long, short, info, default)) => {
                let field_ident = info.ident;
                let field_ident_str = field_ident.to_string();

                let long = match long.as_deref() {
                    Some(&long) => quote! {::core::option::Option::Some(#long)},
                    None => quote! {::core::option::Option::None},
                };

                let short = match short.as_deref() {
                    Some(&short) => quote! {::core::option::Option::Some(#short)},
                    None => quote! {::core::option::Option::None},
                };

                let default = match default {
                    FieldDefault::Expr(expr) => quote! { #expr },
                    FieldDefault::Trait => quote! { ::core::default::Default::default() },
                    FieldDefault::None => quote! { match ::debate::parameter::Parameter::absent() {
                        ::core::result::Result::Ok(value) => value,
                        ::core::result::Result::Err(::debate::parameter::RequiredError) =>
                            return ::core::result::Result::Err(
                                ::debate::from_args::Error::required(
                                    #field_ident_str, #long, #short
                                )
                            ),
                    }},
                };

                quote! {
                    #field_ident: match state.fields.#field_ident {
                        ::core::option::Option::Some(value) => value,
                        ::core::option::Option::None => #default,
                    },
                }
            }
            FlattenOr::Flatten(info) => {
                let field_ident = info.basics.ident;

                quote! {
                    #field_ident: match ::debate::from_args::BuildFromArgs::build(
                        state.fields.#field_ident
                    ) {
                        ::core::result::Result::Ok(value) => value,
                        ::core::result::Result::Err(err) => return ::core::result::Result::Err(err),
                    },
                }
            }
        });

    let state_ident = format_ident!("__{name}State");
    let field_state_ident = format_ident!("__{name}FieldState");

    Ok(quote! {
        // TODO: instead of polluting the namespace with this pair of types,
        // consider using a tuple? Need to find a good way to map field
        // identifiers to it.
        #[derive(::core::default::Default)]
        #[doc(hidden)]
        struct #field_state_ident<'arg> {
            #(#field_state_contents)*

            // TODO: find a way to create a safe name for the phantom
            phantom: ::core::marker::PhantomData<&'arg ()>,
        }

        #[doc(hidden)]
        #[derive(::core::default::Default)]
        struct #state_ident<'arg> {
            fields: #field_state_ident<'arg>,
            position: u16,
            help: bool,
        }

        impl<'arg> ::debate::state::State<'arg> for #state_ident<'arg> {
            fn add_positional<E>(
                &mut self,
                argument: ::debate_parser::Arg<'arg>
            ) -> ::core::result::Result<(), E>
            where
                E: ::debate::state::Error<'arg, ()>
            {
                #(#visit_positional_arms)*

                ::core::result::Result::Err(
                    ::debate::state::Error::unrecognized(())
                )
            }

            fn add_long_option<E>(
                &mut self,
                option: ::debate_parser::Arg<'arg>,
                argument: ::debate_parser::Arg<'arg>
            ) -> ::core::result::Result<(), E>
            where
                E: ::debate::state::Error<'arg, ()>
            {
                match option.bytes() {
                    #(#visit_long_option_arms)*
                    _ => {
                        #(#visit_long_option_flattened_arms)*

                        ::core::result::Result::Err(
                            ::debate::state::Error::unrecognized(())
                        )
                    }
                }
            }

            fn add_long<A, E>(
                &mut self,
                option: ::debate_parser::Arg<'arg>,
                argument: A
            ) -> ::core::result::Result<(), E>
            where
                A: ::debate_parser::ArgAccess<'arg>,
                E: ::debate::state::Error<'arg, A>
            {
                match option.bytes() {
                    #(#visit_long_arms)*
                    _ => {
                        #(#visit_long_flattened_arms)*

                        ::core::result::Result::Err(
                            ::debate::state::Error::unrecognized(
                                argument
                            )
                        )
                    }
                }
            }

            fn add_short<A, E>(
                &mut self,
                option: u8,
                argument: A
            ) -> ::core::result::Result<(), E>
            where
                A: ::debate_parser::ArgAccess<'arg>,
                E: ::debate::state::Error<'arg, A>
            {
                match option {
                    #(#visit_short_arms)*
                    _ => {
                        #(#visit_short_flattened_arms)*

                        ::core::result::Result::Err(
                            ::debate::state::Error::unrecognized(
                                argument
                            )
                        )
                    }
                }
            }
        }

        impl<'arg> ::debate::from_args::BuildFromArgs<'arg> for #name {
            type State = #state_ident<'arg>;

            fn build<E>(state: Self::State) -> ::core::result::Result<Self, E>
            where
                E: ::debate::from_args::Error<'arg>
            {
                ::core::result::Result::Ok(Self {
                    #(#final_field_initializers)*
                })
            }
        }
    })
}

fn derive_args_enum(
    name: &Ident,
    data: &DataEnum,
    generics: &Generics,
    attrs: &[Attribute],
) -> syn::Result<TokenStream2> {
    todo!()
}

pub fn derive_args_result(item: TokenStream2) -> syn::Result<TokenStream2> {
    let input: DeriveInput = syn::parse2(item)?;

    let no_derive = |msg| Err(syn::Error::new(input.span(), msg));

    match input.data {
        syn::Data::Struct(ref data) => derive_args_struct(
            &input.ident,
            match data.fields {
                Fields::Named(ref fields) => &fields.named,
                Fields::Unnamed(_) => {
                    return no_derive("Can't derive `FromArgs` on a tuple struct");
                }
                Fields::Unit => return no_derive("can't derive `FromArgs` on a unit struct"),
            },
            &input.generics,
            &input.attrs,
        ),
        syn::Data::Enum(ref data) => {
            derive_args_enum(&input.ident, data, &input.generics, &input.attrs)
        }
        syn::Data::Union(_) => no_derive("can't derive `FromArgs` on a union"),
    }
}
