use darling::{FromMeta, util::Override};
use heck::ToKebabCase;
use itertools::Itertools as _;

use proc_macro2::{Literal, Span, TokenStream as TokenStream2};
use quote::{ToTokens, format_ident, quote};
use syn::{
    Attribute, DataEnum, DeriveInput, Expr, Field, Fields, Generics, Ident, Token, Type,
    punctuated::Punctuated, spanned::Spanned,
};

#[derive(darling::FromMeta)]
struct RawParsedAttr {
    long: Option<Override<String>>,
    short: Option<Override<char>>,
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
        let debate_attr = field
            .attrs
            .iter()
            .filter(|attr| attr.path().is_ident("debate"))
            .at_most_one()
            .map_err(|mut err| {
                let extra = err.nth(1).unwrap();
                syn::Error::new(extra.span(), "extra `debate` attribute")
            })?;

        let ident = field
            .ident
            .as_ref()
            .expect("already checked that this isn't a tuple struct");

        let basics = BasicFieldInfo {
            ident,
            ident_str: ident.to_string(),
            ty: &field.ty,
        };

        Ok(if let Some(debate_attr) = debate_attr {
            let parsed = RawParsedAttr::from_meta(&debate_attr.meta)?;

            // TODO: enforce that flatten doesn't coexist with other variants.
            if let Some(()) = parsed.flatten {
                return Ok(Self::Flatten(FlattenFieldInfo { basics }));
            }

            let long = parsed
                .long
                .map(|long| compute_long(long.explicit(), ident, field.span()))
                .transpose()?;

            let short = parsed
                .short
                .map(|short| compute_short(short.explicit(), ident, field.span()))
                .transpose()?;

            let default = FieldDefault::new(parsed.default);

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
            }
        } else {
            Self::Positional(PositionalFieldInfo {
                basics,
                default: FieldDefault::None,
                docs: String::new(),
            })
        })
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

fn compute_long(user_long: Option<String>, field_name: &Ident, _span: Span) -> syn::Result<String> {
    // Currently this can never fail, but we want to leave open the possibility
    Ok(user_long.unwrap_or_else(|| field_name.to_string().to_kebab_case()))
}

fn compute_short(user_short: Option<char>, field_name: &Ident, span: Span) -> syn::Result<char> {
    let c = user_short.unwrap_or_else(|| {
        field_name
            .to_string()
            .chars()
            .next()
            .expect("Identifiers can't be empty")
    });

    match c.is_ascii_graphic() {
        true => Ok(c),
        false => Err(syn::Error::new(
            span,
            "short flags must be ascii printables",
        )),
    }
}

enum OptionTag {
    Long(String),
    Short(char),
    LongShort(String, char),
}

impl OptionTag {
    pub fn long(&self) -> Option<&str> {
        match *self {
            OptionTag::Long(ref long) => Some(long),
            OptionTag::LongShort(ref long, _) => Some(long),
            OptionTag::Short(_) => None,
        }
    }

    pub fn short(&self) -> Option<char> {
        match *self {
            OptionTag::Short(short) => Some(short),
            OptionTag::LongShort(_, short) => Some(short),
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
        match self.fields.#field_ident.take() {
            ::core::option::Option::None => ::debate::parameter::Parameter::#initial_method(argument),
            ::core::option::Option::Some(old) => ::debate::parameter::Parameter::#follow_up_method(old, argument),
        }
    }
}

fn try_parameter(expr: impl ToTokens, field_name: &str) -> impl ToTokens {
    quote! {
        match (#expr) {
            ::core::result::Result::Ok(value) => value,
            ::core::result::Result::Err(err) => return ::core::result::Result::Err(
                ::debate::from_args::StateError::parameter(#field_name, err)
            )
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
            let handle_argument = try_parameter(
                apply_arg_to_field(field_ident, initial_method, follow_up_method),
                &info.basics.ident_str,
            );

            quote! {
                #scrutinee => {
                    self.fields.#field_ident = ::core::option::Option::Some(
                        #handle_argument
                    );
                    ::core::result::Result::Ok(())
                }
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
            ::debate::from_args::State::#method(
                &mut self.fields.#field_ident,
                argument,
            )
        },
        FlattenMode::Option => quote! {
            ::debate::from_args::State::#method(
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
                let handle_argument = try_parameter(
                    quote! { ::debate::parameter::Parameter::arg(argument) },
                    info.basics.ident_str.as_str(),
                );

                quote! {
                    if self.position == #idx {
                        self.fields.#field_ident = ::core::option::Option::Some(#handle_argument);
                        self.position = #idx + 1;
                        return ::core::result::Result::Ok(());
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

    let terminal_positional_arm = match fields
        .iter()
        .filter_map(|info| info.get_positional())
        .next_back()
    {
        // There are no positional parameters at all, so this is an
        // unconditional error
        None => quote! {
            ::core::result::Result::Err(
                ::debate::from_args::StateError::unrecognized(())
            )
        },
        // This is a positional parameter
        Some(FlattenOr::Normal(info)) => {
            let field_ident = info.basics.ident;
            let handle_argument = try_parameter(
                apply_arg_to_field(field_ident, &arg_ident, &add_arg_ident),
                &info.basics.ident_str,
            );

            // TODO: currently, it's not possible for this to return the
            // unrecognized error that we want if the terminal positional
            // rejects this additional argument. Need to find a way to allow
            // for this, so that #[flatten] can work.
            quote! {
                self.fields.#field_ident = ::core::option::Option::Some(#handle_argument);
                ::core::result::Result::Ok(())
            }
        }
        // This is a flattened field
        Some(FlattenOr::Flatten(info)) => {
            let field_ident = info.basics.ident;

            quote! {
                ::debate::from_args::State::add_positional(
                    &mut self.fields.#field_ident,
                    argument
                )
            }
        }
    };

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
                .map(|short| Literal::byte_character(short as u8))
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

                let long = match long {
                    Some(long) => quote! {::core::option::Option::Some(#long)},
                    None => quote! {::core::option::Option::None},
                };

                let short = match short {
                    Some(short) => quote! {::core::option::Option::Some(#short)},
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

        impl<'arg> ::debate::from_args::State<'arg> for #state_ident<'arg> {
            fn add_positional<E>(
                &mut self,
                argument: ::debate_parser::Arg<'arg>
            ) -> ::core::result::Result<(), E>
            where
                E: ::debate::from_args::StateError<'arg, ()>
            {
                #(#visit_positional_arms)*
                #terminal_positional_arm
            }

            fn add_long_option<E>(
                &mut self,
                option: ::debate_parser::Arg<'arg>,
                argument: ::debate_parser::Arg<'arg>
            ) -> ::core::result::Result<(), E>
            where
                E: ::debate::from_args::StateError<'arg, ()>
            {
                match option.bytes() {
                    #(#visit_long_option_arms)*
                    _ => {
                        #(#visit_long_option_flattened_arms)*

                        ::core::result::Result::Err(
                            ::debate::from_args::StateError::unrecognized(())
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
                E: ::debate::from_args::StateError<'arg, A>
            {
                match option.bytes() {
                    #(#visit_long_arms)*
                    _ => {
                        #(#visit_long_flattened_arms)*

                        ::core::result::Result::Err(
                            ::debate::from_args::StateError::unrecognized(
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
                E: ::debate::from_args::StateError<'arg, A>
            {
                match option {
                    #(#visit_short_arms)*
                    _ => {
                        #(#visit_short_flattened_arms)*

                        ::core::result::Result::Err(
                            ::debate::from_args::StateError::unrecognized(
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
