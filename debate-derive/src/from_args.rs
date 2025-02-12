use std::default;

use darling::{FromMeta, util::Override};
use heck::ToKebabCase;
use itertools::Itertools as _;
use proc_macro::TokenStream;
use proc_macro2::{Literal, Span, TokenStream as TokenStream2};
use quote::{ToTokens, format_ident, quote};
use syn::{
    Attribute, DataEnum, DeriveInput, Expr, Field, Fields, Generics, Ident, LitChar, LitStr, Token,
    Type,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
};

struct MaybeValue<T> {
    value: Option<T>,
}

impl<T: Parse> Parse for MaybeValue<T> {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let eq: Option<Token![=]> = input.parse()?;
        let value = eq.map(|_| input.parse()).transpose()?;

        Ok(Self { value })
    }
}

#[derive(darling::FromMeta)]
struct ParsedAttr {
    long: Option<Override<String>>,
    short: Option<Override<char>>,
    default: Option<Override<Expr>>,
    // clear = "no-verbose"
    // placeholder = "VALUE"
}

// impl Parse for ParsedAttr {
//     fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
//         let mut long = None;
//         let mut short = None;
//         let mut default = None;

//         loop {
//             let ident: Ident = input.parse()?;

//             // TODO: for the love of god find a way to deduplicate this.
//             // It seems like maybe `darling` is good for generating code like
//             // this? Probably we should just write a macro.
//             if ident == "long" {
//                 if long.is_some() {
//                     return Err(syn::Error::new(ident.span(), "duplicate field 'long'"));
//                 }
//                 let MaybeValue { value }: MaybeValue<LitStr> = input.parse()?;
//                 long = Some(value.map(|lit| lit.value()))
//             } else if ident == "short" {
//                 if short.is_some() {
//                     return Err(syn::Error::new(ident.span(), "duplicate field 'short'"));
//                 }
//                 let MaybeValue { value }: MaybeValue<LitChar> = input.parse()?;
//                 short = Some(value.map(|lit| lit.value()))
//             } else if ident == "default" {
//                 if default.is_some() {
//                     return Err(syn::Error::new(ident.span(), "duplicate field 'default'"));
//                 }
//                 let MaybeValue { value } = input.parse()?;
//                 default = Some(value);
//             } else {
//                 return Err(syn::Error::new(ident.span(), "unrecognized attribute"));
//             }

//             if input.is_empty() {
//                 return Ok(Self::Arg {
//                     long,
//                     short,
//                     default,
//                 });
//             }

//             let _comma: Token![,] = input.parse()?;

//             if input.is_empty() {
//                 return Ok(Self::Arg {
//                     long,
//                     short,
//                     default,
//                 });
//             }
//         }
//     }
// }

struct ParsedFieldInfo<'a> {
    ident: &'a Ident,
    ty: &'a Type,
    span: Span,

    default: Option<Option<Expr>>,
    docs: String,
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

enum ParamClassification {
    Positional,
    Option(OptionTag),
}

impl ParamClassification {
    pub fn new(long: Option<String>, short: Option<char>) -> Self {
        match (long, short) {
            (Some(long), Some(short)) => {
                ParamClassification::Option(OptionTag::LongShort(long, short))
            }
            (Some(long), None) => ParamClassification::Option(OptionTag::Long(long)),
            (None, Some(short)) => ParamClassification::Option(OptionTag::Short(short)),
            (None, None) => ParamClassification::Positional,
        }
    }
}

impl<'a> ParsedFieldInfo<'a> {
    pub fn from_field(field: &'a Field) -> syn::Result<(Self, ParamClassification)> {
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

        let (long, short, default) = match debate_attr {
            Some(debate_attr) => {
                let parsed = ParsedAttr::from_meta(&debate_attr.meta)?;
                let long = parsed
                    .long
                    .map(|long| compute_long(long.explicit(), ident, field.span()))
                    .transpose()?;
                let short = parsed
                    .short
                    .map(|short| compute_short(short.explicit(), ident, field.span()))
                    .transpose()?;

                (
                    long,
                    short,
                    parsed.default.map(|default| default.explicit()),
                )
            }
            None => (None, None, None),
        };

        ::core::result::Result::Ok((
            Self {
                ident,
                ty: &field.ty,
                span: field.span(),
                default,
                docs: String::new(),
            },
            ParamClassification::new(long, short),
        ))
    }
}

fn derive_args_struct(
    name: &Ident,
    fields: &Punctuated<Field, Token![,]>,
    generics: &Generics,
    attrs: &[Attribute],
) -> syn::Result<TokenStream2> {
    let mut options = Vec::new();
    let mut positionals = Vec::new();

    for field in fields.iter() {
        let (parsed, classification) = ParsedFieldInfo::from_field(field)?;

        match classification {
            ParamClassification::Positional => positionals.push(parsed),
            ParamClassification::Option(option_tag) => options.push((option_tag, parsed)),
        }
    }

    let field_state_contents = options
        .iter()
        .map(|(_, info)| info)
        .chain(&positionals)
        .map(|info| {
            let ident = info.ident;
            let ty = info.ty;

            quote! { #ident : ::core::option::Option<#ty>, }
        });

    let visit_positional_arms = match positionals.as_slice().split_last() {
        None => quote! {
            _ => ::core::result::Result::Err(
                ::debate::from_args::StateError::unrecognized(argument)
            ),
        },
        Some((last, positionals)) => {
            let arms = positionals.iter().enumerate().map(|(idx, info)| {
                // quote will add a typed integer suffix, which is usually
                // great, but in this case we'd rather not.
                let idx = Literal::usize_unsuffixed(idx);
                let field_ident = info.ident;
                let field_ident_str = field_ident.to_string();

                quote! {
                    #idx => {
                        self.fields.#field_ident = match ::debate::parameter::Parameter::arg(argument) {
                            ::core::result::Result::Ok(value) => Some(value),
                            ::core::result::Result::Err(err) => return ::core::result::Result::Err(
                                ::debate::from_args::StateError::parameter(#field_ident_str, err)
                            ),
                        };
                        self.position += 1;
                        ::core::result::Result::Ok(())
                    }
                }
            });

            let last_field_ident = last.ident;
            let last_field_ident_str = last_field_ident.to_string();

            quote! {
                #(#arms)*
                _ => {
                    self.fields.#last_field_ident = ::core::option::Option::Some(
                        match match self.fields.#last_field_ident.take() {
                            ::core::option::Option::None => ::debate::parameter::Parameter::arg(argument),
                            ::core::option::Option::Some(old) => ::debate::parameter::Parameter::add_arg(old, argument),
                        } {
                            ::core::result::Result::Ok(value) => value,
                            ::core::result::Result::Err(err) => return ::core::result::Result::Err(
                                ::debate::from_args::StateError::parameter(#last_field_ident_str, err)
                            ),
                        }
                    );
                    ::core::result::Result::Ok(())
                }
            }
        }
    };

    let visit_long_option_arms = options
        .iter()
        .filter_map(|(tag, info)| tag.long().map(|long| (long, info)))
        .map(|(long, info)| {
            let long_bytes = Literal::byte_string(long.as_bytes());
            let field_ident = info.ident;
            let field_ident_str = field_ident.to_string();

            // TODO: We're gonna need to switch away from a match when we do
            // subcommands and flattened args.
            // TODO: in a REALLY ideal world, we continue to use a match if
            // all arguments are normal
            // TODO: phf?
            quote! {
                #long_bytes => {
                    self.fields.#field_ident = ::core::option::Option::Some(
                        match match self.fields.#field_ident.take() {
                            ::core::option::Option::None => ::debate::parameter::Parameter::arg(argument),
                            ::core::option::Option::Some(old) => ::debate::parameter::Parameter::add_arg(old, argument),
                        } {
                            ::core::result::Result::Ok(value) => value,
                            ::core::result::Result::Err(err) => return ::core::result::Result::Err(
                                ::debate::from_args::StateError::parameter(#field_ident_str, err)
                            ),
                        }
                    );
                    ::core::result::Result::Ok(())
                }
            }
        });

    let visit_long_arms = options
            .iter()
            .filter_map(|(tag, info)| tag.long().map(|long| (long, info)))
            .map(|(long, info)| {
                let long_bytes = Literal::byte_string(long.as_bytes());
                let field_ident = info.ident;
                let field_ident_str = field_ident.to_string();

                quote! {
                    #long_bytes => {
                        self.fields.#field_ident = ::core::option::Option::Some(
                            match match self.fields.#field_ident.take() {
                                ::core::option::Option::None => ::debate::parameter::Parameter::present(argument),
                                ::core::option::Option::Some(old) => ::debate::parameter::Parameter::add_present(old, argument),
                            } {
                                ::core::result::Result::Ok(value) => value,
                                ::core::result::Result::Err(err) => return ::core::result::Result::Err(
                                    ::debate::from_args::StateError::parameter(#field_ident_str, err)
                                ),
                            }
                        );
                        ::core::result::Result::Ok(())
                    }
                }
            });

    let visit_short_arms = options
        .iter()
        .filter_map(|(tag, info)| tag.short().map(|short| (short, info)))
        .map(|(short, info)| {
            // TODO: move this conversion to elsewhere
            let short_byte = Literal::byte_character(short as u8);
            let field_ident = info.ident;
            let field_ident_str = field_ident.to_string();

            // TODO: We're gonna need to switch away from a match when we do
            // subcommands and flattened args.
            // TODO: in a REALLY ideal world, we continue to use a match if
            // all arguments are normal
            // TODO: phf?
            quote! {
                #short_byte => {
                    self.fields.#field_ident = ::core::option::Option::Some(
                        match match self.fields.#field_ident.take() {
                            ::core::option::Option::None => ::debate::parameter::Parameter::present(argument),
                            ::core::option::Option::Some(old) => ::debate::parameter::Parameter::add_present(old, argument),
                        } {
                            ::core::result::Result::Ok(value) => value,
                            ::core::result::Result::Err(err) => return ::core::result::Result::Err(
                                ::debate::from_args::StateError::parameter(#field_ident_str, err)
                            ),
                        }
                    );
                    ::core::result::Result::Ok(())
                }
            }
        });

    let final_field_initializers = options
        .iter()
        .map(|(tags, info)| (Some(tags), info))
        .chain(positionals.iter().map(|info| (None, info)))
        .map(|(tags, info)| {
            let field_ident = info.ident;
            let field_ident_str = field_ident.to_string();

            let (long, short) = match tags {
                None => (None, None),
                Some(tags) => match tags {
                    OptionTag::Long(long) => (Some(long.as_str()), None),
                    OptionTag::Short(short) => (None, Some(short)),
                    OptionTag::LongShort(long, short) => (Some(long.as_str()), Some(short)),
                },
            };

            let long = match long {
                Some(long) => quote! {::core::option::Option::Some(#long)},
                None => quote! {::core::option::Option::None},
            };

            let short = match short {
                Some(short) => quote! {::core::option::Option::Some(#short)},
                None => quote! {::core::option::Option::None},
            };

            let default = match info.default {
                Some(Some(ref expr)) => quote! { #expr },
                Some(None) => quote! { ::core::default::Default::default() },
                None => quote! { match ::debate::parameter::Parameter::absent() {
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
        });

    Ok(quote! {
        // TODO: instead of polluting the namespace with this pair of types,
        // consider using a tuple? Need to find a good way to map field
        // identifiers to it.
        #[derive(::core::default::Default)]
        #[doc(hidden)]
        struct __FieldState {
            #(#field_state_contents)*
        }

        #[doc(hidden)]
        #[derive(::core::default::Default)]
        struct __State {
            fields: __FieldState,
            position: u16,
            help: bool,
        }

        impl<'arg> ::debate::from_args::State<'arg> for __State {
            fn add_positional<E>(
                &mut self,
                argument: ::debate_parser::Arg<'arg>
            ) -> ::core::result::Result<(), E>
            where
                E: ::debate::from_args::StateError<'arg, ()>
            {
                match self.position {
                    #visit_positional_arms
                }
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
                    _ => ::core::result::Result::Err(
                        ::debate::from_args::StateError::unrecognized(())
                    ),
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
                    _ => ::core::result::Result::Err(
                        ::debate::from_args::StateError::unrecognized(
                            argument
                        )
                    ),
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
                    _ => ::core::result::Result::Err(
                        ::debate::from_args::StateError::unrecognized(
                            argument
                        )
                    ),
                }
            }
        }

        impl<'arg> ::debate::from_args::BuildFromArgs<'arg> for #name {
            type State = __State;

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
