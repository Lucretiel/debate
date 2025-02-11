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

enum ParsedAttr {
    Arg {
        long: Option<Option<String>>,
        short: Option<Option<char>>,
        default: Option<Option<Expr>>,
        // clear = "no-verbose"
        // placeholder = "VALUE"
    },
    Flatten,
    Subcommand,
}

impl Parse for ParsedAttr {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        {
            let input = input.fork();
            let ident: Ident = input.parse()?;

            // Check flatten and subcommand. Deduplicate.
        }

        let mut long = None;
        let mut short = None;
        let mut default = None;

        loop {
            let ident: Ident = input.parse()?;

            // TODO: for the love of god find a way to deduplicate this
            if ident == "long" {
                if long.is_some() {
                    return Err(syn::Error::new(ident.span(), "duplicate field 'long'"));
                }
                let MaybeValue { value }: MaybeValue<LitStr> = input.parse()?;
                long = Some(value.map(|lit| lit.value()))
            } else if ident == "short" {
                if short.is_some() {
                    return Err(syn::Error::new(ident.span(), "duplicate field 'short'"));
                }
                let MaybeValue { value }: MaybeValue<LitChar> = input.parse()?;
                short = Some(value.map(|lit| lit.value()))
            } else if ident == "default" {
                if default.is_some() {
                    return Err(syn::Error::new(ident.span(), "duplicate field 'default'"));
                }
                let MaybeValue { value } = input.parse()?;
                default = Some(value);
            } else {
                return Err(syn::Error::new(ident.span(), "unrecognized attribute"));
            }

            if input.is_empty() {
                return Ok(Self::Arg {
                    long,
                    short,
                    default,
                });
            }

            let _comma: Token![,] = input.parse()?;

            if input.is_empty() {
                return Ok(Self::Arg {
                    long,
                    short,
                    default,
                });
            }
        }
    }
}

struct ParsedFieldInfo<'a> {
    ident: &'a Ident,
    ty: &'a Type,
    span: Span,

    default: Option<Option<Expr>>,
    docs: String,
}

fn compute_long(
    user_long: Option<String>,
    field_name: Option<&Ident>,
    span: Span,
) -> syn::Result<String> {
    user_long
        .or_else(|| field_name.map(|name| name.to_string().to_kebab_case()))
        .ok_or_else(|| {
            syn::Error::new(
                span,
                "tuple struct fields can't have a naked `long` attribute",
            )
        })
}

fn compute_short(
    user_short: Option<char>,
    field_name: Option<&Ident>,
    span: Span,
) -> syn::Result<char> {
    user_short
        .or_else(|| {
            // TODO: verify that the char is printable
            field_name.map(|name| {
                name.to_string()
                    .chars()
                    .next()
                    .expect("Identifiers can't be empty")
            })
        })
        .ok_or_else(|| {
            syn::Error::new(
                span,
                "tuple struct fields can't have a naked `short` attribute",
            )
        })
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
    pub fn from_field(field: &'a Field, idx: u32) -> syn::Result<(Self, ParamClassification)> {
        let debate_attr = field
            .attrs
            .iter()
            .filter(|attr| attr.path().is_ident("debate"))
            .at_most_one()
            .map_err(|mut err| {
                let extra = err.nth(1).unwrap();
                syn::Error::new(extra.span(), "extra `debate` attribute")
            })?;

        let (long, short, default) = match debate_attr {
            Some(debate_attr) => match debate_attr.parse_args()? {
                ParsedAttr::Flatten => panic!("flatten unimplemented"),
                ParsedAttr::Subcommand => panic!("subcommand unimplemented"),
                ParsedAttr::Arg {
                    long,
                    short,
                    default,
                } => {
                    let long = long
                        .map(|long| compute_long(long, field.ident.as_ref(), field.span()))
                        .transpose()?;
                    let short = short
                        .map(|short| compute_short(short, field.ident.as_ref(), field.span()))
                        .transpose()?;

                    (long, short, default)
                }
            },
            None => (None, None, None),
        };

        ::core::result::Result::Ok((
            Self {
                ident: field
                    .ident
                    .as_ref()
                    .expect("already checked that this isn't a tuple struct"),
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

    for (field, idx) in fields.iter().zip(0..) {
        let (parsed, classification) = ParsedFieldInfo::from_field(field, idx)?;

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

    let visit_long_option_arms = options
        .iter()
        .filter_map(|(tag, info)| tag.long().map(|long| (long, info)))
        .map(|(long, info)| {
            let long_bytes = Literal::byte_string(long.as_bytes());
            let field_ident = info.ident;

            // TODO: We're gonna need to switch away from a match when we do
            // subcommands and flattened args.
            // TODO: in a REALLY ideal world, we continue to use a match if
            // all arguments are normal
            // TODO: phf?
            quote! {
                #long_bytes => {
                    self.fields.#field_ident = ::core::option::Option::Some(
                        match match self.fields.#field_ident.take() {
                            ::core::option::Option::None => ::debate::Parameter::arg(argument),
                            ::core::option::Option::Some(old) => ::debate::Parameter::add_arg(old, argument),
                        } {
                            ::core::result::Result::Ok(value) => value,
                            ::core::result::Result::Err(err) => return ::core::result::Result::Err(
                                ::debate::error::FlagError::long(option, err)
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
            let field_ident = &info.ident;

            let default = match info.default {
                Some(Some(ref expr)) => quote! { #expr },
                Some(None) => quote! { ::core::default::Default::default() },
                None => quote! { match ::debate::Parameter::absent() {
                    ::core::result::Result::Ok(value) => value,
                    ::core::result::Result::Err(err) => return ::core::result::Result::Err(
                        panic!("Need an error type here")
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
        impl<'arg> ::debate::FromArgs<'arg> for #name {
            fn from_args<I, E>(mut args: ::debate::primitives::Arguments<'arg, I>) -> Result<Self, E>
            where
                I: ::core::iter::Iterator<Item=&'arg [u8]>,
                E: ::debate::error::FlagError,
            {
                #[derive(::core::default::Default)]
                struct FieldState<'arg> {
                    #(#field_state_contents)*

                    // TODO: better name for this field
                    __phantom: ::core::marker::PhantomData<&'arg ()>,
                }

                struct State<'arg, E> {
                    fields: FieldState<'arg>,
                    positional: u16,
                    help: bool,
                    error: ::core::marker::PhantomData<E>,
                }

                impl<E> State<'_, E> {
                    fn new() -> Self {
                        Self {
                            fields: ::core::default::Default::default(),
                            positional: 0,
                            help: false,
                            error: ::core::marker::PhantomData,
                        }
                    }
                }

                impl<'arg, E> ::debate::primitives::Visitor<'arg> for &mut State<'arg, E>
                where
                    E: ::debate::error::FlagError
                {
                    type Value = Result<(), E>;

                    fn visit_positional(
                        self,
                        argument: ::debate::primitives::Arg<'arg>
                    ) -> Self::Value {
                        todo!()
                    }

                    fn visit_long_option(
                        self,
                        option: ::debate::primitives::Arg<'arg>,
                        argument: ::debate::primitives::Arg<'arg>
                    ) -> Self::Value {
                        match option.bytes() {
                            #(#visit_long_option_arms)*
                            unrecognized => ::core::result::Result::Err(
                                ::debate::error::FlagError::unrecognized_long(
                                    option,
                                    ::core::option::Option::Some(argument)
                                )
                            ),
                        }
                    }

                    fn visit_long(
                        self,
                        option: ::debate::primitives::Arg<'arg>,
                        arg: impl ::debate::primitives::ArgAccess<'arg>,
                    ) -> Self::Value {
                        todo!()
                    }

                    fn visit_short(
                        self,
                        option: u8,
                        argument: impl ::debate::primitives::ArgAccess<'arg>) -> Self::Value {
                        todo!()
                    }
                }

                let mut state: State<'_, E> = State::new();

                while let ::core::option::Option::Some(()) = args.next_arg(&mut state).transpose()? {}

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

fn derive_args_result(item: TokenStream2) -> syn::Result<TokenStream2> {
    let input: DeriveInput = syn::parse2(item)?;

    let no_derive = |msg| Err(syn::Error::new(input.span(), msg));

    match input.data {
        syn::Data::Struct(ref data) => derive_args_struct(
            &input.ident,
            match data.fields {
                Fields::Named(ref fields) => &fields.named,
                Fields::Unnamed(_) => return no_derive("Can't derive `Debate` on a tuple struct"),
                Fields::Unit => return no_derive("can't derive `Debate` on a unit struct"),
            },
            &input.generics,
            &input.attrs,
        ),
        syn::Data::Enum(ref data) => {
            derive_args_enum(&input.ident, data, &input.generics, &input.attrs)
        }
        syn::Data::Union(_) => Err(syn::Error::new(
            input.span(),
            "can't derive `Debate` on a union",
        )),
    }
}

#[proc_macro_derive(FromArgs, attributes(doc, debate))]
pub fn derive_args(item: TokenStream) -> TokenStream {
    match derive_args_result(item.into()) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}
