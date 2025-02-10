use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    Attribute, DataEnum, DataStruct, DeriveInput, Expr, Field, Generics, Ident, Type,
    spanned::Spanned,
};

struct ParsedFieldInfo<'a> {
    name: &'a Ident,
    ty: &'a Type,

    long: Option<String>,
    short: Option<String>,

    default: Option<TokenStream2>,
    docs: String,
}

impl<'a> ParsedFieldInfo<'a> {
    pub fn from_field(field: &'a Field) {}
}

fn derive_args_struct(
    name: &Ident,
    data: &DataStruct,
    generics: &Generics,
    attrs: &[Attribute],
) -> syn::Result<TokenStream2> {
    Ok(quote! {
        impl<'arg> ::debate::FromArgs<'arg> for #name {
            fn from_args<I, E>(args: ::debate::primitives::Arguments<'arg, I>) -> Result<Self, E>
            where
                I: ::core::iter::Iterator<Item=&'arg [u8]>,
                E: ::debate::FlagError,
            {
                #[derive(Default)]
                struct FieldState<'arg> {

                }

                #[derive(Default)]
                struct State<'arg, E> {
                    fields: FieldState,
                    positional: u16,
                    help: bool,
                    error: ::core::marker::PhantomData<E>,
                }

                impl<'arg, E> ::debate::primitives::Visitor<'arg> for &mut State<'arg, E>
                where
                    E: ::debate::FlagError;
                {
                    type Value = Result<(), E>;

                    fn visit_positional(self, argument: Arg<'arg>) -> Self::Value {
                        todo!()
                    }

                    fn visit_long_option(self, option: Arg<'arg>, argument: Arg<'arg>) -> Self::Value {
                        todo!()
                    }

                    fn visit_long(
                        self,
                        option: Arg<'arg>,
                        arg: impl primitives::ArgAccess<'arg>,
                    ) -> Self::Value {
                        todo!()
                    }

                    fn visit_short(self, option: u8, arg: impl primitives::ArgAccess<'arg>) -> Self::Value {
                        match option {
                            b'p' => {
                                self.path = Some(match self.path.take() {
                                    None => Value::present(arg)?,
                                    Some(old) => Value::add_present(old, arg)?,
                                });
                                Ok(())
                            }
                        }
                    }
                }
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

    match input.data {
        syn::Data::Struct(ref data) => {
            derive_args_struct(&input.ident, data, &input.generics, &input.attrs)
        }
        syn::Data::Enum(ref data) => {
            derive_args_enum(&input.ident, data, &input.generics, &input.attrs)
        }
        syn::Data::Union(_) => Err(syn::Error::new(
            input.span(),
            "can't derive Args on a union",
        )),
    }
}

#[proc_macro_derive(Args, attributes(doc, arg, args))]
pub fn derive_args(item: TokenStream) -> TokenStream {
    match derive_args_result(item.into()) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}
