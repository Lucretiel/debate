use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, quote};
use syn::{Ident, Type, token::Token};

pub struct IdentString<'a> {
    raw: &'a Ident,
    string: String,
}

impl<'a> IdentString<'a> {
    pub fn new(ident: &'a Ident) -> Self {
        Self {
            string: ident.to_string(),
            raw: ident,
        }
    }

    pub fn as_str(&self) -> &str {
        self.string.as_str()
    }

    pub fn raw(&self) -> &'a Ident {
        self.raw
    }
}

impl ToTokens for IdentString<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.raw.to_tokens(tokens);
    }
}

pub enum FlattenOr<F, T> {
    Flatten(F),
    Normal(T),
}

/// Create a state block, in curlies. Used both for the struct state, and
/// for separate enum variant states
pub fn make_struct_state_block<'a>(
    fields: impl IntoIterator<Item = FlattenOr<&'a Type, &'a Type>>,
) -> impl ToTokens {
    let field_state_contents = fields.into_iter().map(|field| match field {
        FlattenOr::Flatten(ty) => quote! {
            <#ty as ::debate::from_args::BuildFromArgs<'arg>>::State
        },
        FlattenOr::Normal(ty) => quote! {
            ::core::option::Option<#ty>
        },
    });

    quote! {
        {
            fields: (#(#field_state_contents,)*),
            position: u16,
            phantom: ::core::marker::PhantomData<& 'arg ()>,
        }
    }
}
