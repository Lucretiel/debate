use proc_macro2::TokenStream as TokenStream2;
use quote::ToTokens;
use syn::Ident;

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
