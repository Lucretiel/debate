use itertools::Itertools;
use proc_macro2::{Punct, Span, TokenStream as TokenStream2, TokenTree as TokenTree2};
use quote::ToTokens;
use syn::{Generics, Lifetime, spanned::Spanned};

/// A lifetime that, when rendered with `to_tokens`, includes `< >` around it.
/// This exists because you can wrap it in `Option` to make it easy to omit
/// or include an entire generics clause.
pub struct AngleBracedLifetime {
    lifetime: Lifetime,
}

impl ToTokens for AngleBracedLifetime {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        use proc_macro2::Spacing::*;

        tokens.extend([TokenTree2::Punct(Punct::new('<', Alone))]);
        self.lifetime.to_tokens(tokens);
        tokens.extend([TokenTree2::Punct(Punct::new('>', Alone))]);
    }
}

/// Helper for computing the lifetime generics for an impl block. Most `debate`
/// impl blocks want an `'arg` lifetime, and this lifetime should match
/// any lifetime that exists on the type already.
pub fn compute_generics(
    generics: &Generics,
) -> syn::Result<(Lifetime, Option<AngleBracedLifetime>)> {
    if let Some(param) = generics.const_params().next() {
        return Err(syn::Error::new(
            param.span(),
            "const generics aren't (yet) supported by debate",
        ));
    }

    if let Some(param) = generics.type_params().next() {
        return Err(syn::Error::new(
            param.span(),
            "generic types aren't (yet) supported by debate",
        ));
    }

    generics
        .lifetimes()
        .at_most_one()
        .map(|param| match param {
            None => (Lifetime::new("'arg", Span::mixed_site()), None),
            Some(param) => (
                param.lifetime.clone(),
                Some(AngleBracedLifetime {
                    lifetime: param.lifetime.clone(),
                }),
            ),
        })
        .map_err(|_| {
            syn::Error::new(
                generics.params.span(),
                "can't derive debate traits with more than one lifetime",
            )
        })
}
