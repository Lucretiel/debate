use itertools::Itertools;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{ToTokens, TokenStreamExt, quote};
use syn::{
    Attribute, FnArg, Ident, Pat, PatType, Signature, Visibility, braced,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
    token::{Brace, Comma},
};

use crate::common::IdentString;

struct LazyPair<T, F> {
    first: Option<T>,
    second: Option<F>,
}

impl<T, F: FnOnce() -> Option<T>> LazyPair<T, F> {
    pub fn new(first: T, second: F) -> Self {
        Self {
            first: Some(first),
            second: Some(second),
        }
    }
}

impl<T, F: FnOnce() -> Option<T>> Iterator for LazyPair<T, F> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.first.take() {
            Some(item) => Some(item),
            None => self.second.take()?(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let count = self.first.is_some() as usize + self.second.is_some() as usize;
        (count, Some(count))
    }
}

fn is_args_attr(attr: &Attribute) -> bool {
    match attr.meta.require_path_only() {
        Ok(path) => path.is_ident("args"),
        Err(_) => false,
    }
}

/**
Get the function argument associated with the debate arguments object. This is
the only argument, or if there is more than one, the argument tagged with
#[args]
*/
fn extract_args_fn_input(
    args: impl IntoIterator<Item = FnArg>,
    span: Span,
) -> syn::Result<(PatType, Punctuated<FnArg, Comma>)> {
    let mut args = match args.into_iter().at_most_one() {
        Ok(None) => {
            return Err(syn::Error::new(
                span,
                "#[debate::main] function requires an argument, \
                that's the whole point",
            ));
        }
        Ok(Some(FnArg::Typed(arg))) => return Ok((arg, Punctuated::new())),
        Ok(Some(FnArg::Receiver(_))) => {
            return Err(syn::Error::new(
                span,
                "#[debate::main] function requires an argument \
                (`self` doesn't count)",
            ));
        }

        // Wow, aren't you fancy. Fine, we'll track it down ourselves.
        Err(args) => args,
    };

    let mut inputs = Punctuated::new();

    let arg = args
        .find_map(|arg| {
            let arg = match arg {
                FnArg::Typed(arg) if arg.attrs.iter().any(is_args_attr) => {
                    return Some(arg);
                }
                arg => arg,
            };

            inputs.push(arg);
            None
        })
        .ok_or_else(|| {
            syn::Error::new(
                span,
                "#[debate::main] function with more than one argument must \
                use #[args] to identify the CLI arguments parameter",
            )
        })?;

    inputs.extend(args);
    Ok((arg, inputs))
}

/// Check if an identifer is lowercase (defined as it does NOT contain
/// any uppercase, since punctuation is fine and it doesn't have a case)
fn ident_is_lowercase(ident: &IdentString<'_>) -> bool {
    ident.as_str().chars().all(|c| !c.is_uppercase())
}

/// Given a list of identifiers, find the best one. This is the first one
/// that is all lowercase, or else just the first one in the list.
fn find_best_ident<'a>(
    options: impl IntoIterator<Item = IdentString<'a>>,
) -> Option<IdentString<'a>> {
    let mut options = options.into_iter();
    let first = options.next()?;

    Some(match ident_is_lowercase(&first) {
        true => first,
        false => options
            .find(|ident| ident_is_lowercase(ident))
            .unwrap_or(first),
    })
}

fn find_best_ident_from_pattern_list<'a>(
    option: impl IntoIterator<Item = &'a Pat>,
) -> Option<IdentString<'a>> {
    find_best_ident(option.into_iter().filter_map(get_useful_ident))
}

/// Get an ident from the pattern. We don't really care which one, though we
/// prefer lowercase names. This ident will be shadowed.
fn get_useful_ident(pattern: &Pat) -> Option<IdentString<'_>> {
    match pattern {
        Pat::Ident(pat) => match pat.subpat {
            None => Some(IdentString::new(&pat.ident)),
            Some((_, ref subpat)) => {
                find_best_ident(LazyPair::new(IdentString::new(&pat.ident), || {
                    get_useful_ident(subpat)
                }))
            }
        },
        Pat::Or(pat) => find_best_ident_from_pattern_list(&pat.cases),
        Pat::Paren(inner) => get_useful_ident(&inner.pat),
        Pat::Path(path) => path.path.get_ident().map(IdentString::new),
        Pat::Reference(pat) => get_useful_ident(&pat.pat),
        Pat::Slice(pat) => find_best_ident_from_pattern_list(&pat.elems),
        Pat::Struct(pat) => {
            // Not totally sure this is correct, syn does some "helpful" stuff
            // here.
            find_best_ident_from_pattern_list(pat.fields.iter().map(|field| &*field.pat))
        }
        Pat::Tuple(pat) => find_best_ident_from_pattern_list(&pat.elems),
        Pat::TupleStruct(pat) => find_best_ident_from_pattern_list(&pat.elems),
        Pat::Type(typed) => get_useful_ident(&typed.pat),
        _ => None,
    }
}

enum Mode {
    Normal,
    Leak,
}

impl Parse for Mode {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident: Option<Ident> = input.parse()?;

        match ident {
            None => Ok(Mode::Normal),
            Some(ident) => match ident == "leak" {
                true => Ok(Mode::Leak),
                false => Err(syn::Error::new(ident.span(), "unrecognized token")),
            },
        }
    }
}

/// Basically the same as ItemFn, but without parsing all of the statements
/// within. Used for performance reasons for a macro that only cares about the
/// attributes and signature of a function and not its body. Also doesn't
/// care about inner attributes.
struct ItemFnBlob {
    outer_attrs: Vec<Attribute>,
    visibility: Visibility,
    signature: Signature,
    brace: Brace,
    inner_attrs: Vec<Attribute>,
    body: TokenStream2,
}

impl Parse for ItemFnBlob {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;

        Ok(Self {
            outer_attrs: input.call(Attribute::parse_outer)?,
            visibility: input.parse()?,
            signature: input.parse()?,
            brace: braced!(content in input),
            inner_attrs: content.call(Attribute::parse_inner)?,
            body: content.parse()?,
        })
    }
}

impl ToTokens for ItemFnBlob {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.append_all(&self.outer_attrs);
        self.visibility.to_tokens(tokens);
        self.signature.to_tokens(tokens);
        self.brace.surround(tokens, |inner| {
            inner.append_all(&self.inner_attrs);
            self.body.to_tokens(inner);
        });
    }
}

pub fn decorate_fn_main(attrs: TokenStream2, function: TokenStream2) -> syn::Result<TokenStream2> {
    let mode: Mode = syn::parse2(attrs)?;

    let mut function: ItemFnBlob = syn::parse2(function)?;

    // Identify the argument. It's probably the only argument, but we'll also
    // accept an argument tagged with #[args].
    let inputs_span = function.signature.paren_token.span.span();
    let (arg, updated_inputs) = extract_args_fn_input(function.signature.inputs, inputs_span)?;
    function.signature.inputs = updated_inputs;

    // Check that the arg doesn't have any weird attributes
    if let Some(weird) = arg.attrs.iter().find(|attr| !is_args_attr(attr)) {
        return Err(syn::Error::new(
            weird.span(),
            "unrecgonzied attribute on #[debate::main] CLI arguments parameter",
        ));
    }

    // Get a variable to store the `Arguments` in
    let storage_identifier = {
        let original_ident = get_useful_ident(&arg.pat)
            .ok_or_else(|| syn::Error::new(arg.pat.span(), "there aren't any variables here"))?
            .raw();

        let mut ident = original_ident.clone();
        ident.set_span(Span::mixed_site().located_at(original_ident.span()));
        ident
    };

    let user_pattern = &arg.pat;
    let user_type = &arg.ty;

    let body_prefix = {
        let storage = match mode {
            Mode::Normal => quote! {
                let #storage_identifier =
                    ::debate::arguments::LoadedArguments::from_env();
            },
            Mode::Leak => quote! {
                let #storage_identifier: &'static ::debate::arguments::LoadedArguments =
                    ::std::boxed::Box::leak(
                        ::std::boxed::Box::new(
                            ::debate::arguments::LoadedArguments::from_env()
                        )
                    );
            },
        };
        quote! {
            #storage

            let #user_pattern: #user_type =
                ::debate::arguments::LoadedArguments::parse(&#storage_identifier);
        }
    };

    let old_body = function.body;
    function.body = quote! {
        #body_prefix
        #old_body
    };

    Ok(function.into_token_stream())
}
