use std::mem;

use itertools::Itertools;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::ToTokens;
use syn::{
    Attribute, FnArg, Ident, ItemFn, Pat, PatType,
    parse::{Parse, ParseStream},
    parse_quote,
    punctuated::Punctuated,
    spanned::Spanned,
    token::Comma,
};

use crate::common::IdentString;

enum LazyPair<T, F> {
    First(T, F),
    Second(F),
    Done,
}

impl<T, F: FnOnce() -> Option<T>> LazyPair<T, F> {
    pub fn new(first: T, second: F) -> Self {
        Self::First(first, second)
    }
}

impl<T, F: FnOnce() -> Option<T>> Iterator for LazyPair<T, F> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match mem::replace(self, LazyPair::Done) {
            LazyPair::First(item, func) => {
                *self = LazyPair::Second(func);
                Some(item)
            }
            LazyPair::Second(func) => func(),
            LazyPair::Done => None,
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match *self {
            Self::First(..) => (1, Some(2)),
            Self::Second(..) => (0, Some(1)),
            Self::Done => (0, Some(0)),
        }
    }
}

fn is_args_attr(attr: &Attribute) -> bool {
    match attr.meta.require_path_only() {
        Ok(path) => path.is_ident("args"),
        Err(_) => false,
    }
}

fn extract_args_fn_input(
    args: impl IntoIterator<Item = FnArg>,
    span: Span,
) -> syn::Result<(PatType, Punctuated<FnArg, Comma>)> {
    let args = args.into_iter();
    let mut args = match args.at_most_one() {
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

pub fn decorate_fn_main(attrs: TokenStream2, function: TokenStream2) -> syn::Result<TokenStream2> {
    let mode: Mode = syn::parse2(attrs)?;

    let mut function: ItemFn = syn::parse2(function)?;

    // Identify the argument. It's probably the only argument, but we'll also
    // accept an argument tagged with #[args].
    let inputs_span = function.sig.paren_token.span.span();
    let (arg, updated_inputs) = extract_args_fn_input(function.sig.inputs, inputs_span)?;
    function.sig.inputs = updated_inputs;

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

    let body_prefix = [
        match mode {
            Mode::Normal => parse_quote! {
                let #storage_identifier =
                    ::debate::arguments::LoadedArguments::from_env();
            },
            Mode::Leak => parse_quote! {
                let #storage_identifier: &'static ::debate::arguments::LoadedArguments =
                    ::std::boxed::Box::leak(
                        ::std::boxed::Box::new(
                            ::debate::arguments::LoadedArguments::from_env()
                        )
                    );
            },
        },
        parse_quote! {
            let #user_pattern: #user_type =
                ::debate::arguments::LoadedArguments::parse(&#storage_identifier);
        },
    ];

    function.block.stmts = itertools::chain(body_prefix, function.block.stmts).collect();

    Ok(function.into_token_stream())
}
