use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{Ident, Lifetime, Token, Variant, punctuated::Punctuated};

use crate::generics::AngleBracedLifetime;

pub fn derive_args_enum_flag_set(
    name: &Ident,
    variants: &Punctuated<Variant, Token![,]>,
    lifetime: &Lifetime,
    type_lifetime: Option<&AngleBracedLifetime>,
) -> syn::Result<TokenStream2> {
    let state_ident = format_ident!("__{name}State");

    /*
    Let's talk algorithm.

    enum Flags {
        Flag,
        Arg(arg),
        Set3 {
            bar: bool,
            baz: bool,
        }
        Set {
            foo: i32,
            bar: bool,
        }
        Set2 {
            foo: i32,
            baz: bool,
        }
    }

    The flags here are --flag, --arg, --foo, --bar, and --baz. Most of them
    are unique (and, in fact, we expect a vast majority of flags using the
    enum to be unique).

    We require that each instance of a tag across the enum be identical.

    Each time we receive a flag, we check if it's unique, and if so,
    unconditionally switch our state to that flag.

    Otherwise, we update the viability set. The viability set is initially that
    all variants are viable, but each flag disables viability for each variant
    it isn't a member of. The idea is that, because flags can be in default
    states, it isn't sufficient to just try each one in sequence; we have to
    actively track which set the user is opting into, and consider only those.

    For each viable variant, we'll attempt to construct it, falling through
    if any of its non-optional fields are absent.

    If ALL of the flags are unique, then we won't even bother tracking the
    viability set; it's a large array of bools and a cost we don't need to pay.

    TODO: figure out error reporting for this.
    */
    Ok(quote! {
        enum #state_ident <#lifetime> {
            // In the generic state, we haven't selected a specific variant yet

        }
    })
}
