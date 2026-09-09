pub mod flag_set;
pub mod subcommand;

use darling::{FromAttributes, util::SpannedValue};
use itertools::Itertools;
use quote::format_ident;
use syn::Ident;

use super::IdentString;

/// Attributes that can be applied directly to an enum
#[derive(FromAttributes)]
#[darling(attributes(debate))]
pub struct ValueEnumAttr {
    pub subcommand: Option<()>,
    pub short: Option<SpannedValue<()>>,
    pub long: Option<SpannedValue<()>>,
    // TODO: help
    // TODO: name attribute to rename the command
    // TODO: override for simple flag sets
}

/// Create `mixed_site` identifer with a name that's `root` with enough prefixed
/// underscores that it doesn't collide with anything in `variants`
pub fn create_non_colliding_ident<'a>(
    root: &str,
    variants: impl Iterator<Item = &'a IdentString<'a>>,
) -> Ident {
    // We're aiming to prefix the root with as many underscores as necessary
    // so that it doesn't collide with anything in variants
    let underscore_counts = variants
        .map(|variant| variant.as_str())
        .filter_map(|variant| variant.strip_suffix(root))
        // At this point it's an iterator of prefixes. Find only the prefixes
        // that are all underscores.
        .filter(|prefix| prefix.as_bytes().iter().all(|&b| b == b'_'))
        // Count the underscores in each prefix
        .map(|prefix| prefix.len())
        .minmax();

    let count = match underscore_counts.into_option() {
        Some((0, max)) => max + 1,
        None | Some(..) => 0,
    };

    format_ident!("{:_<count$}{root}", "")
}

#[cfg(test)]
mod non_colliding_ident_tests {
    use super::*;

    use quote::format_ident;

    #[test]
    fn no_collision() {
        let ident1 = format_ident!("Ident1");
        let ident2 = format_ident!("Ident2");

        let out = create_non_colliding_ident(
            "Root",
            [IdentString::new(&ident1), IdentString::new(&ident2)].iter(),
        );

        assert_eq!(out, "Root")
    }

    #[test]
    fn collision() {
        let ident1 = format_ident!("_Root");
        let ident2 = format_ident!("Root");

        let out = create_non_colliding_ident(
            "Root",
            [IdentString::new(&ident1), IdentString::new(&ident2)].iter(),
        );

        assert_eq!(out, "__Root")
    }

    #[test]
    fn near_collision() {
        let ident1 = format_ident!("___Root");
        let ident2 = format_ident!("__Root");

        let out = create_non_colliding_ident(
            "Root",
            [IdentString::new(&ident1), IdentString::new(&ident2)].iter(),
        );

        assert_eq!(out, "Root")
    }
}
