#![no_std]

#[cfg(feature = "std")]
extern crate std;

pub mod build;
pub mod errors;
pub mod from_args;
pub mod help;
mod impls;
pub mod parameter;
pub mod state;
pub mod util;

#[cfg(feature = "std")]
pub mod arguments;

#[cfg(feature = "std")]
mod printers;

// TODO: re-exports

/// The set of tags that identify a particular option (`-short`, `--long`)
#[derive(Debug, Clone, Copy)]
pub enum Tags<'a> {
    /// This parameter uses only a long tag
    Long { long: &'a str },

    /// This parameter uses only a short tag
    Short { short: char },

    /// This parameter uses both a long and short tag
    LongShort { long: &'a str, short: char },
}

impl<'a> Tags<'a> {
    /// Get the long tag, if any
    #[inline]
    #[must_use]
    pub const fn long(&self) -> Option<&'a str> {
        match self {
            Tags::Long { long } | Tags::LongShort { long, .. } => Some(long),
            Tags::Short { .. } => None,
        }
    }

    /// Get the short tag, if any
    #[inline]
    #[must_use]
    pub const fn short(&self) -> Option<char> {
        match self {
            Tags::Short { short } | Tags::LongShort { short, .. } => Some(*short),
            Tags::Long { .. } => None,
        }
    }
}
