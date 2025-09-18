/*!
A principled, type and trait-driven command line arguments parsing library;
debate is a more sensible way to handle (command-line) arguments.

More substantial docs and examples are coming. If you're here, you're probably
interested in the [`parameter`] module, which provides traits you can implement
on your own types to make them parsable as command line arguments, or in
`debate`'s macros:

- [`#[derive(FromArgs)]`][FromArgs] and [`#[derive(Usage)]`][Usage] can be
  derived on your structs, making them into containers for parsed command-line
  arguments. The `#[debate]` attribute lets you customize parsing behavior with
  attributes like `short`, `long`, `default`, `placeholder`, `override`, and
  `invert`. It's likely that, in the future, it won't be necessary to separately
  derive [`Usage`].

- [`#[debate::main]`][main] can be placed on your `main` function to make the
  command line arguments available as a function argument. It even works on
  tokio `async fn main`!

Until we write more complete docs and examples, check out the
[debate-demo](https://github.com/Lucretiel/debate/blob/main/debate-demo/src/main.rs)
for examples on how to use `debate`.
*/

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

pub use debate_derive::{FromArgs, ParameterUsage, Usage, Value, main};
pub use debate_parser::Arg;

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
