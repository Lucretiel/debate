/*!
A principled, type and trait-driven command line arguments parsing library;
debate is a more sensible way to handle (command-line) arguments.

# Quick start

This example includes a quick reference for most of debtate's features.

```
use debate::{FromArgs, Usage, Value, ParameterUsage};

#[derive(Debug, FromArgs, Usage)]
#[debate(help)] // To create a -h / --help flag, for usage messages
struct MyProgram<'a> {
    /// A mandatory positional parameter.
    path: String,

    /// An optional positional parameter
    second_path: Option<String>,

    // To make flags, use `short` and/or `long`

    /// A simple switch, enabled with --enable.
    #[debate(short='e', long="enable")]
    switch: bool,

    /// A flag, `-t` and `--target-name` taking an argument. Invert creates
    /// an additional flag `--no-target-name` that clears the target name.
    #[debate(short, long, invert)]
    target_name: Option<String>,

    /// A mandatory flag, `--numeric-value`
    #[debate(long)]
    numeric_value: u32,

    /// An optional flag that uses `Default::default` if the flag is omitted
    #[debate(long="number", default)]
    defaulted_number: u32,

    /// An optional flag, `--default-ten` that populates its value with an
    /// expression if the flag is omitted. `--back-to-ten` resets this value
    /// to its default
    #[debate(long, default=10, invert="back-to-ten")]
    default_ten: i32,

    /// Arguments can borrow from the command line.
    #[debate(long)]
    another_string: Option<&'a str>,

    /// Other `FromArgs` types can be nested with `--flatten`. This is now
    /// subcommands can be added.
}

// Use `debate::main` to inject command-line arguments as a function argument
// into `main`. This is compatible with other `main` decorators, like
// `tokio::main`
#[debate::main]
fn main(args: MyProgram<'_>) {
    eprintln!("{args:?}")
}

/// Subcommands are enums. Each subcommand can have its own unique set of
/// arguments, including additional arguments via `#[debate(flatten)]`
#[derive(Debug, FromArgs, Usage)]
#[debate(subcommand)]
enum Subcommand {
    Build {
        #[debate(long)]
        name: Option<String>,

        #[debate(flatten)]
        mode: Option<Target>
    },
    Clean,
}

/// Mutually exclusive flags are also enums
#[derive(Debug, FromArgs, Usage)]
#[debate(long)]
enum Target {
    Release,
    Debug,
}

/// Mutually exclusive flags can be arbitrarily complex

enum Volume {
}


```

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
