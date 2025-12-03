/*!
A principled, type and trait-driven command line arguments parsing library;
debate is a more sensible way to handle (command-line) arguments.

# Quick start

This example includes a quick reference for most of debate's common features.

```
use debate::{FromArgs, Usage, Value, ParameterUsage};

/// This is a usage message for the overall program; it's printed at the top,
/// before the individual flags and arguments
#[derive(Debug, FromArgs, Usage)]
// #[debate(help)] to create a -h / --help flag, for usage messages.
// Usage messages will be populated from the docstrings for each field.
#[debate(help)]
struct MyProgram<'a> {
    /// A mandatory positional parameter
    path: String,

    /// An optional positional parameter
    second_path: Option<String>,

    // To make flags, use `short` and/or `long`

    /// A simple switch, enabled with --enable or -e.
    #[debate(short='e', long="enable")]
    switch: bool,

    /// An optional flag, `-t` or `--target-name`, taking an argument.
    #[debate(short, long)]
    target_name: Option<String>,

    /// A mandatory flag, `--numeric-value`
    #[debate(long)]
    numeric_value: u32,

    /// An optional flag, --number, that uses `Default::default` if the flag is
    /// omitted
    #[debate(long="number", default)]
    defaulted_number: u32,

    /// An optional flag, `--default-ten` that populates its value with an
    /// expression if the flag is omitted.
    #[debate(long, default=10)]
    default_ten: i32,

    /// A simple switch, --switch2, that resets to false if you pass
    /// --no-switch2
    #[debate(long, invert)]
    switch2: bool,

    /// A mandatory flag, `--selection`, that can be given more than once;
    /// the last one is used. Must be `small`, `medium`, or `large`.
    #[debate(long, override)]
    selection: Size,

    /// Arguments can borrow from the command line.
    #[debate(long)]
    another_string: Option<&'a str>,

    /// `FromArgs` types can be nested.
    #[debate(flatten)]
    subcommand: Subcommand
}

#[derive(Debug, Value, ParameterUsage)]
enum Size {
    Small,
    Medium,
    Large,
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

/// Mutually exclusive flags are also enums. This flag is exactly one of
/// `--release`, `--debug`, or `--target <TARGET>`. Arbitrarily complex sets
/// of flags are permissible here, even flags that are reused between
/// different variants.
#[derive(Debug, FromArgs, Usage)]
#[debate(long)]
enum Target {
    Debug,
    Release,
    Target(String),
}
```

# Using the Derives

Debate is designed as a derive-first library. Unlike most other command-line
parsing libraries, it does not have any sense of a "parser object", which is
filled at runtime with arguments and populates WHAT DOES IT POPULATE. WHAT
DEBATE DOES INSTEAD. This section describes the specific logic that informs
debate's derives.

## `BuildFromArgs`

`#[derive(BuildFromArgs)]` generates a [`BuildFromArgs`] implementation for a
type, allowing it to be parsed from raw command-line arguments. Each field on
the type is a parameter, associated with a single flag or positional parameter.

During parsing, each incoming raw argument will be matched with a parameter,
and the [`Parameter`][parameter::Parameter] trait used to parse the argument.
Positional parameters will be be matched in definition order, from first to
last, while flags will be matched directly by name.

### Attributes

You can use attributes to mark fields as flags or customize the behavior of the
command line argument parsing. A complete list of field attributes is provided
here. All attributes are optional unless otherwise specified.

- `#[debate(short [= 'f'])]`, `#[debate(long [= "flag"])]`

  A `short` or a `long` attribute added to a field marks that field as a flag,
  rather than a positional parameter.  If an explicit flag name is omitted, the
  field name (converted to `kebab-case`) is used.

- `#[debate(default [= <expr>])]`

  A `default` attribute defines the value a field should have if it is omitted
  from the command line. If an `<expr>` is given, the field is initialized with
  that expression; otherwise, the [`Default`] value is used.

  If `default` is omitted, a type's [`Parameter::absent`][parameter::Parameter::absent]
  method will define what happens of a parameter is absent. Usually this will
  be an error, but some types (like `bool`, `Option`, and `Vec`) have a normal
  default to fall back to.

- `#[debate(placeholder = "PLACEHOLDER")]`

  The `placeholder` is the name that is printed in usage messages for the value
  taken by this parameter. For instance, in `--min <MINIMUM>`, `MINIMUM` is the
  placeholder. By default, the placeholder is the field name in `ALL_CAPS`.

- `#[debate(invert [= "invert-flag"])]`

  An `invert` attribute creates an additional flag that "inverts" the original
  flag, making it as though it didn't appear at all.

  If no explicit flag name is given, the inverting flag will be the original
  flag, prefixed with `no-` (or, if it already has a `no-`, that `no-` will be
  stripped). Debate will do its best to preserve whatever case convention
  you're using.

  FINISH THIS EXAMPLE
  ```
  use debate::FromArgs
  #[derive(From)]
  struct
  ```

  `invert` can only be used with flags, not positional arguments.

- `#[debate(override)]`

  An `override` attribute allows a flag to appear more than once, where each
  subsequent instance of the flag overrides earlier instances. This is often
  used in shell scripts or shell aliases, where a default behavior provided
  by a script can be overridden by a user-provided flag.

  Without `override`, a type's [`Parameter`][parameter::Parameter] implementation
  will defune what happens if that flag appears more than once. For most types
  this will be an error, but some types (like `Vec`) allow a flag to appear
  more than once and parse all of the instances into a collection.

- `#[debate(flatten)]`

  A flattened field is a type that has its own `BuildFromArgs` implementation,
  allowing parsing to delegate arguments to the inner type. Most commonly this
  is used to embed a [subcommand](#subcommands) into a larger arguments
  structure.

  During parsing, any unrecognized flags will be forwarded to each `flatten`
  field in order, until one of them accepts the flag, and positional arguments
  will be forwarded to all positional fields and flattened fields in order,
  until one of them accepts the argument.

  When printing usage messages, fields contained in `flatten` field will be
  grouped together, rather than interspersed with the other parameters.

  `flatten` is exclusive with all other attributes.

### Subcommands

In order to make a subcommand, derive `BuildFromArgs` on an `enum` with
`#[debate(subcommand)]`. Each variant of the `enum` will be a different
subcommand, and each variant has its own set of parameters.

EXAMPLE HERE, INLCUDING NEWTYPES

### Flag Sets

`debate` deliberately doesn't provide attributes that establishe constraints
between separate flags, such as mutual exclusion, because these constraints
aren't reflected in the parsed value and require brittle and unpleasant code
to take advantage of. It prefers to **make impossible states unrepresentable**.

Instead, `debate` allows such constraints to be expressed in the form of enums
containing flags, where exactly one of the enum's variants can be selected.
This is called a Flag Set. Flag Sets can express mutual exclusion, A-requires-B,
and other types of constraints. To create a Flag Set, `derive(BuildFromArgs)`
on an `enum`.

#### Rules

- Unit variants, like `Switch` are interpreted as `--switch` flags, which don't
  take an  argument.
- Newtype variants, like `Value(u32)`, are interpreted as flags taking a value:
  `--value <VALUE>`.
- Struct variants, like `Opts { ... }`, treat each field in the struct as its
  own flag. Generally all fields

Flag Sets can only contain flags; they cannot contain positional parameters or
flattened fields.

Flags *may* appear in more than one variant of a flag set; this allows you
to express relationships resembling "`--volume <VOLUME>` requires `--loud`". If
a flag appears more than once, it must be identical in all instances (it can't
have `-v` in one place but `-n` in another, or be an `i32` in one place but
a `String` in another).

#### Selecting a variant

After all flags are parsed, `debate` will select a variant that matches *all*
of the recognized flags. If there is more than one such variant, they will be
tried in order (using the normal logic for absent flags), and the first such
variant used.

#### Flag Set Attributes

Flag set fields can have all the same attributes as flags. Because all flags
must have a `-s` short or `--long` form (or both), you may choose to add a
`#[debate(short)]` or `#[debate(long)]` (or both) to the `enum` itself, to
cause all fields to gain a short or a long form.

#### Examples

##### Mutual Exclusion

The most common use of a flag set is to express mutual exclusion. This enum
requires exactly one of `-r / --red`, `-g / --green`, or `-b / --blue`:

```
#[derive(FromArgs)]
#[debate(short, long)]
enum ColorSelection {
    Red,
    Green,
    Blue
}
```

##### A-requires-B

If one flag requires the existence of another flag, because it modifies that
root flag in some way, the flag can be added to multiple variants. This enum
requires either `--quiet`, `--loud`, or `--loud --volume <VOLUME>` (that is,
`--volume` *requires* `--loud`):

```
#[derive(FromArgs)]
#[debate(long)]
enum Sound {
    Quiet,
    Loud,
    Volume {
        loud: (),
        volume: u32,
    }
}
```

Note that it uses the `()` unit type to express a mandatory flag. In a `struct`
context this is pretty much useless (it expresses a flag that must be present
and contains no data), but in a Flag Set it provides a way to select over
different variants.

### Newtype Struct

When derived on a tuple struct with a single argument, [`BuildFromArgs`] will
delegate automatically to the [`BuildFromArgs`] implementation of the inner
type.

# Terminology

RAW ARGUMENTS PARAMETERS FLAGS POSITIONALS SWITCHES

# Principles

Debate prioritizes regularity and consistency in all aspects of its design. It
tries to avoid some of the common magical or ambiguous behaviors of other
command line argument parsers.

## Non-ambiguous parsing

Debate explicitly avoids many some argument parsing features when they give rise
to ambiguous and potentially surprising interpretation of the raw arguments
passed to the process. This means:

- Each argument is interpreted immediately as a flag (if it starts with `-` or
  `--`) or as a positional.
- Each flag *must* be either a switch, which takes no argument, or an option,
  which does. If a flag takes an argument, the next raw argument will
  unconditionally be used. A flag can be optional, but a present flag cannot
  optionally take an argument.
- Similarly, flags that take an argument can only take a *single* argument. A
  flag may appear more than once (`--feature foo --feature bar`), but a each
  instance of the flag can only take one argument. This prevents ambiguities
  in understanding whether a particular argument is intended as part of an
  earlier flag or as a different parameter.

## Trait-driven behavior

All of Debate's functionality is tied to traits. The derive macros do not have
any special-cased behavior tied to the *name* of a field type being something
like `bool` or `Option` or `Vec`; instead, the relevant [traits][crate::parameter]
are designed to accomodate any behavior a parameter type might care to express,
and the derives use these traits in unconditional ways. This ensures that type
aliases work flawlessly, and that any expressive power held by a built-in type
can be used in equal measure by a custom type with its own trait implementation.

## Type-driven invariants

Debate generally does not support adding an arbitrary nest of constraints and
validators to fields, as this leads to an equally arbitrary and equally brittle
nest of `unwrap`s and "this was checked by the argument parser" panics. Instead,
in keeping with the princples of **parse, don't validate** and **make impossible
states unrepresentable**, debate expects all possible constraints on your
argument parsing to be described in the type system.

## Compile-time specification

Debate doesn't have any equivelent of the `Command` type, which holds a runtime
representation of all the parameters or their properties. Instead, because the
entire picture of the parameters and types is available at compile time, it
operates directly on those types and on generated intermediary types. This
allows the parsing step to happen entirely without any allocating, and without
any unecessary fallibility in the implementation.

# Overview of argument parsing

`debate`, like all command-line argument parsers, has the task of converting
the native arguments passed to the process (an ordered list of strings) into
parsed and structured data useful to the program. It does this using the
conventional interpretation of some arguments as `--flags` or `positionals`,
sometimes called GNU-style arguments.

Each incoming raw argument is first indentified as either a `positional`
parameter or a `--flag` and matched to a particular field. If it is a positional
(that is, if it doesn't start with `-` or `--`), it is matched to the next
positional field; if it's a flag, it is matched to a specific field by name.
Either way,the raw argument is parsed into a value via the[`Parameter`][parameter::Parameter]
trait, using [`present`][parameter::Parameter::present] or [`arg`]

If a raw argument is precisely the string `--`, it is discarded, and all
subsequent raw arguments are unconditionally parsed as positionals.

## Specifics of flag handling

There are a few different ways to express flags, which allow for different
levels of succinctness and adapting to different shell conventions for token
splitting. `debate` understands "short flags" like `-c`, which start with a
single `-` and are only a single character, and long flags like `--count`,
which start with `--` and have a full string as a key. Flags can be switches,
meaning they don't take an argument (the mere presence of the flag is used
during parsing), or options, which do take an argument.

### Long flags

A long flag is a flag that starts with `--` and can take arguments in two
ways. If the flag has an `=`, such as `--count=10`, that is used as an argument
separator, with everything after the

If a flag takes an argument

The argument is parsed via the `Parameter`

# Concepts

## Parameters and Values

In `debate`, the simplest unit of data is the [`Parameter`][crate::parameter::Parameter].
A parameter is a piece of data associated with a field which can be parsed from
0 or more (but usually 1) raw arguments. Parameters are primitives like
booleans, strings, integers, collections like `Vec`, or wrappers like `Option`.

Parameters have the key property that they don't care in what "form" the data
arrives (that is, whether it's a flag or a positional argument).

## Values

A [`Value`][crate::parameter::Value] is a specific, common kind of `Parameter`,
which takes exactly one command-line argument.

## `BuildFromArgs`

Parameters are grouped together into sets of

## The `'arg` lifetime

`debate` supports borrowing parsed

## `FromArgs`

Debate groups parameters together into structures that implement the ``


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

pub use debate_derive::{BuildFromArgs, ParameterUsage, Usage, Value, main};
pub use debate_parser::Arg;

pub use crate::{
    build::BuildFromArgs,
    help::{ParameterUsage, Usage},
    parameter::Value,
};

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
