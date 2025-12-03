/*!
Traits enabling individual parameter parsing.

In [`debate`][crate], a [`Parameter`] is any type parsed from (usually one)
command line argument, such as a string or integer. The trait allows these
primitive types to be parsed without consideration for whether they appear
as a `[POSITIONAL]` or `--flag=FLAG`.

The traits in this module form a hierarchy, with more specific traits being
easier to implement and enforcing common patterns, such as requiring arguments
or only appearing once. Each trait in the hierarchy automatically implements
the trait(s) below it.

The base [`Parameter`] trait applies to ANY parameter. It allows parameters
that are omitted, those that appear once or more than once, and those that
accept or deny arguments. Typically you only need to implement [`Parameter`]
for switches and switch-like types: `--flags` that don't accept arguments. It's
always preferable to implement [`PositionalParameter`] for types that accept
arguments.

The [`PositionalParameter`] trait applies to parameters that expect arguments,
such as positional parameters or `--flag=argument`. It still permits arguments
to be omitted, or to appear more than once. Individual types can enforce more
specific behaviors by implementing the relevant methods.

The [`Value`] trait is the most common: it applies to parameters that expect
*precisely one* argument; no more and no less. This applies to the common
primitive types like strings, paths, integers, and so on; a vast majority of
parameters are values. For this reason, [`Value`] is also used as a common
constraint for containers and wrappers; for instance, [`Option`] is a
[`PositionalParameter`], because it accepts 0 or 1 arguments, but it wraps
a [`Value`], since the inner type *must* accept an argument.

Finally, the [`ParsedValue`] trait is an empty utility trait that creates a
[`Value`] implementation from a [`FromStr`] implementation. This is an easy
way for you to add a [`Value`] implementation for your custom types, so long
as they implement [`FromStr`].

This module also defines an [`Error`] trait, allowing parameters to express
parsing or usage errors.
*/

use core::{
    error::Error as StdError,
    fmt::{self, Display, Formatter},
    str::{self, FromStr},
};

use debate_parser::Arg;

use crate::util::arg_as_str;

/**
A required [`Parameter`] was absent from the command-line arguments.

This type contains no data because a parameter type doesn't have any additional
context about the field it's associated with. Generally a [`RequiredError`]
will trigger a [build error][crate::build::Error] to be created, which will
include more information about the field (such as its `--flag``).

This is a separate error from the [`Error`] trait, because it can only be
produced by [`Parameter::absent`], and there's no reason for
[`absent`][Parameter::absent] to ever produce any *other* error.
*/

#[derive(Debug, Clone, Copy)]
pub struct RequiredError;

impl Display for RequiredError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "command-line parameter required an argument")
    }
}

impl StdError for RequiredError {
    fn description(&self) -> &str {
        "command-line parameter required an argument"
    }
}

/**
Error for things that can go wrong in a `Parameter` implementation when trying
to parse an argument from the command line
*/
pub trait Error<'arg> {
    /// The parameter requires an argument, and none was provided.
    fn needs_arg() -> Self;

    /// The parameter is a `--switch`; it must NOT have a value, and got one.
    fn got_arg(argument: &'arg Arg) -> Self;

    /**
    The argument appeared more times than expected (usually more than once) on
    the command line
    */
    fn got_additional_instance() -> Self;

    /// The argument wasn't valid UTF-8 and should be.
    fn invalid_utf8(argument: &'arg Arg) -> Self;

    /**
    The argument should be one of the given values (or, possibly, a case
    insensitive variation)
    */
    fn should_be(argument: &'arg Arg, expected: &'static [&'static str]) -> Self;

    /**
    The argument was valid UTF-8, but it failed to parse into an instance of
    the type.
    */
    fn parse_error(argument: &'arg str, message: impl Display) -> Self;

    /**
    The argument failed to parse into an instance of the type.
    */
    fn byte_parse_error(argument: &'arg Arg, message: impl Display) -> Self;

    /// Something else went wrong
    fn custom(message: impl Display) -> Self;
}

/**
Trait enabling conditional access to argument values when parsing the command
line arguments.

When parsing flags from the command line, the low-level parser can't know in
general whether a given argument is a standalone value, or whether it's an
argument to an earlier flag. For instance, given `--foo bar`, `bar` could be
a positional parameter, or it could be an argument to `--foo`, depending on
`--foo`'s type. This trait allows a [`Parameter`] implementation to take an
argument, if required, or to simply exist as a switch, and leave the next raw
command line argument to be parsed on its own.
*/
pub trait ArgAccess<'arg> {
    /**
    Call `with` if and only if this [`Parameter`] type expects an argument. `op`
    will be called with the argument, if any is available, and propagate the
    result; it will return an [`Error::needs_arg`] if no argument was found.

    This method uses a callback rather than simply returning the argument to
    help enforce the requirement that a [`Parameter`] should either *always*
    expect arguments or *never* expect arguments; it shouldn't be able to
    behave in different non-error ways depending on the presence or absence of
    an argument.
    */
    fn with<T, E>(self, op: impl FnOnce(&'arg Arg) -> Result<T, E>) -> Result<T, E>
    where
        E: Error<'arg>;
}

/**
A parameter is a type that can be parsed from one or more command line
arguments and flags and so on.

The parameter trait allows types to operate independently of the field or
type of argument (option vs flag vs positional). It is the "type" part of the
command line argument, responsible for requesting arguments (if appropriate)
and parsing them into the underlying type.

A type should have a consistent behavior with regard to taking values; that is,
it should either *always* or *never* accept arguments. This ensures that the parse
behavior is as consistent as possible, with no weird ambiguities about flags
vs positionals vs switches.

This is the lowest-level parameter trait, with the broadest range of
capabilities; usually it makes more sense to implement [`Value`] (for types
that take a value and appear exactly once) or [`PositionalParameter`] (for
types that take an argument, but may appear 0 or more times).
*/
pub trait Parameter<'arg>: Sized {
    /**
    This parameter was absent from the command line.

    Most types should return a [`RequiredError`] here (the default behavior),
    and allow defaults to be handled by an [`Option`] or `#[debate(default)]`.
    However, there are plenty of cases where a type has a sensible behavior if
    it doesn't appear on the command line, such as a [`bool`] flag being false or
    a [`Vec`][std::vec::Vec] being empty.
    */
    #[inline]
    fn absent() -> Result<Self, RequiredError> {
        Err(RequiredError)
    }

    /**
    This parameter got an argument from the command line.

    Types that operate as switches (such as `--verbose`) should return an error
    in this case.

    If you're implementing this method, it probably makes more sense to
    implement [`PositionalParameter`] instead of [`Parameter`].
    */
    #[inline]
    fn arg<E: Error<'arg>>(argument: &'arg Arg) -> Result<Self, E> {
        Err(E::got_arg(argument))
    }

    /**
    This parameter appeared on the command line.

    The parameter can choose to get an argument by calling `argument.with()`,
    if that's appropriate for this type. Flags like `--verbose` should not do
    this, whereas options like `--output-file ./foo.txt` should do this.
    */
    fn present<E: Error<'arg>>(argument: impl ArgAccess<'arg>) -> Result<Self, E>;

    /**
    This parameter appeared an additional time on the command line, with an
    argument.

    Most parameters should return an error here. However, some types (like
    counters and [`Vec`][std::vec::Vec]) can collect more than one instance
    together.

    Additionally, switches should return an error unconditionally here, since
    switches don't take arguments.
    */
    #[inline]
    fn add_arg<E: Error<'arg>>(&mut self, argument: &'arg Arg) -> Result<(), E> {
        Err(E::got_arg(argument))
    }

    /**
    This parameter appeared more than once on the command line.

    Most parameters should return an error here. However, some types (like
    counters and [`Vec`][std::vec::Vec]) can collect more than one instance
    together.
    */
    fn add_present<E: Error<'arg>>(&mut self, argument: impl ArgAccess<'arg>) -> Result<(), E>;
}

/**
A parameter that can be used as a positional parameter (as opposed to a
`--switch`), which means that it MUST take an argument. A positional
parameter may appear 0 or more times on the command line (depending on its
specific implementation); for types that should appear exactly once (like
integers and strings), you should implement [`Value`] instead.

Note that, despite the name, a [`PositionalParameter`] *can* be used as a
`--flag` type. This type expresses any parameters that expect arguments.

Types that implement [`PositionalParameter`] automatically implement
[`Parameter`].
*/
pub trait PositionalParameter<'arg>: Sized {
    /**
    This parameter was absent from the command line.

    Most types should return a [`RequiredError`] here (the default behavior),
    and allow defaults to be handled by an [`Option`] or `#[debate(default)]`.
    However, there are plenty of cases where a type has a sensible behavior if
    it doesn't appear on the command line, as a `Vec` being empty.
    */
    #[inline]
    fn absent() -> Result<Self, RequiredError> {
        Err(RequiredError)
    }

    /**
    This parameter appeared on the command line, with an argument.
    */
    fn arg<E: Error<'arg>>(argument: &'arg Arg) -> Result<Self, E>;

    /**
    This parameter appeared more than once on the command line, with an
    argument.

    Most parameters should return an error here. However, some types (like
    [`Vec`][std::vec::Vec]) can collect more than one instance together.
    */
    fn add_arg<E: Error<'arg>>(&mut self, argument: &'arg Arg) -> Result<(), E>;
}

impl<'arg, T: PositionalParameter<'arg>> Parameter<'arg> for T {
    #[inline]
    fn absent() -> Result<Self, RequiredError> {
        PositionalParameter::absent()
    }

    #[inline]
    fn arg<E: Error<'arg>>(argument: &'arg Arg) -> Result<Self, E> {
        PositionalParameter::arg(argument)
    }

    #[inline]
    fn present<E: Error<'arg>>(argument: impl ArgAccess<'arg>) -> Result<Self, E> {
        argument.with(PositionalParameter::arg)
    }

    #[inline]
    fn add_arg<E: Error<'arg>>(&mut self, argument: &'arg Arg) -> Result<(), E> {
        PositionalParameter::add_arg(self, argument)
    }

    #[inline]
    fn add_present<E: Error<'arg>>(&mut self, argument: impl ArgAccess<'arg>) -> Result<(), E> {
        argument.with(|arg| PositionalParameter::add_arg(self, arg))
    }
}

/**
Parameters that must appear exactly once and take an argument.

Types that implement [`Value`] automatically implement [`PositionalParameter`]
and [`Parameter`] such that they return an error if they are absent, don't
receive an argument, or are present more than once.

[`Value`] is the typical constraint for containers and wrappers, like [`Option`]
and [`Vec`][std::vec::Vec]. This is because each value *within* the container
needs to be a value that appeared exactly once.
*/
pub trait Value<'arg>: Sized {
    /// Parse a `Value` from an [`Arg`] given on the command line
    fn from_arg<E: Error<'arg>>(arg: &'arg Arg) -> Result<Self, E>;
}

impl<'arg, T> PositionalParameter<'arg> for T
where
    T: Value<'arg>,
{
    #[inline]
    fn arg<E: Error<'arg>>(argument: &'arg Arg) -> Result<Self, E> {
        Value::from_arg(argument)
    }

    #[inline]
    fn add_arg<E: Error<'arg>>(&mut self, _arg: &'arg Arg) -> Result<(), E> {
        Err(E::got_additional_instance())
    }
}

/**
For types with a [`FromStr`] implementation, [`ParsedValue`] automatically
gives them a [`Value`] implementation so that they can be used as command line
argument.

# Example

```
use debate::parameter::{Parameter, ParsedValue, errors::EmptyError};

struct ColonPair {
    left: String,
    right: String,
}

impl FromStr for ColonPair {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, String> {
        input
            .split_once(":")
            .map(|(left, right)| Self {
                left: left.to_owned(),
                right: right.to_owned(),
            })
            .ok_or_else(|| "input didn't have a colon".to_owned())
    }
}

impl ParsedValue for ColonPair {}

// TODO: write an assertion. This needs an implementation of parameter::Error.
```
*/
pub trait ParsedValue: FromStr {}

impl<'arg, T> Value<'arg> for T
where
    T: ParsedValue,
    T::Err: Display,
{
    fn from_arg<E: Error<'arg>>(arg: &'arg Arg) -> Result<Self, E> {
        let arg = arg_as_str(arg)?;
        arg.parse().map_err(|err| E::parse_error(arg, err))
    }
}
