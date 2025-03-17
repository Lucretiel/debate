use core::{
    fmt::Display,
    str::{self, FromStr},
};

use debate_parser::Arg;

use crate::util::arg_as_str;

/**
A required [`Parameter`] was absent from the command-line arguments.

This type contains no data because a parameter type doesn't have any additional
context about the field it's associated with. Generally a [`RequiredError`]
will trigger a call to [`Error::required`][crate::from_args::Error::required],
which includes more context.
*/

// TODO: error implementation here
#[derive(Debug, Clone, Copy)]
pub struct RequiredError;

/// Error for things that can go wrong in a `Parameter` implementation
pub trait Error<'arg> {
    /// The argument requires a value, and none was provided
    fn needs_arg() -> Self;

    /// The argument must NOT have a value, and got one
    fn got_arg(argument: Arg<'arg>) -> Self;

    /// The argument appeared more times than expected on the command line
    fn got_additional_instance() -> Self;

    /// The argument wasn't valid UTF-8 and should be
    fn invalid_utf8(argument: Arg<'arg>) -> Self;

    /// The argument was valid UTF-8, but it failed to parse into an instance
    /// of the type
    fn parse_error(argument: &str, message: impl Display) -> Self;

    /// The argument failed to parse into an instance of the type.
    fn byte_parse_error(argument: Arg<'arg>, message: impl Display) -> Self;

    /// Something else went wrong
    fn custom(message: impl Display) -> Self;
}

pub trait ArgAccess<'arg> {
    fn with<T, E>(self, op: impl FnOnce(Arg<'arg>) -> Result<T, E>) -> Result<T, E>
    where
        E: Error<'arg>;
}

/**
A parameter is a type that can be parsed from one or more command line
arguments, flags, or options.

The parameter trait allows types to operate independently of the field or
type of argument (option vs flag vs positional). It is the "type" part of the
command line argument, responsible for requesting arguments (if appropriate)
and parsing them into the underlying type.

Generally a type should have a consistent behavior; that is, it should either
ALWAYS or NEVER accept arguments. This ensures that the parse behavior is as
consistent as possible, with no weird ambiguities about options vs arguments.

For most types it makes more sense to implement [`Value`] or [`RawValue`]
instead of [`Parameter`]. A type should only implement [`Parameter`] if it
wants to act as a flag that takes no arguments, or it wants to customize its
behavior if it appears less than or more than once on the command line.
*/
pub trait Parameter<'arg>: Sized {
    /**
    This parameter was absent from the command line.

    Most types should return a [`RequiredError`] here (the default behavior),
    and allow defaults to be handled by an [`Option`] or `#[debate(default)]`.
    However, there are plenty of cases where a type has a sensible behavior if
    it doesn't appear on the command line, such as a bool flag being false or
    a [`Vec`] being empty.
    */
    #[inline]
    fn absent() -> Result<Self, RequiredError> {
        Err(RequiredError)
    }

    /**
    This parameter got an argument from the command line.

    Types that operate as flags (such as `--verbose`) should return an error
    in this case, because only options should accept arguments.

    If you're implementing this method, it probably makes more sense to
    implement [`PositionalParameter`] instead.
    */
    #[inline]
    fn arg<E: Error<'arg>>(argument: Arg<'arg>) -> Result<Self, E> {
        Err(E::got_arg(argument))
    }

    /**
    This parameter was present on the command line.

    The parameter can choose to get an argument by calling `argument.with()`,
    if that's appropriate for this type. Flags like `--verbose` should not do
    this, whereas options like `--output-file ./foo.txt` should do this.
    */
    fn present<E: Error<'arg>>(argument: impl ArgAccess<'arg>) -> Result<Self, E>;

    /**
    This parameter appeared more than once on the command line, with an
    argument.

    Most parameters should return an error here. However, some types (like
    counters and [`Vec`]) can collect more than one instance together.
    */
    fn add_arg<E: Error<'arg>>(&mut self, argument: Arg<'arg>) -> Result<(), E> {
        Err(E::got_arg(argument))
    }

    /**
    This parameter appeared more than once on the command line.

    Most parameters should return an error here. However, some types (like
    counters and [`Vec`]) can collect more than one instance together.
    */
    fn add_present<E: Error<'arg>>(&mut self, argument: impl ArgAccess<'arg>) -> Result<(), E>;
}

/// A parameter that can be used as a positional parameter (as opposed to a
/// --option), which means that it MUST take an argument.
pub trait PositionalParameter<'arg>: Sized {
    /**
    This parameter was absent from the command line.

    Most types should return a [`RequiredError`] here (the default behavior),
    and allow defaults to be handled by an [`Option`] or `#[debate(default)]`.
    However, there are plenty of cases where a type has a sensible behavior if
    it doesn't appear on the command line, such as a bool flag being false or
    a [`Vec`] being empty
    */
    fn absent() -> Result<Self, RequiredError> {
        Err(RequiredError)
    }

    /**
    This parameter got an argument from the command line.
    */
    fn arg<E: Error<'arg>>(argument: Arg<'arg>) -> Result<Self, E>;

    /**
    This parameter appeared more than once on the command line, with an
    argument.

    Most parameters should return an error here. However, some types (like
    counters and [`Vec`]) can collect more than one instance together.
    */
    fn add_arg<E: Error<'arg>>(&mut self, argument: Arg<'arg>) -> Result<(), E>;
}

impl<'arg, T: PositionalParameter<'arg>> Parameter<'arg> for T {
    #[inline]
    fn absent() -> Result<Self, RequiredError> {
        PositionalParameter::absent()
    }

    #[inline]
    fn arg<E: Error<'arg>>(argument: Arg<'arg>) -> Result<Self, E> {
        PositionalParameter::arg(argument)
    }

    #[inline]
    fn present<E: Error<'arg>>(argument: impl ArgAccess<'arg>) -> Result<Self, E> {
        argument.with(PositionalParameter::arg)
    }

    #[inline]
    fn add_arg<E: Error<'arg>>(&mut self, argument: Arg<'arg>) -> Result<(), E> {
        PositionalParameter::add_arg(self, argument)
    }

    #[inline]
    fn add_present<E: Error<'arg>>(&mut self, argument: impl ArgAccess<'arg>) -> Result<(), E> {
        argument.with(|arg| PositionalParameter::add_arg(self, arg))
    }
}

// TODO: improved version of parameter that allows for stateful, in-progress
// builders. This allows us to support things like arrays, if we want.

/**
Parameters that must appear exactly once and take an argument.

Types that implement [`Value`] automatically implement [`PositionalParameter`]
and [`Parameter`] such that they return an error if they are absent, don't
receive an argument, or are present more than once.
*/
pub trait Value<'arg>: Sized {
    /// Parse a `Value` from an [`Arg`] given on the command line
    fn from_arg<E: Error<'arg>>(arg: Arg<'arg>) -> Result<Self, E>;
}

impl<'arg, T> PositionalParameter<'arg> for T
where
    T: Value<'arg>,
{
    #[inline]
    fn arg<E: Error<'arg>>(argument: Arg<'arg>) -> Result<Self, E> {
        Value::from_arg(argument)
    }

    #[inline]
    fn add_arg<E: Error<'arg>>(&mut self, _arg: Arg<'arg>) -> Result<(), E> {
        Err(E::got_additional_instance())
    }
}

/// For types with a [`FromStr`] implementation, [`ParsedValue`] automatically
/// gives them a [`Parameter`] implementation so that they can be used as
/// command line argument.
pub trait ParsedValue: FromStr {}

impl<'arg, T> Value<'arg> for T
where
    T: ParsedValue,
    T::Err: Display,
{
    fn from_arg<E: Error<'arg>>(arg: Arg<'arg>) -> Result<Self, E> {
        let arg = arg_as_str(arg)?;
        arg.parse().map_err(|err| E::parse_error(arg, err))
    }
}
