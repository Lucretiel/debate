use core::{
    fmt::Display,
    str::{self, FromStr},
};

use debate_parser::{Arg, ArgAccess};

#[derive(Debug, Clone, Copy)]
pub struct RequiredError;

pub trait Parameter<'arg>: Sized {
    fn absent() -> Result<Self, RequiredError>;

    fn arg<E: Error<'arg>>(arg: Arg<'arg>) -> Result<Self, E>;
    fn present<E: Error<'arg>>(arg: impl ArgAccess<'arg>) -> Result<Self, E>;

    fn add_arg<E: Error<'arg>>(self, arg: Arg<'arg>) -> Result<Self, E>;
    fn add_present<E: Error<'arg>>(self, arg: impl ArgAccess<'arg>) -> Result<Self, E>;
}

/// Error for things that can go wrong in a `Parameter` implementation
pub trait Error<'arg> {
    /// The argument requires a value, and none was provided
    fn needs_arg() -> Self;

    /// The argument must NOT have a value, and got one
    fn got_arg(arg: Arg<'arg>) -> Self;

    /// The argument appeared more than once on the command line, and shouldn't have
    fn got_additional_instance() -> Self;

    /// The argument wasn't valid UTF-8 and should be
    fn invalid_utf8(arg: Arg<'arg>) -> Self;

    /// The argument was valid UTF-8, but it failed to parse into an instance
    /// of the type
    fn parse_error(arg: &str, msg: impl Display) -> Self;

    /// The argument failed to parse into an instance of the type.
    fn byte_parse_error(arg: Arg<'arg>, msg: impl Display) -> Self;

    /// Something else went wrong
    fn custom(msg: impl Display) -> Self;
}

// TODO: improved version of parameter that allows for stateful, in-progress
// builders. This allows us to support things like arrays, if we want, and
// also makes the behavior around `absent` MUCH better.

/// Parameters that MUST always have a single argument. Types that implement
/// `Value` automatically implement `Parameter` such that they return an error
/// if they are absent, don't receive an argument, or are present more than
/// once
pub trait Value<'arg>: Sized {
    fn from_arg<E: Error<'arg>>(arg: Arg<'arg>) -> Result<Self, E>;
}

impl<'arg, T> Parameter<'arg> for T
where
    T: Value<'arg>,
{
    #[inline]
    fn absent() -> Result<Self, RequiredError> {
        Err(RequiredError)
    }

    #[inline]
    fn arg<E: Error<'arg>>(arg: Arg<'arg>) -> Result<Self, E> {
        T::from_arg(arg)
    }

    #[inline]
    fn present<E: Error<'arg>>(arg: impl ArgAccess<'arg>) -> Result<Self, E> {
        T::from_arg(arg.take().ok_or_else(|| E::needs_arg())?)
    }

    #[inline]
    fn add_arg<E: Error<'arg>>(self, _arg: Arg<'arg>) -> Result<Self, E> {
        Err(E::got_additional_instance())
    }

    #[inline]
    fn add_present<E: Error<'arg>>(self, _arg: impl ArgAccess<'arg>) -> Result<Self, E> {
        Err(E::got_additional_instance())
    }
}

/// Parameters that MUST always have a single argument, which should be a `str`.
/// Types that implement `StrValue` automatically implement `Parameter` such
/// that they return an error if they are absent, don't receive an argument,
/// are present more than once, or the argument they receive isn't a valid
/// UTF-8 str.
pub trait StrValue<'arg>: Sized {
    fn from_arg<E: Error<'arg>>(arg: &'arg str) -> Result<Self, E>;
}

impl<'arg, T> Value<'arg> for T
where
    T: StrValue<'arg>,
{
    #[inline]
    fn from_arg<E: Error<'arg>>(arg: Arg<'arg>) -> Result<Self, E> {
        StrValue::from_arg(str::from_utf8(arg.bytes()).map_err(|_| E::invalid_utf8(arg))?)
    }
}

/// For types with a `FromStr` implementation, `FromStrValue` automatically
/// gives them a `Parameter` implementation so that they can be used as
/// command line argument.
pub trait FromStrValue: FromStr {}

impl<'arg, T> StrValue<'arg> for T
where
    T: FromStrValue,
    T::Err: Display,
{
    #[inline]
    fn from_arg<E: Error<'arg>>(arg: &'arg str) -> Result<Self, E> {
        arg.parse().map_err(|err| E::parse_error(arg, err))
    }
}
