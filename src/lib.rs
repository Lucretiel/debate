//#![no_std]

use core::{
    fmt::Display,
    str::{self, FromStr},
};

use primitives::{Arg, ArgAccess};

pub mod primitives;

#[doc(hidden)]
pub mod util;

pub trait ParameterError {
    /// The argument is required, and was absent on the command line
    fn required() -> Self;

    /// The argument appeared more than once on the command line, and shouldn't have
    fn got_additional_instance() -> Self;

    /// The argument requires a value, and none was provided
    fn needs_arg() -> Self;

    /// The argument must NOT have a value, and got one
    fn got_arg(arg: Arg<'_>) -> Self;

    /// The argument wasn't valid UTF-8 and should be
    fn invalid_utf8(arg: Arg<'_>) -> Self;

    /// The argument was valid UTF-8, but it failed to parse into an instance
    /// of the type
    fn parse_error(arg: &str, msg: impl Display) -> Self;

    /// Something else went wrong
    fn custom(msg: impl Display) -> Self;
}

pub trait Parameter<'arg>: Sized {
    fn absent<E: ParameterError>() -> Result<Self, E>;
    fn arg<E: ParameterError>(arg: Arg<'arg>) -> Result<Self, E>;
    fn present<E: ParameterError>(arg: impl ArgAccess<'arg>) -> Result<Self, E>;

    fn add_arg<E: ParameterError>(self, arg: Arg<'arg>) -> Result<Self, E>;
    fn add_present<E: ParameterError>(self, arg: impl ArgAccess<'arg>) -> Result<Self, E>;
}

pub trait Value<'arg>: Sized {
    fn from_arg<E: ParameterError>(arg: Arg<'arg>) -> Result<Self, E>;
}

impl<'arg, T> Parameter<'arg> for T
where
    T: Value<'arg>,
{
    fn absent<E: ParameterError>() -> Result<Self, E> {
        Err(E::required())
    }

    fn arg<E: ParameterError>(arg: Arg<'arg>) -> Result<Self, E> {
        T::from_arg(arg)
    }

    fn present<E: ParameterError>(arg: impl ArgAccess<'arg>) -> Result<Self, E> {
        T::from_arg(arg.take_argument().ok_or_else(|| E::needs_arg())?)
    }

    fn add_arg<E: ParameterError>(self, _arg: Arg<'arg>) -> Result<Self, E> {
        Err(E::got_additional_instance())
    }

    fn add_present<E: ParameterError>(self, _arg: impl ArgAccess<'arg>) -> Result<Self, E> {
        Err(E::got_additional_instance())
    }
}

pub struct Count {
    count: u16,
}

impl<'arg> Parameter<'arg> for Count {
    fn absent<E: ParameterError>() -> Result<Self, E> {
        Ok(Self { count: 0 })
    }

    fn arg<E: ParameterError>(arg: Arg<'arg>) -> Result<Self, E> {
        Err(E::got_arg(arg))
    }

    fn present<E: ParameterError>(_arg: impl ArgAccess<'arg>) -> Result<Self, E> {
        Ok(Self { count: 1 })
    }

    fn add_arg<E: ParameterError>(self, arg: Arg<'arg>) -> Result<Self, E> {
        Err(E::got_arg(arg))
    }

    fn add_present<E: ParameterError>(self, _arg: impl ArgAccess<'arg>) -> Result<Self, E> {
        Ok(Self {
            count: self.count + 1,
        })
    }
}

pub trait FromStrValue: FromStr {}

impl<'arg, T> Value<'arg> for T
where
    T: FromStrValue,
    T::Err: Display,
{
    fn from_arg<E: ParameterError>(arg: Arg<'arg>) -> Result<Self, E> {
        let arg = str::from_utf8(arg.bytes()).map_err(|_| E::invalid_utf8(arg))?;
        arg.parse().map_err(|err| E::parse_error(arg, err))
    }
}

pub trait FlagError {
    type ValueError: ParameterError;

    /// A positional parameter had an error
    fn positional(error: Self::ValueError) -> Self;

    /// A long option had an error
    fn long(option: Arg<'_>, error: Self::ValueError) -> Self;

    /// A short option had an error
    fn short(option: u8, error: Self::ValueError) -> Self;

    /// Got a long argument that we didn't recognize
    fn unrecognized_long(option: Arg<'_>, argument: Option<Arg<'_>>) -> Self;

    /// Got a short argument that we didn't recognize
    fn unrecognized_short(option: u8) -> Self;

    /// Got a positional argument that we didn't recognize
    fn unrecognized_positional(arg: Arg<'_>) -> Self;

    // TODO: rejected forms. Used for composability.
}

pub trait FromArgs<'arg>: Sized {
    fn from_args<I, E>(args: primitives::Arguments<'arg, I>) -> Result<Self, E>
    where
        I: Iterator<Item = &'arg [u8]>,
        E: FlagError;
}
