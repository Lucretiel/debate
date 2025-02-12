#![no_std]

#[cfg(feature = "std")]
extern crate std;

pub mod error;
mod impls;

use core::{
    fmt::Display,
    str::{self, FromStr},
};

use debate_parser::{Arg, ArgAccess, ArgumentsParser};

use crate::error::{FlagError, ParameterError};

pub trait Parameter<'arg>: Sized {
    fn absent<E: ParameterError>() -> Result<Self, E>;
    fn arg<E: ParameterError>(arg: Arg<'arg>) -> Result<Self, E>;
    fn present<E: ParameterError>(arg: impl ArgAccess<'arg>) -> Result<Self, E>;

    fn add_arg<E: ParameterError>(self, arg: Arg<'arg>) -> Result<Self, E>;
    fn add_present<E: ParameterError>(self, arg: impl ArgAccess<'arg>) -> Result<Self, E>;
}

// TODO: improved version of parameter that allows for stateful, in-progress
// builders. This allows us to support things like arrays, if we want, and
// also makes the behavior around `absent` MUCH better.

/// Parameters that MUST always have a single argument. Types that implement
/// `Value` automatically implement `Parameter` such that they return an error
/// if they are absent, don't receive an argument, or are present more than
/// once
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
        T::from_arg(arg.take().ok_or_else(|| E::needs_arg())?)
    }

    fn add_arg<E: ParameterError>(self, _arg: Arg<'arg>) -> Result<Self, E> {
        Err(E::got_additional_instance())
    }

    fn add_present<E: ParameterError>(self, _arg: impl ArgAccess<'arg>) -> Result<Self, E> {
        Err(E::got_additional_instance())
    }
}

/// Parameters that MUST always have a single argument, which should be a `str`.
/// Types that implement `StrValue` automatically implement `Parameter` such
/// that they return an error if they are absent, don't receive an argument,
/// are present more than once, or the argument they receive isn't a valid
/// UTF-8 str.
pub trait StrValue<'arg>: Sized {
    fn from_arg<E: ParameterError>(arg: &'arg str) -> Result<Self, E>;
}

impl<'arg, T> Value<'arg> for T
where
    T: StrValue<'arg>,
{
    fn from_arg<E: ParameterError>(arg: Arg<'arg>) -> Result<Self, E> {
        StrValue::from_arg(str::from_utf8(arg.bytes()).map_err(|_| E::invalid_utf8(arg))?)
    }
}

pub trait FromStrValue: FromStr {}

impl<'arg, T> StrValue<'arg> for T
where
    T: FromStrValue,
    T::Err: Display,
{
    fn from_arg<E: ParameterError>(arg: &'arg str) -> Result<Self, E> {
        arg.parse().map_err(|err| E::parse_error(arg, err))
    }
}

pub trait FromArgs<'arg>: Sized {
    fn from_args<I, E>(args: ArgumentsParser<'arg, I>) -> Result<Self, E>
    where
        I: Iterator<Item = &'arg [u8]>,
        E: FlagError;
}
