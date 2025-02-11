//#![no_std]

use core::{
    fmt::Display,
    str::{self, FromStr},
};

use primitives::{Arg, ArgAccess};

pub mod error;
pub mod primitives;

mod impls;
#[doc(hidden)]
pub mod util;

use error::{FlagError, ParameterError};

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
    fn from_args<I, E>(args: primitives::Arguments<'arg, I>) -> Result<Self, E>
    where
        I: Iterator<Item = &'arg [u8]>,
        E: FlagError;
}
