use debate_parser::{Arg, ArgAccess};

use crate::{FromStrValue, Parameter, StrValue, Value, error::ParameterError};

macro_rules! from_str {
    ($(
        $(#[cfg($($cfg:tt)*)])*
        $type:ident $($(::$path:ident)*,)?
    )*) => {
        $(
            $(#[cfg($($cfg)*)])*
            impl FromStrValue for $type $($(:: $path)*)? {}
        )*
    };
}

from_str! {
    u8 u16 u32 u64 u128
    i8 i16 i32 i64 i128
    f32 f64

    #[cfg(feature="std")]
    std::string::String,

    #[cfg(feature="std")]
    std::path::PathBuf,
}

impl<'arg> StrValue<'arg> for &'arg str {
    fn from_arg<E: ParameterError>(arg: &'arg str) -> Result<Self, E> {
        Ok(arg)
    }
}

#[cfg(feature = "std")]
impl<'arg> StrValue<'arg> for &'arg std::path::Path {
    fn from_arg<E: ParameterError>(arg: &'arg str) -> Result<Self, E> {
        Ok(std::path::Path::new(arg))
    }
}

impl<'arg> Parameter<'arg> for bool {
    fn absent<E: ParameterError>() -> Result<Self, E> {
        Ok(false)
    }

    fn arg<E: ParameterError>(arg: Arg<'arg>) -> Result<Self, E> {
        Err(E::got_arg(arg))
    }

    fn present<E: ParameterError>(_arg: impl ArgAccess<'arg>) -> Result<Self, E> {
        Ok(true)
    }

    fn add_arg<E: ParameterError>(self, _arg: Arg<'arg>) -> Result<Self, E> {
        Err(E::got_additional_instance())
    }

    fn add_present<E: ParameterError>(self, _arg: impl ArgAccess<'arg>) -> Result<Self, E> {
        Err(E::got_additional_instance())
    }
}

impl<'arg, T> Parameter<'arg> for Option<T>
where
    T: Value<'arg>,
{
    fn absent<E: ParameterError>() -> Result<Self, E> {
        Ok(None)
    }

    fn arg<E: ParameterError>(arg: Arg<'arg>) -> Result<Self, E> {
        T::from_arg(arg).map(Some)
    }

    fn present<E: ParameterError>(arg: impl ArgAccess<'arg>) -> Result<Self, E> {
        Self::arg(arg.take().ok_or_else(|| E::needs_arg())?)
    }

    fn add_arg<E: ParameterError>(self, _arg: Arg<'arg>) -> Result<Self, E> {
        Err(E::got_additional_instance())
    }

    fn add_present<E: ParameterError>(self, _arg: impl ArgAccess<'arg>) -> Result<Self, E> {
        Err(E::got_additional_instance())
    }
}

#[cfg(feature = "std")]
impl<'arg, T> Parameter<'arg> for std::vec::Vec<T>
where
    T: Value<'arg>,
{
    fn absent<E: ParameterError>() -> Result<Self, E> {
        Ok(std::vec::Vec::new())
    }

    fn arg<E: ParameterError>(arg: Arg<'arg>) -> Result<Self, E> {
        T::from_arg(arg).map(|value| std::vec::Vec::from([value]))
    }

    fn present<E: ParameterError>(arg: impl ArgAccess<'arg>) -> Result<Self, E> {
        Self::arg(arg.take().ok_or_else(|| E::needs_arg())?)
    }

    fn add_arg<E: ParameterError>(mut self, arg: Arg<'arg>) -> Result<Self, E> {
        T::from_arg(arg).map(|value| {
            self.push(value);
            self
        })
    }

    fn add_present<E: ParameterError>(self, arg: impl ArgAccess<'arg>) -> Result<Self, E> {
        Self::add_arg(self, arg.take().ok_or_else(|| E::needs_arg())?)
    }
}
