use debate_parser::Arg;

use crate::help::{ParameterUsage, ParameterValueKind, Repetition, Requirement};
use crate::parameter::{
    ArgAccess, Error as ParameterError, Parameter, ParsedValue, PositionalParameter, RequiredError,
    Value,
};
use crate::util::arg_as_str;

macro_rules! from_str {
    ($(
        $(#[cfg($($cfg:tt)*)])*
        $type:ident $($(::$path:ident)*,)?
    )*) => {
        $(
            $(#[cfg($($cfg)*)])*
            impl ParsedValue for $type $($(:: $path)*)? {}

            $(#[cfg($($cfg)*)])*
            impl ParameterUsage for $type $($(:: $path)*)? {
                const VALUE: ParameterValueKind = ParameterValueKind::Value;
                const REQUIREMENT: Requirement = Requirement::Mandatory;
                const REPETITION: Repetition = Repetition::Single;
            }
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

impl<'arg> Value<'arg> for &'arg str {
    #[inline]
    fn from_arg<E: ParameterError<'arg>>(arg: Arg<'arg>) -> Result<Self, E> {
        arg_as_str(arg)
    }
}

impl ParameterUsage for &str {
    const VALUE: ParameterValueKind = ParameterValueKind::Value;
    const REQUIREMENT: Requirement = Requirement::Mandatory;
    const REPETITION: Repetition = Repetition::Single;
}

#[cfg(feature = "std")]
impl<'arg> Value<'arg> for &'arg std::path::Path {
    #[inline]
    fn from_arg<E: ParameterError<'arg>>(arg: Arg<'arg>) -> Result<Self, E> {
        arg_as_str(arg).map(std::path::Path::new)
    }
}

#[cfg(feature = "std")]
impl ParameterUsage for &std::path::Path {
    const VALUE: ParameterValueKind = ParameterValueKind::Value;
    const REQUIREMENT: Requirement = Requirement::Mandatory;
    const REPETITION: Repetition = Repetition::Single;
}

impl<'arg> Parameter<'arg> for bool {
    #[inline]
    fn absent() -> Result<Self, RequiredError> {
        Ok(false)
    }

    #[inline]
    fn present<E: ParameterError<'arg>>(_arg: impl ArgAccess<'arg>) -> Result<Self, E> {
        Ok(true)
    }

    #[inline]
    fn add_present<E: ParameterError<'arg>>(
        &mut self,
        _arg: impl ArgAccess<'arg>,
    ) -> Result<(), E> {
        Err(E::got_additional_instance())
    }
}

impl ParameterUsage for bool {
    const VALUE: ParameterValueKind = ParameterValueKind::Flag;
    const REQUIREMENT: Requirement = Requirement::Optional;
    const REPETITION: Repetition = Repetition::Single;
}

impl<'arg> Parameter<'arg> for () {
    fn present<E: ParameterError<'arg>>(_arg: impl ArgAccess<'arg>) -> Result<Self, E> {
        Ok(())
    }

    fn add_present<E: ParameterError<'arg>>(
        &mut self,
        _arg: impl ArgAccess<'arg>,
    ) -> Result<(), E> {
        Err(E::got_additional_instance())
    }
}

impl ParameterUsage for () {
    const VALUE: ParameterValueKind = ParameterValueKind::Flag;
    const REQUIREMENT: Requirement = Requirement::Mandatory;
    const REPETITION: Repetition = Repetition::Single;
}

impl<'arg, T> PositionalParameter<'arg> for Option<T>
where
    T: Parameter<'arg>,
{
    #[inline]
    fn absent() -> Result<Self, RequiredError> {
        Ok(None)
    }

    #[inline]
    fn arg<E: ParameterError<'arg>>(argument: Arg<'arg>) -> Result<Self, E> {
        T::arg(argument).map(Some)
    }

    #[inline]
    fn add_arg<E: ParameterError<'arg>>(&mut self, _arg: Arg<'arg>) -> Result<(), E> {
        Err(E::got_additional_instance())
    }
}

impl<T: ParameterUsage> ParameterUsage for Option<T> {
    const VALUE: ParameterValueKind = T::VALUE;
    const REQUIREMENT: Requirement = Requirement::Optional;
    const REPETITION: Repetition = Repetition::Single;
}

// We assume that collections all need the std feature. We can always relax
// this requirement later.

#[cfg(feature = "std")]
impl<'arg, T> PositionalParameter<'arg> for std::vec::Vec<T>
where
    T: Value<'arg>,
{
    #[inline]
    fn absent() -> Result<Self, RequiredError> {
        Ok(std::vec::Vec::new())
    }

    #[inline]
    fn arg<E: ParameterError<'arg>>(argument: Arg<'arg>) -> Result<Self, E> {
        T::from_arg(argument).map(|value| std::vec::Vec::from([value]))
    }

    #[inline]
    fn add_arg<E: ParameterError<'arg>>(&mut self, argument: Arg<'arg>) -> Result<(), E> {
        T::from_arg(argument).map(|value| {
            self.push(value);
        })
    }
}

#[cfg(feature = "std")]
impl<T: ParameterUsage> ParameterUsage for std::vec::Vec<T> {
    const VALUE: ParameterValueKind = T::VALUE;
    const REQUIREMENT: Requirement = Requirement::Optional;
    const REPETITION: Repetition = Repetition::Multiple;
}
