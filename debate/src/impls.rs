use debate_parser::Arg;

use crate::parameter::{
    ArgAccess, Error as ParameterError, Parameter, ParsedValue, RequiredError, Value,
};

macro_rules! from_str {
    ($(
        $(#[cfg($($cfg:tt)*)])*
        $type:ident $($(::$path:ident)*,)?
    )*) => {
        $(
            $(#[cfg($($cfg)*)])*
            impl ParsedValue for $type $($(:: $path)*)? {}
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
    fn from_arg_str<E: ParameterError<'arg>>(arg: &'arg str) -> Result<Self, E> {
        Ok(arg)
    }
}

#[cfg(feature = "std")]
impl<'arg> Value<'arg> for &'arg std::path::Path {
    #[inline]
    fn from_arg_str<E: ParameterError<'arg>>(arg: &'arg str) -> Result<Self, E> {
        Ok(std::path::Path::new(arg))
    }
}

impl<'arg> Parameter<'arg> for bool {
    #[inline]
    fn absent() -> Result<Self, RequiredError> {
        Ok(false)
    }

    #[inline]
    fn arg<E: ParameterError<'arg>>(arg: Arg<'arg>) -> Result<Self, E> {
        Err(E::got_arg(arg))
    }

    #[inline]
    fn present<E: ParameterError<'arg>>(_arg: impl ArgAccess<'arg>) -> Result<Self, E> {
        Ok(true)
    }

    #[inline]
    fn add_arg<E: ParameterError<'arg>>(&mut self, _arg: Arg<'arg>) -> Result<(), E> {
        Err(E::got_additional_instance())
    }

    #[inline]
    fn add_present<E: ParameterError<'arg>>(
        &mut self,
        _arg: impl ArgAccess<'arg>,
    ) -> Result<(), E> {
        Err(E::got_additional_instance())
    }
}

impl<'arg> Parameter<'arg> for () {
    fn absent() -> Result<Self, RequiredError> {
        Err(RequiredError)
    }

    fn arg<E: ParameterError<'arg>>(argument: Arg<'arg>) -> Result<Self, E> {
        Err(E::got_arg(argument))
    }

    fn present<E: ParameterError<'arg>>(_arg: impl ArgAccess<'arg>) -> Result<Self, E> {
        Ok(())
    }

    fn add_arg<E: ParameterError<'arg>>(&mut self, _arg: Arg<'arg>) -> Result<(), E> {
        Err(E::got_additional_instance())
    }

    fn add_present<E: ParameterError<'arg>>(
        &mut self,
        _arg: impl ArgAccess<'arg>,
    ) -> Result<(), E> {
        Err(E::got_additional_instance())
    }
}

impl<'arg, T> Parameter<'arg> for Option<T>
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
    fn present<E: ParameterError<'arg>>(argument: impl ArgAccess<'arg>) -> Result<Self, E> {
        T::present(argument).map(Some)
    }

    #[inline]
    fn add_arg<E: ParameterError<'arg>>(&mut self, _arg: Arg<'arg>) -> Result<(), E> {
        Err(E::got_additional_instance())
    }

    #[inline]
    fn add_present<E: ParameterError<'arg>>(
        &mut self,
        _arg: impl ArgAccess<'arg>,
    ) -> Result<(), E> {
        Err(E::got_additional_instance())
    }
}

// We assume that collections all need the std feature. We can always relax
// this requirement later.

#[cfg(feature = "std")]
impl<'arg, T> Parameter<'arg> for std::vec::Vec<T>
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
    fn present<E: ParameterError<'arg>>(argument: impl ArgAccess<'arg>) -> Result<Self, E> {
        argument.with(Self::arg)
    }

    #[inline]
    fn add_arg<E: ParameterError<'arg>>(&mut self, argument: Arg<'arg>) -> Result<(), E> {
        T::from_arg(argument).map(|value| {
            self.push(value);
        })
    }

    #[inline]
    fn add_present<E: ParameterError<'arg>>(
        &mut self,
        argument: impl ArgAccess<'arg>,
    ) -> Result<(), E> {
        argument.with(|arg| self.add_arg(arg))
    }
}
