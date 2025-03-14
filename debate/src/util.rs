use core::fmt;

use debate_parser::Arg;

use crate::{
    parameter::{self, ArgAccess, Parameter, RequiredError},
    state,
};

pub enum DetectUnrecognized<A, E> {
    Unrecognized(A),
    Error(E),
}

impl<'arg, E> parameter::Error<'arg> for DetectUnrecognized<(), E>
where
    E: parameter::Error<'arg>,
{
    fn needs_arg() -> Self {
        Self::Error(E::needs_arg())
    }

    fn got_arg(arg: Arg<'arg>) -> Self {
        Self::Error(E::got_arg(arg))
    }

    fn got_additional_instance() -> Self {
        Self::Unrecognized(())
    }

    fn invalid_utf8(arg: Arg<'arg>) -> Self {
        Self::Error(E::invalid_utf8(arg))
    }

    fn parse_error(arg: &str, msg: impl fmt::Display) -> Self {
        Self::Error(E::parse_error(arg, msg))
    }

    fn byte_parse_error(arg: Arg<'arg>, msg: impl fmt::Display) -> Self {
        Self::Error(E::byte_parse_error(arg, msg))
    }

    fn custom(msg: impl fmt::Display) -> Self {
        Self::Error(E::custom(msg))
    }
}

impl<'arg, A, E> state::Error<'arg, A> for DetectUnrecognized<A, E>
where
    E: state::Error<'arg, A>,
{
    type ParameterError = E::ParameterError;

    fn parameter(field: &'static str, error: Self::ParameterError) -> Self {
        Self::Error(E::parameter(field, error))
    }

    fn unrecognized(argument: A) -> Self {
        Self::Unrecognized(argument)
    }

    fn flattened(field: &'static str, error: Self) -> Self {
        Self::Error(match error {
            DetectUnrecognized::Unrecognized(_) => todo!(),
            DetectUnrecognized::Error(err) => E::flattened(field, err),
        })
    }

    fn unknown_subcommand(expected: &'static [&'static str]) -> Self {
        Self::Error(E::unknown_subcommand(expected))
    }

    fn wrong_subcommand_for_argument(subcommand: &str, allowed: &[&'static str]) -> Self {
        Self::Error(E::wrong_subcommand_for_argument(subcommand, allowed))
    }
}

/// A parameter that counts the number of times it appears on the command
/// line.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Count {
    pub count: u32,
}

impl<'arg> Parameter<'arg> for Count {
    fn absent() -> Result<Self, RequiredError> {
        Ok(Self { count: 0 })
    }

    fn arg<E: parameter::Error<'arg>>(argument: Arg<'arg>) -> Result<Self, E> {
        Err(E::got_arg(argument))
    }

    fn present<E: parameter::Error<'arg>>(_arg: impl ArgAccess<'arg>) -> Result<Self, E> {
        Ok(Self { count: 1 })
    }

    fn add_arg<E: parameter::Error<'arg>>(&mut self, argument: Arg<'arg>) -> Result<(), E> {
        Err(E::got_arg(argument))
    }

    fn add_present<E: parameter::Error<'arg>>(
        &mut self,
        _arg: impl ArgAccess<'arg>,
    ) -> Result<(), E> {
        self.count = self
            .count
            .checked_add(1)
            .ok_or_else(|| E::custom("too many appearences (overflowed a u32)"))?;

        Ok(())
    }
}
