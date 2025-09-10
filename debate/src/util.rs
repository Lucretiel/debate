use core::{fmt, str::from_utf8};

use debate_parser::Arg;

use crate::{
    help::{self, HelpRequest},
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

    fn got_arg(arg: &'arg Arg) -> Self {
        Self::Error(E::got_arg(arg))
    }

    fn got_additional_instance() -> Self {
        Self::Unrecognized(())
    }

    fn invalid_utf8(arg: &'arg Arg) -> Self {
        Self::Error(E::invalid_utf8(arg))
    }

    fn parse_error(arg: &'arg str, msg: impl fmt::Display) -> Self {
        Self::Error(E::parse_error(arg, msg))
    }

    fn byte_parse_error(arg: &'arg Arg, msg: impl fmt::Display) -> Self {
        Self::Error(E::byte_parse_error(arg, msg))
    }

    fn should_be(argument: &'arg Arg, expected: &'static [&'static str]) -> Self {
        Self::Error(E::should_be(argument, expected))
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
        // Currently we get rid of flattens for unrecognized. This kind of
        // makes sense because unrecognition is a property of the entire state
        // tree, not any particular part of it.
        match error {
            Self::Unrecognized(arg) => Self::Unrecognized(arg),
            Self::Error(err) => Self::Error(E::flattened(field, err)),
        }
    }

    fn unknown_subcommand(expected: &'static [&'static str]) -> Self {
        Self::Error(E::unknown_subcommand(expected))
    }

    fn wrong_subcommand_for_argument(
        subcommand: &'static str,
        allowed: &'static [&'static str],
    ) -> Self {
        Self::Error(E::wrong_subcommand_for_argument(subcommand, allowed))
    }

    fn help_requested(req: HelpRequest) -> Self {
        Self::Error(E::help_requested(req))
    }
}

/// A parameter that counts the number of times it appears on the command
/// line. Enables things like increasing verbosity levels via `-v`, `-vv`,
/// `-vvv`
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Count {
    pub count: u32,
}

impl<'arg> Parameter<'arg> for Count {
    fn absent() -> Result<Self, RequiredError> {
        Ok(Self { count: 0 })
    }

    fn present<E: parameter::Error<'arg>>(_arg: impl ArgAccess<'arg>) -> Result<Self, E> {
        Ok(Self { count: 1 })
    }

    fn add_present<E: parameter::Error<'arg>>(
        &mut self,
        _arg: impl ArgAccess<'arg>,
    ) -> Result<(), E> {
        self.count = self
            .count
            .checked_add(1)
            .ok_or_else(|| E::custom("too many appearances (overflowed a u32)"))?;

        Ok(())
    }
}

impl help::ParameterUsage for Count {
    const VALUE: help::ParameterValueKind = help::ParameterValueKind::Flag;
    const REQUIREMENT: help::Requirement = help::Requirement::Optional;
    const REPETITION: help::Repetition = help::Repetition::Multiple;
}

/// Input arguments are always raw byte slices; this function converts
/// an argument to a string and handles returning the appropriate error
/// in a `PositionalParameter` or `Value` implementation.
pub fn arg_as_str<'arg, E>(arg: &'arg Arg) -> Result<&'arg str, E>
where
    E: parameter::Error<'arg>,
{
    from_utf8(arg.bytes()).map_err(|_err| E::invalid_utf8(arg))
}
