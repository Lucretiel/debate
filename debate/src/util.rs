use std::marker::PhantomData;

use crate::{parameter, state};

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

    fn got_arg(arg: debate_parser::Arg<'arg>) -> Self {
        Self::Error(E::got_arg(arg))
    }

    fn got_additional_instance() -> Self {
        Self::Unrecognized(())
    }

    fn invalid_utf8(arg: debate_parser::Arg<'arg>) -> Self {
        Self::Error(E::invalid_utf8(arg))
    }

    fn parse_error(arg: &str, msg: impl core::fmt::Display) -> Self {
        Self::Error(E::parse_error(arg, msg))
    }

    fn byte_parse_error(arg: debate_parser::Arg<'arg>, msg: impl core::fmt::Display) -> Self {
        Self::Error(E::byte_parse_error(arg, msg))
    }

    fn custom(msg: impl core::fmt::Display) -> Self {
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

    fn rejected() -> Self {
        Self::Error(E::rejected())
    }
}
