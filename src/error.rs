use core::fmt::Display;

use crate::primitives::Arg;

/// An error occurred while parsing a specific flag.
pub trait FlagError {
    type ParameterError: ParameterError;

    /// A positional parameter had an error
    fn positional(error: Self::ParameterError) -> Self;

    /// A long option had an error
    fn long(option: Arg<'_>, error: Self::ParameterError) -> Self;

    /// A short option had an error
    fn short(option: u8, error: Self::ParameterError) -> Self;

    /// Got a long argument that we didn't recognize
    fn unrecognized_long(option: Arg<'_>, argument: Option<Arg<'_>>) -> Self;

    /// Got a short argument that we didn't recognize
    fn unrecognized_short(option: u8) -> Self;

    /// Got a positional argument that we didn't recognize
    fn unrecognized_positional(arg: Arg<'_>) -> Self;

    // TODO: rejected forms. Used for composability.
}

pub trait ParameterError {
    /// This argument is required and was absent on the command line
    fn required() -> Self;

    /// The argument requires a value, and none was provided
    fn needs_arg() -> Self;

    /// The argument must NOT have a value, and got one
    fn got_arg(arg: Arg<'_>) -> Self;

    /// The argument appeared more than once on the command line, and shouldn't have
    fn got_additional_instance() -> Self;

    /// The argument wasn't valid UTF-8 and should be
    fn invalid_utf8(arg: Arg<'_>) -> Self;

    /// The argument was valid UTF-8, but it failed to parse into an instance
    /// of the type
    fn parse_error(arg: &str, msg: impl Display) -> Self;

    /// Something else went wrong
    fn custom(msg: impl Display) -> Self;
}
