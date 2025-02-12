use core::fmt::Display;

use debate_parser::Arg;

/*
Note to my Future self: you're generally pretty happy with `ParameterError`,
as the error type for the `Parameter` trait, but `FlagError` is way more up
in the air.

In general, you need to worry about these things for the error system you're
trying to design

Errors involving parameters, which might be absent, have invalid states. These
always involve a specific field and parameter type, and sometimes involve
a flag on the command line (notably, they DON'T when the error is absence)

Errors involving flags, which might be parameter errors, or might be
unrecognized flag errors. We're targeting composability, which

The main problem with the FlagError trait is that it's trying to do everything
and it shouldn't; in particular it's taking care of implementation details
related to how context is accumulated in an error that it shouldn't be.
*/

/// An error occurred while parsing a specific flag. This error will likel
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

    /// A parameter wasn't present. This method is the goofiness that makes
    /// the whole `FlagError` trait sort of senseless.
    fn absent_parameter(error: Self::ParameterError) -> Self;
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

    /// The argument failed to parse into an instance of the type.
    fn byte_parse_error(arg: Arg<'_>, msg: impl Display) -> Self;

    /// Something else went wrong
    fn custom(msg: impl Display) -> Self;
}
