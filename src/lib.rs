//#![no_std]

use core::fmt::Display;

use primitives::{Arg, ArgAccess};

pub mod primitives;

#[doc(hidden)]
pub mod util;

pub trait ValueError {
    /// The argument is required, and was absent on the command line
    fn required() -> Self;

    /// The argument appeared more than once on the command line, and shouldn't have
    fn got_additional_instance() -> Self;

    /// The argument requires a value, and none was provided
    fn needs_arg() -> Self;

    /// The argument must NOT have a value, and got one
    fn got_arg(arg: Arg<'_>) -> Self;

    /// The argument wasn't valid UTF-8 and should be
    fn invalid_utf8(arg: Arg<'_>) -> Self;

    /// The argument was valid UTF-8, but it failed to parse into an instance
    /// of the type
    fn parse_error(msg: impl Display) -> Self;

    /// Something else went wrong
    fn custom(msg: impl Display) -> Self;
}

pub trait Value<'arg>: Sized {
    fn absent<E: ValueError>() -> Result<Self, E>;
    fn arg<E: ValueError>(arg: Arg<'arg>) -> Result<Self, E>;
    fn present<E: ValueError>(arg: impl ArgAccess<'arg>) -> Result<Self, E>;

    fn add_arg<E: ValueError>(self, arg: Arg<'arg>) -> Result<Self, E>;
    fn add_present<E: ValueError>(self, arg: impl ArgAccess<'arg>) -> Result<Self, E>;
}

pub trait FlagError {
    type ValueError: ValueError;

    /// A positional parameter had an error
    fn positional(error: Self::ValueError) -> Self;

    /// A long option had an error
    fn long(option: Arg<'_>, error: Self::ValueError) -> Self;

    /// A short option had an error
    fn short(option: u8, error: Self::ValueError) -> Self;

    /// Got a long argument that we didn't recognize
    fn unrecognized_long(option: Arg<'_>, argument: Option<Arg<'_>>) -> Self;

    /// Got a short argument that we didn't recognize
    fn unrecognized_short(option: u8) -> Self;

    /// Got a positional argument that we didn't recognize
    fn unrecognized_positional(arg: Arg<'_>) -> Self;
}

/// Mutating version of the visitor
pub trait ApplyArg<'arg> {
    /// A positional parameter.
    fn positional<E: FlagError>(&mut self, argument: Arg<'arg>) -> Result<(), E>;

    /// A long option that definitely has an argument, because it was given
    /// as `--option=argument`
    fn long_option<E: FlagError>(
        &mut self,
        option: Arg<'arg>,
        argument: Arg<'arg>,
    ) -> Result<(), E>;

    /// A long option or flag, such as `--option`
    fn visit_long<E: FlagError>(
        &mut self,
        option: Arg<'arg>,
        arg: impl ArgAccess<'arg>,
    ) -> Result<(), E>;

    /// A long option or flag, such as `-o`
    fn visit_short<E: FlagError>(&mut self, option: u8, arg: impl ArgAccess<'arg>)
    -> Result<(), E>;
}

pub trait FromArgs<'arg>: Sized {
    fn from_args<I, E>(args: primitives::Arguments<'arg, I>) -> Result<Self, E>
    where
        I: Iterator<Item = &'arg [u8]>,
        E: FlagError;
}
