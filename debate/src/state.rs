use debate_parser::{Arg, ArgAccess};

use crate::parameter;

/// The state associated with a [`BuildFromArgs`] type that is in the middle
/// of being parsed
pub trait State<'arg>: Default {
    fn add_positional<E>(&mut self, argument: Arg<'arg>) -> Result<(), E>
    where
        E: Error<'arg, ()>;

    fn add_long_option<E>(&mut self, option: Arg<'arg>, argument: Arg<'arg>) -> Result<(), E>
    where
        E: Error<'arg, ()>;

    fn add_long<A, E>(&mut self, option: Arg<'arg>, argument: A) -> Result<(), E>
    where
        A: ArgAccess<'arg>,
        E: Error<'arg, A>;

    fn add_short<A, E>(&mut self, option: u8, argument: A) -> Result<(), E>
    where
        A: ArgAccess<'arg>,
        E: Error<'arg, A>;
}

/// Errors that can occur when adding an argument to the state during parsing
pub trait Error<'arg, Arg> {
    type ParameterError: parameter::Error<'arg>;

    /// A parameter type returned an error
    fn parameter(field: &'static str, error: Self::ParameterError) -> Self;

    /// An argument was unrecognized. In this case, the Argument can be
    /// returned unused inside of `Self`, so that it can be retried by a
    /// different parser. For instance, a subcommand parser could indicate that
    /// an argument is unrecognized, and that argument can later be handled as
    /// a global argument.
    fn unrecognized(argument: Arg) -> Self;

    /// An argument was recognized, but rejected (for instance, because of a
    /// mutual exclusion rule). This variant should gain a field for rationale
    /// to be attached.
    fn rejected() -> Self;
}
