use debate_parser::Arg;

use crate::{help::HelpRequest, parameter};

pub struct SubcommandChain<'a> {
    command: &'static str,
    prev: Option<&'a SubcommandChain<'a>>,
}

impl<'a> SubcommandChain<'a> {
    pub const fn new(command: &'static str) -> SubcommandChain<'static> {
        SubcommandChain {
            command,
            prev: None,
        }
    }

    pub fn for_each(&self, func: &mut impl FnMut(&'static str)) {
        if let Some(prev) = self.prev {
            prev.for_each(func);
        }
        func(self.command)
    }

    pub const fn chain(&'a self, command: &'static str) -> Self {
        Self {
            command,
            prev: Some(self),
        }
    }
}

/// The state associated with a [`BuildFromArgs`] type that is in the middle
/// of being parsed
pub trait State<'arg> {
    fn add_positional<E>(&mut self, argument: &'arg Arg) -> Result<(), E>
    where
        E: Error<'arg, ()>;

    fn add_long_option<E>(&mut self, option: &'arg Arg, argument: &'arg Arg) -> Result<(), E>
    where
        E: Error<'arg, ()>;

    fn add_long<A, E>(&mut self, option: &'arg Arg, argument: A) -> Result<(), E>
    where
        A: parameter::ArgAccess<'arg>,
        E: Error<'arg, A>;

    fn add_short<A, E>(&mut self, option: u8, argument: A) -> Result<(), E>
    where
        A: parameter::ArgAccess<'arg>,
        E: Error<'arg, A>;

    // IF this state includes a subcommand, it should call the handler using
    // that subcommand. It should first attempt to forward the handler as
    // deeply as possible to any nested subcommands.
    //
    // This method exists purely to assist with printing usage messages
    // for subcommands.
    fn with_subcommand_context<T, F: FnOnce(SubcommandChain<'_>) -> T>(
        &self,
        handler: F,
    ) -> Result<T, F> {
        Err(handler)
    }
}

/// Errors that can occur when adding an argument to the state during parsing
pub trait Error<'arg, Arg>: Sized {
    type ParameterError: parameter::Error<'arg>;

    /// A parameter type returned an error. This means that the argument was
    /// recognized and matched to a specific field, but something went wrong
    /// during parsing.
    fn parameter(field: &'static str, error: Self::ParameterError) -> Self;

    /// An argument was unrecognized. In this case, the Argument can be
    /// returned unused inside of `Self`, so that it can be retried by a
    /// different parser. For instance, a subcommand parser could indicate that
    /// an argument is unrecognized, and that argument can later be handled as
    /// a global argument.
    fn unrecognized(argument: Arg) -> Self;

    /// There was a state error from a flattened field
    fn flattened(field: &'static str, error: Self) -> Self;

    /// The positional argument was interpreted as a subcommand, but wasn't
    /// recognized as a known subcommand. The list of known subcommands is
    /// given.
    fn unknown_subcommand(expected: &'static [&'static str]) -> Self;

    /// The option was recognized, but it isn't a valid for this particular
    /// subcommand. The current subcommand, along with the list of subcommands
    /// that accept this option, are given
    fn wrong_subcommand_for_argument(
        subcommand: &'static str,
        allowed: &'static [&'static str],
    ) -> Self;

    /// This was a request for a usage message. This error doesn't need to
    /// interrupt argument parsing, since it can be useful to have a complete
    /// state object to print more contextually useful usage messages.
    fn help_requested(request: HelpRequest) -> Self;
}
