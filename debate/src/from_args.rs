use core::{fmt::Display, marker::PhantomData};

use debate_parser::{Arg, ArgAccess, ArgumentsParser};

use crate::parameter;

/// A type that can be parsed from command line arguments
pub trait FromArgs<'arg>: Sized {
    fn from_args<I, E>(args: ArgumentsParser<'arg, I>) -> Result<Self, E>
    where
        I: Iterator<Item = &'arg [u8]>,
        E: Error<'arg>;
}

/// The state associated with a [`BuildFromArgs`] type that is in the middle
/// of being parsed
pub trait State<'arg>: Sized {
    fn start() -> Self;

    fn add_positional<E>(&mut self, argument: Arg<'arg>) -> Result<(), E>
    where
        E: StateError<'arg, Arg<'arg>>;

    fn add_long_option<E>(&mut self, option: Arg<'arg>, argument: Arg<'arg>) -> Result<(), E>
    where
        E: StateError<'arg, Arg<'arg>>;

    fn add_long<A, E>(&mut self, option: Arg<'arg>, argument: A) -> Result<(), E>
    where
        A: ArgAccess<'arg>,
        E: StateError<'arg, A>;

    fn add_short<A, E>(&mut self, option: u8, argument: A) -> Result<(), E>
    where
        A: ArgAccess<'arg>,
        E: StateError<'arg, A>;
}

/**
A type that can be parsed from command line arguments by repeatedly feeding
those argument into a `State`, and then then turning that state into this
final type.

The main advantage of `BuildFromArgs` is that it allows command-line argument
parsing to compose. The top-level `Args` struct can delegate parts of its
parsing to a selected subcommand type, for example.
*/
pub trait BuildFromArgs<'arg>: Sized {
    type State: State<'arg>;

    fn build<E>(state: Self::State) -> Result<Self, E>
    where
        E: Error<'arg>;
}

impl<'arg, T> FromArgs<'arg> for T
where
    T: BuildFromArgs<'arg>,
{
    fn from_args<I, E>(mut args: ArgumentsParser<'arg, I>) -> Result<Self, E>
    where
        I: Iterator<Item = &'arg [u8]>,
        E: Error<'arg>,
    {
        let mut builder = T::State::start();

        struct Visitor<B, E> {
            builder: B,
            error: PhantomData<E>,
        }

        impl<'arg, B, E> debate_parser::Visitor<'arg> for Visitor<&mut B, E>
        where
            B: State<'arg>,
            E: Error<'arg>,
        {
            type Value = Result<(), E>;

            fn visit_positional(self, argument: Arg<'arg>) -> Self::Value {
                self.builder
                    .add_positional(argument)
                    .map_err(|err| E::argument(err, ParameterKind::Positional, Some(argument)))
            }

            fn visit_long_option(self, option: Arg<'arg>, argument: Arg<'arg>) -> Self::Value {
                self.builder
                    .add_long_option(option, argument)
                    .map_err(|err| E::argument(err, ParameterKind::Long(option), Some(argument)))
            }

            fn visit_long(self, option: Arg<'arg>, arg: impl ArgAccess<'arg>) -> Self::Value {
                self.builder
                    .add_long(option, arg)
                    .map_err(|err| E::argument(err, ParameterKind::Long(option), None))
            }

            fn visit_short(self, option: u8, arg: impl ArgAccess<'arg>) -> Self::Value {
                self.builder
                    .add_short(option, arg)
                    .map_err(|err| E::argument(err, ParameterKind::Short(option), None))
            }
        }

        loop {
            match args.next_arg(Visitor {
                builder: &mut builder,
                error: PhantomData,
            }) {
                None => break,
                Some(Err(err)) => return Err(err),
                Some(Ok(())) => continue,
            }
        }

        Self::build(builder)
    }
}

/// Errors that can occur when adding an argument to the state during parsing
pub trait StateError<'arg, Arg> {
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

/// Simple enum used to indicate what kind of argument encountered an error
#[derive(Debug, Clone, Copy)]
pub enum ParameterKind<'arg> {
    /// A positional parameter
    Positional,

    /// A `--long` flag or option
    Long(Arg<'arg>),

    /// A `-s` short flag or option
    Short(u8),
}

/// Errors that can occur while parsing arguments into some kind of structure
pub trait Error<'arg> {
    type StateError<A>: StateError<'arg, A>;

    /// There was an error handling one of the command line arguments: it
    /// wasn't recognized, or there was a parse error, or something like that.
    /// If there was a known argument (for instance, because of a positional
    /// parameter or --arg=value), it is provided.
    fn argument<A>(
        error: Self::StateError<A>,
        kind: ParameterKind<'arg>,
        argument: Option<Arg<'arg>>,
    ) -> Self;

    /// A required field wasn't present among the command line arguments. If
    /// the field is a flag or an option, its long and short CLI names are also
    /// provided
    fn required(field: &'static str, long: Option<&'static str>, short: Option<char>) -> Self;

    /// Something else went wrong
    fn custom(msg: impl Display) -> Self;
}
