use core::{fmt::Display, marker::PhantomData};

use debate_parser::{Arg, ArgAccess, ArgumentsParser};

use crate::state;

/// A type that can be parsed from command line arguments
pub trait FromArgs<'arg>: Sized {
    fn from_args<I, E>(args: ArgumentsParser<'arg, I>) -> Result<Self, E>
    where
        I: Iterator<Item = &'arg [u8]>,
        E: Error<'arg>;
}

/// Errors that can occur while parsing arguments into some kind of structure
pub trait Error<'arg> {
    type StateError<A>: state::Error<'arg, A>;

    /// There was an error handling a positional argument
    fn positional(argument: Arg<'arg>, error: Self::StateError<()>) -> Self;

    /// There was an error handling a `--long=argument` argument
    fn long_with_argument(
        option: Arg<'arg>,
        argument: Arg<'arg>,
        error: Self::StateError<()>,
    ) -> Self;

    /// There was an error handling a `--long` long argument
    fn long<A>(option: Arg<'arg>, error: Self::StateError<A>) -> Self;

    /// There was an error handling a `-s` short argument
    fn short<A>(option: u8, error: Self::StateError<A>) -> Self;

    /// A required field wasn't present among the command line arguments. If
    /// the field is a flag or an option, its long and short CLI names are also
    /// provided
    fn required(field: &'static str, long: Option<&'static str>, short: Option<char>) -> Self;

    /// There was an error parsing arguments for a flattened field
    fn flattened(field: &'static str, error: Self) -> Self;

    /// We required a subcommand, but none was provied
    fn required_subcommand(expected: &'static [&'static str]) -> Self;

    /// Something else went wrong
    fn custom(msg: impl Display) -> Self;
}

/**
A type that can be parsed from command line arguments by repeatedly feeding
those argument into a `State`, and then then turning that state into this
final type. Types that implement `BuildFromArgs` automatically implement
`FromArgs`.

If you are manually implementing [`FromArgs`], it usually makes sense to
instead implement [`BuildFromArgs`]. It will take care of the looping logic
and allow you to focus on individual argument handling, and this will also
grant compatibility with delegating argument parsing with `#[debate(flatten)]`.
*/
pub trait BuildFromArgs<'arg>: Sized {
    type State: state::State<'arg>;

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
        let mut state = T::State::default();

        struct Visitor<B, E> {
            state: B,
            error: PhantomData<E>,
        }

        impl<'arg, B, E> debate_parser::Visitor<'arg> for Visitor<&mut B, E>
        where
            B: state::State<'arg>,
            E: Error<'arg>,
        {
            type Value = Result<(), E>;

            fn visit_positional(self, argument: Arg<'arg>) -> Self::Value {
                self.state
                    .add_positional(argument)
                    .map_err(|error| E::positional(argument, error))
            }

            fn visit_long_option(self, option: Arg<'arg>, argument: Arg<'arg>) -> Self::Value {
                self.state
                    .add_long_option(option, argument)
                    .map_err(|error| E::long_with_argument(option, argument, error))
            }

            fn visit_long(self, option: Arg<'arg>, arg: impl ArgAccess<'arg>) -> Self::Value {
                self.state
                    .add_long(option, arg)
                    .map_err(|error| E::long(option, error))
            }

            fn visit_short(self, option: u8, arg: impl ArgAccess<'arg>) -> Self::Value {
                self.state
                    .add_short(option, arg)
                    .map_err(|error| E::short(option, error))
            }
        }

        loop {
            match args.next_arg(Visitor {
                state: &mut state,
                error: PhantomData,
            }) {
                None => break Self::build(state),
                Some(Err(err)) => break Err(err),
                Some(Ok(())) => continue,
            }
        }
    }
}
