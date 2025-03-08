use core::marker::PhantomData;

use debate_parser::{Arg, ArgumentsParser};

use crate::{build, parameter, state};

/// A type that can be parsed from command line arguments
pub trait FromArgs<'arg>: Sized {
    fn from_parser<I, E>(args: ArgumentsParser<'arg, I>) -> Result<Self, E>
    where
        I: Iterator<Item = &'arg [u8]>,
        E: Error<'arg> + build::Error;
}

/// Errors that can occur while handling incoming arguments
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
}

impl<'arg, T> FromArgs<'arg> for T
where
    T: build::BuildFromArgs<'arg>,
{
    fn from_parser<I, E>(mut args: ArgumentsParser<'arg, I>) -> Result<Self, E>
    where
        I: Iterator<Item = &'arg [u8]>,
        E: Error<'arg> + build::Error,
    {
        let mut state = T::State::default();

        struct ArgAccessAdapter<A>(A);

        impl<'arg, A> parameter::ArgAccess<'arg> for ArgAccessAdapter<A>
        where
            A: debate_parser::ArgAccess<'arg>,
        {
            #[inline]
            fn with<T, E>(self, op: impl FnOnce(Arg<'arg>) -> Result<T, E>) -> Result<T, E>
            where
                E: parameter::Error<'arg>,
            {
                op(self.0.take().ok_or_else(|| E::needs_arg())?)
            }
        }

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

            fn visit_long(
                self,
                option: Arg<'arg>,
                arg: impl debate_parser::ArgAccess<'arg>,
            ) -> Self::Value {
                self.state
                    .add_long(option, ArgAccessAdapter(arg))
                    .map_err(|error| E::long(option, error))
            }

            fn visit_short(
                self,
                option: u8,
                arg: impl debate_parser::ArgAccess<'arg>,
            ) -> Self::Value {
                self.state
                    .add_short(option, ArgAccessAdapter(arg))
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
