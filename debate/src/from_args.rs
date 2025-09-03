use core::marker::PhantomData;

use debate_parser::{Arg, ArgumentsParser};

use crate::{build, help::Usage, parameter, state};

/// A type that can be parsed from command line arguments
pub trait FromArgs<'arg>: Sized {
    fn try_from_parser<E>(
        arguments: ArgumentsParser<'arg, impl Iterator<Item = &'arg [u8]>>,
    ) -> Result<Self, E>
    where
        E: Error<'arg> + build::Error;

    #[cfg(feature = "std")]
    fn from_parser(arguments: ArgumentsParser<'arg, impl Iterator<Item = &'arg [u8]>>) -> Self
    where
        Self: Usage;
}

/// Errors that can occur while handling incoming arguments
pub trait Error<'arg> {
    type StateError<A>: state::Error<'arg, A>;

    /// There was an error handling a positional argument
    fn positional(argument: &'arg Arg, error: Self::StateError<()>) -> Self;

    /// There was an error handling a `--long=argument` argument
    fn long_with_argument(
        option: &'arg Arg,
        argument: &'arg Arg,
        error: Self::StateError<()>,
    ) -> Self;

    /// There was an error handling a `--long` long argument
    fn long<A>(option: &'arg Arg, error: Self::StateError<A>) -> Self;

    /// There was an error handling a `-s` short argument
    fn short<A>(option: u8, error: Self::StateError<A>) -> Self;

    /// There were multiple argument errors
    fn and(self, error: Self) -> Self;
}

impl<'arg, T> FromArgs<'arg> for T
where
    T: build::BuildFromArgs<'arg>,
{
    fn try_from_parser<E>(
        arguments: ArgumentsParser<'arg, impl Iterator<Item = &'arg [u8]>>,
    ) -> Result<Self, E>
    where
        E: Error<'arg> + build::Error,
    {
        let mut state = T::State::default();

        load_state_from_parser(&mut state, arguments)?;

        Self::build(state)
    }

    #[cfg(feature = "std")]
    fn from_parser(arguments: ArgumentsParser<'arg, impl Iterator<Item = &'arg [u8]>>) -> Self
    where
        Self: Usage,
    {
        use std::{io, process};

        use crate::errors::BuildError;
        use crate::printers::{print_help, write_build_error};

        let mut state = T::State::default();

        // TODO: this is going to need to be refactored later, because usage
        // messages in some cases will make use of the state.
        let error: BuildError<'arg> = match load_state_from_parser(&mut state, arguments) {
            Ok(()) => match Self::build(state) {
                Ok(parsed) => return parsed,
                Err(error) => error,
            },
            Err(error) => error,
        };

        // TODO: if we have a subcommand, show help for that subcommand.
        // Probably it's gonna make sense to use nested Errors to create a
        // path to the relevant subcommand for looking up the usage in the
        // UsageItems we have.
        if let Some(_) = error.help_request() {
            use crate::printers::print_help;

            print_help(
                &mut io::stdout().lock(),
                Self::NAME,
                Self::DESCRIPTION.long,
                &Self::ITEMS,
            )
            .unwrap();
            process::exit(0);
        } else {
            // TODO: print a usage message here, perhaps one that specifically
            // accounts the flag that caused the error
            write_build_error(&mut io::stderr().lock(), &error).unwrap();
            process::exit(1);
        }
    }
}

pub fn load_state_from_parser<'arg, T, E>(
    state: &mut T,
    mut arguments: ArgumentsParser<'arg, impl Iterator<Item = &'arg [u8]>>,
) -> Result<(), E>
where
    T: state::State<'arg>,
    E: Error<'arg>,
{
    struct ArgAccessAdapter<A>(A);

    impl<'arg, A> parameter::ArgAccess<'arg> for ArgAccessAdapter<A>
    where
        A: debate_parser::ArgAccess<'arg>,
    {
        #[inline]
        fn with<T, E>(self, op: impl FnOnce(&'arg Arg) -> Result<T, E>) -> Result<T, E>
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

        fn visit_positional(self, argument: &'arg Arg) -> Self::Value {
            self.state
                .add_positional(argument)
                .map_err(|error| E::positional(argument, error))
        }

        fn visit_long_option(self, option: &'arg Arg, argument: &'arg Arg) -> Self::Value {
            self.state
                .add_long_option(option, argument)
                .map_err(|error| E::long_with_argument(option, argument, error))
        }

        fn visit_long(
            self,
            option: &'arg Arg,
            arg: impl debate_parser::ArgAccess<'arg>,
        ) -> Self::Value {
            self.state
                .add_long(option, ArgAccessAdapter(arg))
                .map_err(|error| E::long(option, error))
        }

        fn visit_short(self, option: u8, arg: impl debate_parser::ArgAccess<'arg>) -> Self::Value {
            self.state
                .add_short(option, ArgAccessAdapter(arg))
                .map_err(|error| E::short(option, error))
        }
    }

    let mut result = Ok(());

    loop {
        result = match arguments.next_arg(Visitor {
            state: &mut *state,
            error: PhantomData,
        }) {
            None => break result,
            Some(Ok(())) => continue,
            Some(Err(err)) => match result {
                Ok(()) => Err(err),
                Err(old) => Err(old.and(err)),
            },
        }
    }
}
