use core::marker::PhantomData;

use debate_parser::{Arg, ArgumentsParser};

use crate::{build, help::Usage, parameter, state};

macro_rules! use_stream_and_die {
    (
        $stream:ident = $make_stream:expr,
        $body:expr $(,)?
    ) => {{
        let $stream = $make_stream;
        let mut $stream = io::BufWriter::new($stream);

        $body.unwrap();
        $stream.flush().unwrap();
        process::exit(1);
    }};
}

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
        use std::{
            io::{self, Write},
            process,
        };

        use crate::errors::BuildError;
        use crate::printers::write_build_error;

        let mut state = T::State::default();

        // First step: loading arguments into the state. Most problems happen
        // here. This step is cool because, even if there's an error, the state
        // is still available, so we can do things like print more specific
        // usage messages based on the selected subcommand. This is where
        // --help is detected.
        if let Err(error) = load_state_from_parser(&mut state, arguments) {
            let error: BuildError<'_> = error;

            if let Some(mode) = error.help_request() {
                use std::eprintln;

                use crate::{printers::print_help, state::State};

                let stdout = io::stdout().lock();
                let mut stdout = io::BufWriter::new(stdout);

                print_help(
                    &mut stdout,
                    Self::NAME,
                    &Self::DESCRIPTION,
                    &Self::ITEMS,
                    mode,
                )
                .unwrap();

                let _ = state.get_subcommand_path({
                    use crate::state::SubcommandVisitor;

                    struct Visitor;

                    impl SubcommandVisitor for Visitor {
                        type Output = ();

                        fn visit_subcommand(
                            self,
                            subcommand: &state::SubcommandPath,
                        ) -> Self::Output {
                            eprintln!("PATH: {subcommand:#?}");
                        }
                    }

                    Visitor
                });

                stdout.flush().unwrap();

                process::exit(0);
            } else {
                // TODO: print a usage message here, perhaps one that specifically
                // accounts the flag that caused the error
                use_stream_and_die! {
                    stderr = io::stderr().lock(),
                    write_build_error(&mut stderr, &error)
                }
            }
        }

        // Second step: build the final parsed arguments from the state. This
        // is generally where absent required arguments, issues reconciling
        // alternation, and similar problems.
        // TODO: Use a different `build::Error` type for this part.
        let error: BuildError<'arg> = match Self::build(state) {
            Ok(parsed) => return parsed,
            Err(error) => error,
        };

        use_stream_and_die! {
            stderr = io::stderr().lock(),
            write_build_error(&mut stderr, &error)
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
