use core::marker::PhantomData;

use debate_parser::{Arg, ArgumentsParser};

use crate::{
    build,
    help::{Description, HelpRequest, Parameter, Usage, UsageItems},
    parameter,
    state::{self, SubcommandPath, SubcommandPathItem},
};

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

/**
A type that can be parsed from command line arguments

If you're implementing this by hand, you should certainly prefer instead
to implement [`BuildFromArgs`][crate::build::BuildFromArgs]. That takes care of
a lot of the tedious looping over the `arguments` object, and also allows
delegating argument parsing with `#[debate(flatten)]`.
*/
pub trait FromArgs<'arg>: Sized {
    /**
    Attempt to parse these arguments from the given raw arguments. Return a
    structured error in the event of a problem.
    */
    fn try_from_parser<E>(
        arguments: ArgumentsParser<'arg, impl Iterator<Item = &'arg [u8]>>,
    ) -> Result<Self, E>
    where
        E: Error<'arg> + build::Error;

    /**
    Parse these arguments from the given raw arguments. If there is an error,
    print a useful error message and exit the process with an error code. If
    there is a request for help, print a usage message and exit the process
    with a success code.
     */
    #[cfg(feature = "std")]
    fn from_parser(arguments: ArgumentsParser<'arg, impl Iterator<Item = &'arg [u8]>>) -> Self
    where
        Self: Usage;
}

/**
Errors that can occur while parsing arguments in [`FromArgs`].
*/
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
        use crate::printers::build_error;

        let mut state = T::State::default();

        // FIXME: get your hands on argv(0) somehow. Don't really want to
        // put it into arguments. Take it as a function argument and force
        // people to use the Arguments helper? Don't bother and just insist
        // people name their struct correctly?

        // First step: loading arguments into the state. Most problems happen
        // here. This step is cool because, even if there's an error, the state
        // is still available, so we can do things like print more specific
        // usage messages based on the selected subcommand. This is where
        // --help is detected.
        if let Err(error) = load_state_from_parser(&mut state, arguments) {
            let error: BuildError<'_> = error;

            if let Some(mode) = error.help_request() {
                use crate::{printers::print_help, state::State};

                let stdout = io::stdout().lock();
                let mut stdout = io::BufWriter::new(stdout);

                // Attempt to detect a subcommand and print help for that
                // instead of from `Self`

                let print_result = match state.get_subcommand_path({
                    // FIXME: We'd LOVE it if this could be a closure,
                    // obviously. Currently it chokes on lifetimes.
                    use crate::state::SubcommandVisitor;

                    struct Visitor<'a, T: Usage, W: io::Write> {
                        root: PhantomData<T>,
                        out: &'a mut W,
                        command: &'a str,
                        style: HelpRequest,
                    }

                    impl<T: Usage, W: io::Write> SubcommandVisitor for Visitor<'_, T, W> {
                        type Output = io::Result<()>;

                        fn visit_subcommand(
                            self,
                            subcommand: &state::SubcommandPath<'_>,
                        ) -> Self::Output {
                            let (usage, description) = discover_subcommand_usage_from_path(
                                &T::ITEMS,
                                &T::DESCRIPTION,
                                subcommand,
                            );

                            let printed_subcommands =
                                subcommand.iter().filter_map(|item| match *item {
                                    SubcommandPathItem::Field(_) => None,
                                    SubcommandPathItem::Command(command) => Some(command),
                                });

                            print_help(
                                self.out,
                                self.command,
                                printed_subcommands,
                                description,
                                usage,
                                self.style,
                            )
                        }
                    }

                    Visitor::<Self, _> {
                        root: PhantomData,
                        out: &mut stdout,
                        command: Self::NAME,
                        style: mode,
                    }
                }) {
                    Ok(result) => result,
                    Err(_) => print_help(
                        &mut stdout,
                        Self::NAME,
                        [],
                        &Self::DESCRIPTION,
                        &Self::ITEMS,
                        mode,
                    ),
                };

                print_result.unwrap();
                stdout.flush().unwrap();

                process::exit(0);
            } else {
                use_stream_and_die! {
                    stderr = io::stderr().lock(),
                    writeln!(stderr, "{}", build_error(&error)),
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
            writeln!(stderr, "{}", build_error(&error)),
        }
    }
}

// TODO: the fact that this function is fallible is a signal that the current
// design of `Usage` is probably not ideal. Would like to someday replace
// with a more type-driven version that is capable of infallibly doing this
// work, since it should be possible to directly query the state.
//
// That might require more tightly coupling `Usage` and `BuildFromArgs`,
// though, which I'm REALLY trying to go out of my way to avoid.
fn discover_subcommand_usage_from_path<'a>(
    root: &'a UsageItems<'a>,
    description: &'a Description<'a>,
    path: &SubcommandPath<'_>,
) -> (&'a UsageItems<'a>, &'a Description<'a>) {
    path.iter()
        .fold((root, description), |(root, _), path| match (path, root) {
            (SubcommandPathItem::Field(field), UsageItems::Parameters { parameters }) => parameters
                .iter()
                .find_map(|parameter| match parameter {
                    Parameter::Group(group) if group.id == *field => {
                        Some((&group.contents, &group.description))
                    }
                    _ => None,
                })
                .expect(
                    "field name mismatch when looking up subcommand usage. \
                    This shouldn't be possible",
                ),
            (SubcommandPathItem::Command(command), UsageItems::Subcommands { commands, .. }) => {
                commands
                    .iter()
                    .find(|subcommand| subcommand.command == *command)
                    .map(|command| (&command.usage, &command.description))
                    .expect(
                        "command name mismatch when looking up subcommand \
                        usage. This shouldn't be possible",
                    )
            }
            _ => panic!(
                "type mismatch when looking up subcommand usage. This shouldn't be possible."
            ),
        })
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

        fn visit_long_argument(self, option: &'arg Arg, argument: &'arg Arg) -> Self::Value {
            self.state
                .add_long_argument(option, argument)
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
