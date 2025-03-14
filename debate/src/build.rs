use core::fmt::Display;

use crate::{help::UsagePrinter, state};

/// Errors that can occur while converting the parsed arguments into the final
/// structure
pub trait Error {
    /// A required field wasn't present among the command line arguments. If
    /// the field is a flag or an option, its long and short CLI names are also
    /// provided
    fn required(field: &'static str, long: Option<&'static str>, short: Option<char>) -> Self;

    /// There was an error building a flattened field
    fn flattened(field: &'static str, error: Self) -> Self;

    /// We required a subcommand, but none was provided
    fn required_subcommand(expected: &'static [&'static str]) -> Self;

    /// The user requested some kind of usage message with --help, and the
    /// usage printer didn't gracefully exit the program. Typically this error
    /// only occurs in unit tests.
    fn help_requested() -> Self;

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
        E: Error;

    #[expect(unused_variables)]
    fn build_with_printer<E>(state: Self::State, printer: impl UsagePrinter) -> Result<Self, E>
    where
        E: Error,
    {
        Self::build(state)
    }
}
