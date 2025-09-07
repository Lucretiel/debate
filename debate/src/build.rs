use core::fmt::Display;

use crate::state;

/// Errors that can occur while converting the parsed arguments into the final
/// structure. Generally these failures are related to the absence of required
/// arguments; errors that are specific to a particular parameter or incoming
/// argument are handled earlier.
pub trait Error {
    /// A required long option/flag wasn't present. If the field also includes
    /// a short flag, it is included.
    fn required_long(field: &'static str, long: &'static str, short: Option<char>) -> Self;

    /// A required short option/flag field wasn't present.
    fn required_short(field: &'static str, short: char) -> Self;

    /// A required positional field wasn't present
    fn required_positional(field: &'static str, placeholder: &'static str) -> Self;

    /// There was an error building a flattened field
    fn flattened(field: &'static str, error: Self) -> Self;

    /// We required a subcommand, but none was provided
    fn required_subcommand(expected: &'static [&'static str]) -> Self;

    /// Something else went wrong
    fn custom(message: impl Display) -> Self;
}

/**
A type that can be parsed from command line arguments by repeatedly feeding
those argument into a `State`, and then then turning that state into this
final type. Types that implement `BuildFromArgs` automatically implement
`FromArgs`.

If you are manually implementing [`FromArgs`][crate::from_args::FromArgs], it
usually makes sense to instead implement [`BuildFromArgs`]. It will take care
of the looping logic and allow you to focus on individual argument handling,
and will also grant compatibility with delegating argument parsing with
`#[debate(flatten)]`.

Most of the interesting work happens in the associated
[`State`][BuildFromArgs::State] type, which has methods that allow it to parse
one command line argument at a time. After all arguments have been parsed, the
state object is passed into [`build`][BuildFromArgs::build], which checks for
things like required arguments and then constructs the final object.
*/
pub trait BuildFromArgs<'arg>: Sized {
    type State: state::State<'arg> + Default;

    fn build<E>(state: Self::State) -> Result<Self, E>
    where
        E: Error;
}
