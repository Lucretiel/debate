/*!
Command-line argument parsing state. Pairs with the traits in
[`build`][crate::build].

[`State`] provides the composable model for parsing command-line arguments, one
argument at a time. Each [`BuildFromArgs`][crate::build::BuildFromArgs] type
includes an associated state type. Arguments can be fed into this state, one
at a time, and either parsed or errors return if there are issues (such as
parse errors, unrecognized errors, and so on). In this way, we can compose
argument parsers by allowing states to contain other states, and pass arguments
inward as necessary.

Usually you don't need to implement [`State`] by hand; it happens automatically
in the derive macro.
*/

use core::iter;

use debate_parser::Arg;

use crate::{errors, help::HelpRequest, parameter};

// TODO: move all this subcommand business to another module
pub trait SubcommandVisitor {
    type Output;

    fn visit_subcommand(self, subcommand: &SubcommandPath<'_>) -> Self::Output;
}

// TODO: find a way to impl `SubcommandVisitor` for `FnOnce(&SubcommandPath)`.
// Currently it chokes on the lifetimes but it would be VERY convenient.
// alternatively, find a way to use a closure instead of a visitor in the first
// place. That has trouble with our need to "reverse" the closure process.

#[derive(Debug, Clone, Copy)]
pub enum SubcommandPathItem<'a> {
    Field(&'a str),
    Command(&'a str),
}

#[derive(Debug)]
pub struct SubcommandPath<'a> {
    item: SubcommandPathItem<'a>,
    prev: Option<&'a SubcommandPath<'a>>,
}

impl<'a> SubcommandPath<'a> {
    pub const fn new(item: SubcommandPathItem<'a>) -> SubcommandPath<'a> {
        Self { item, prev: None }
    }

    pub const fn chain(&'a self, item: SubcommandPathItem<'a>) -> Self {
        Self {
            item,
            prev: Some(self),
        }
    }

    pub fn iter<'s>(&'s self) -> impl Iterator<Item = &'s SubcommandPathItem<'a>> {
        iter::successors(Some(self), |current| current.prev).map(|current| &current.item)
    }
}

/// Helper type for implementing nested visitors in a way that's reversible.
/// Normally we'd just use nested closures, but once a closure has been nested
/// there's no way to get it back out again, which it turns out is a necessary
/// step.
#[doc(hidden)]
pub struct SubcommandPathVisitorWithItem<'a, V> {
    item: SubcommandPathItem<'a>,
    visitor: V,
}

impl<'a, V: SubcommandVisitor> SubcommandPathVisitorWithItem<'a, V> {
    #[inline]
    #[must_use]
    pub const fn new(item: SubcommandPathItem<'a>, visitor: V) -> Self {
        Self { item, visitor }
    }

    #[inline]
    #[must_use]
    pub fn into_inner(self) -> V {
        self.visitor
    }

    pub fn call(self) -> V::Output {
        self.visitor
            .visit_subcommand(&SubcommandPath::new(self.item))
    }
}

impl<V: SubcommandVisitor> SubcommandVisitor for SubcommandPathVisitorWithItem<'_, V> {
    type Output = V::Output;

    #[inline]
    fn visit_subcommand(self, subcommand: &SubcommandPath<'_>) -> Self::Output {
        let wrapped = subcommand.chain(self.item);
        self.visitor.visit_subcommand(&wrapped)
    }
}

/**
The state associated with a [`BuildFromArgs`][crate::BuildFromArgs] type that
is in the middle of being parsed. Arguments are passed into the state and
parsed, with usage errors or unrecognized arguments being returned.
*/
pub trait State<'arg> {
    /// Add a positional argument to this state
    fn add_positional<E>(&mut self, argument: &'arg Arg) -> Result<(), E>
    where
        E: Error<'arg, ()>;

    /**
    Add a long flag to this state, that definitely has an argument:
    `--long=value`
    */
    fn add_long_argument<E>(&mut self, option: &'arg Arg, argument: &'arg Arg) -> Result<(), E>
    where
        E: Error<'arg, ()>;

    /**
    Add a `--long` flag to this state. If the argument is unrecognized, it
    should be returned in the error, potentially allowing other states to try
    to parse it.
    */
    fn add_long<A, E>(&mut self, option: &'arg Arg, argument: A) -> Result<(), E>
    where
        A: parameter::ArgAccess<'arg>,
        E: Error<'arg, A>;

    /**
    Add a `-short` flag to this state. If the argument is unrecognized, it
    should be returned in the error, potentially allowing other states to try
    to parse it.
     */
    fn add_short<A, E>(&mut self, option: u8, argument: A) -> Result<(), E>
    where
        A: parameter::ArgAccess<'arg>,
        E: Error<'arg, A>;

    /**
    Method that allows the caller to determine if this state includes a
    subcommand, potentially including any nested subcommands. This is used
    when printing usage messages to allow subcommands to print their own
    messages (for instance, `cargo --help` vs `cargo --help build`).

    If this state includes a subcommand, it calls
    [`visit_subcommand`][SubcommandVisitor::visit_subcommand] to describe it.
    It should recurse as deeply as possible to show the *complete* subcommand.

    Otherwise, it returns the unused handler.

    If the state somehow has more than one sibling subcommand, the LAST one is
    chosen. This helps us with the most predictable behavior for how `--help`
    interacts with subcommands.

    This method exists purely to assist with printing usage messages
    for subcommands.
    */
    fn get_subcommand_path<V>(&self, visitor: V) -> Result<V::Output, V>
    where
        V: SubcommandVisitor,
    {
        Err(visitor)
    }
}

/**
Errors that can occur when adding an argument to the state during parsing. Note
that these errors don't include information about the actual `--flag` or
positional parameter that was parsed; this information is still available to
the caller, who should wrap this error if necessary when propagating it.
*/
pub trait Error<'arg, Arg>: Sized {
    /// Error type for parameter errors (see [`parameter`][Error::parameter]).
    type ParameterError: parameter::Error<'arg>;

    /// List of flags for [`conflicts_with_flags`][Error::conflicts_with_flags].
    type FlagList: errors::FlagsList<'static>;

    /**
    A parameter type returned an error. This means that the argument was
    recognized and matched to a specific field, but something went wrong during
    parsing, such as a letter appearing in a numeric parameter.
    */
    fn parameter(field: &'static str, error: Self::ParameterError) -> Self;

    /**
    An argument was unrecognized. In this case, the Argument can be returned
    unused inside of `Self`, so that it can be retried by a different parser.
    For instance, a subcommand parser could indicate that an argument is
    unrecognized, and that argument can later be handled as a global argument.
    */
    fn unrecognized(argument: Arg) -> Self;

    /// There was a state error from another [`State`] inside of this one.
    fn flattened(field: &'static str, error: Self) -> Self;

    /**
    The positional argument was interpreted as a subcommand, but wasn't
    recognized as a known subcommand. The list of known subcommands is
    given.
    */
    fn unknown_subcommand(expected: &'static [&'static str]) -> Self;

    /**
    The option was recognized, but it isn't a valid for this particular
    subcommand. The current subcommand, along with the list of subcommands
    that accept this option, are given
    */
    fn wrong_subcommand_for_argument(
        subcommand: &'static str,
        allowed: &'static [&'static str],
    ) -> Self;

    /// The argument conflicts with the given flags that were already parsed.
    fn conflicts_with_flags(flags: Self::FlagList) -> Self;

    /**
    This was a request for a usage message. This error doesn't need to
    interrupt argument parsing, since it can be useful to have a complete
    state object to print more contextually useful usage messages.
    */
    fn help_requested(request: HelpRequest) -> Self;
}
