use std::eprintln;

use debate_parser::Arg;

use crate::{help::HelpRequest, parameter};

pub trait SubcommandVisitor {
    type Output;

    fn visit_subcommand(self, subcommand: &SubcommandPath) -> Self::Output;
}

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

    /**
    IF this state includes an actively selected subcommand, it should call
    the handler using that subcommand. It should first attempt to forward
    the handler as deeply as possible to any nested subcommands.

    Otherwise, it returns the unused handler.

    If the state has more than one sibling subcommand, the LAST one is chosen.
    This helps us with (imo) the most predictable behavior for how `--help`
    interacts with subcommands.

    This method exists purely to assist with printing usage messages
    for subcommands.
    */
    fn get_subcommand_path<V: SubcommandVisitor>(&self, visitor: V) -> Result<V::Output, V> {
        Err(visitor)
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
