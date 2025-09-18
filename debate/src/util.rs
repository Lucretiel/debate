/*!
Utility types and functions for [`debate`][crate]
 */

use core::{
    fmt,
    iter::FusedIterator,
    ops::{Index, IndexMut},
    str::from_utf8,
};

use debate_parser::Arg;

use crate::{
    help::{self, HelpRequest},
    parameter::{self, ArgAccess, Parameter, PositionalParameter, RequiredError, Value},
    state,
};

/**
An error type that assists state errors with propagating unrecognized
arguments.

When a [`State`][crate::state::State] doesn't recognize an argument, it returns
the argument it got as a function argument, so that it can be retried
elsewhere. This enum provides a container for this function argument. This
allows the caller of the state method to opt-in or opt-out of actually
receiving this unused argument, depending on if it wants to retry it or not.

This type is mostly used by the [`debate`][crate] derives.
 */
pub enum DetectUnrecognized<A, E> {
    /// This argument wasn't recognized and can be retried
    Unrecognized(A),

    /// A different error occurred
    Error(E),
}

impl<'arg, E> parameter::Error<'arg> for DetectUnrecognized<(), E>
where
    E: parameter::Error<'arg>,
{
    #[inline]
    fn needs_arg() -> Self {
        Self::Error(E::needs_arg())
    }

    #[inline]
    fn got_arg(arg: &'arg Arg) -> Self {
        Self::Error(E::got_arg(arg))
    }

    #[inline]
    fn got_additional_instance() -> Self {
        Self::Unrecognized(())
    }

    #[inline]
    fn invalid_utf8(arg: &'arg Arg) -> Self {
        Self::Error(E::invalid_utf8(arg))
    }

    #[inline]
    fn parse_error(arg: &'arg str, msg: impl fmt::Display) -> Self {
        Self::Error(E::parse_error(arg, msg))
    }

    #[inline]
    fn byte_parse_error(arg: &'arg Arg, msg: impl fmt::Display) -> Self {
        Self::Error(E::byte_parse_error(arg, msg))
    }

    #[inline]
    fn should_be(argument: &'arg Arg, expected: &'static [&'static str]) -> Self {
        Self::Error(E::should_be(argument, expected))
    }

    #[inline]
    fn custom(msg: impl fmt::Display) -> Self {
        Self::Error(E::custom(msg))
    }
}

impl<'arg, A, E> state::Error<'arg, A> for DetectUnrecognized<A, E>
where
    E: state::Error<'arg, A>,
{
    type ParameterError = E::ParameterError;
    type FlagList = E::FlagList;

    #[inline]
    fn parameter(field: &'static str, error: Self::ParameterError) -> Self {
        Self::Error(E::parameter(field, error))
    }

    #[inline]
    fn unrecognized(argument: A) -> Self {
        Self::Unrecognized(argument)
    }

    #[inline]
    fn flattened(field: &'static str, error: Self) -> Self {
        // Currently we get rid of flattens for unrecognized. This makes sense
        // because un-recognition is a property of the entire state tree, not
        // any particular part of it.
        match error {
            Self::Unrecognized(arg) => Self::Unrecognized(arg),
            Self::Error(err) => Self::Error(E::flattened(field, err)),
        }
    }

    #[inline]
    fn unknown_subcommand(expected: &'static [&'static str]) -> Self {
        Self::Error(E::unknown_subcommand(expected))
    }

    #[inline]
    fn wrong_subcommand_for_argument(
        subcommand: &'static str,
        allowed: &'static [&'static str],
    ) -> Self {
        Self::Error(E::wrong_subcommand_for_argument(subcommand, allowed))
    }

    #[inline]
    fn conflicts_with_flags(flags: Self::FlagList) -> Self {
        Self::Error(E::conflicts_with_flags(flags))
    }

    #[inline]
    fn help_requested(req: HelpRequest) -> Self {
        Self::Error(E::help_requested(req))
    }
}

/**
A switch parameter that counts the number of times it appears on the command
line. Enables things like increasing verbosity levels via `-v`, `-vv`,
`-vvv`
*/
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Count {
    /// The number of times this switch appeared
    pub count: u32,
}

impl<'arg> Parameter<'arg> for Count {
    fn absent() -> Result<Self, RequiredError> {
        Ok(Self { count: 0 })
    }

    fn present<E: parameter::Error<'arg>>(_arg: impl ArgAccess<'arg>) -> Result<Self, E> {
        Ok(Self { count: 1 })
    }

    fn add_present<E: parameter::Error<'arg>>(
        &mut self,
        _arg: impl ArgAccess<'arg>,
    ) -> Result<(), E> {
        self.count = self
            .count
            .checked_add(1)
            .ok_or_else(|| E::custom("too many appearances (overflowed a u32)"))?;

        Ok(())
    }
}

impl help::ParameterUsage for Count {
    const VALUE: help::ParameterValueKind = help::ParameterValueKind::Flag;
    const REQUIREMENT: help::Requirement = help::Requirement::Optional;
    const REPETITION: help::Repetition = help::Repetition::Multiple;
}

/**
Input arguments are always raw byte slices; this function converts an argument
to a string and handles returning the appropriate
[parameter error][crate::parameter::Error]. This should be used in [`Value`]
and [`PositionalParameter`] implementations.
*/
pub fn arg_as_str<'arg, E>(arg: &'arg Arg) -> Result<&'arg str, E>
where
    E: parameter::Error<'arg>,
{
    from_utf8(arg.bytes()).map_err(|_err| E::invalid_utf8(arg))
}

// TODO: set-like helpers for NonEmpty<HashSet>, to avoid duplicate elements
// in the hash set.
/**
[`PositionalParameter`] wrapper for collection types that structurally
guarantees that at least one argument is present.

Collections can be empty, so their [`Parameter`] implementations return success
if no values were present on the command line. Wrapping them in this type (for
instance, `NonEmpty<String, Vec<String>>`) ensures that at least one argument
is present

*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NonEmpty<T, C> {
    pub first: T,
    pub rest: C,
}

impl<T, C> NonEmpty<T, C>
where
    for<'a> &'a C: IntoIterator<Item = &'a T>,
{
    pub fn iter(&self) -> NonEmptyIter<<&C as IntoIterator>::IntoIter> {
        NonEmptyIter {
            first: Some(&self.first),
            rest: self.rest.into_iter(),
        }
    }
}

impl<T, C> NonEmpty<T, C>
where
    for<'a> &'a mut C: IntoIterator<Item = &'a mut T>,
{
    pub fn iter_mut(&mut self) -> NonEmptyIter<<&mut C as IntoIterator>::IntoIter> {
        NonEmptyIter {
            first: Some(&mut self.first),
            rest: self.rest.into_iter(),
        }
    }
}

impl<T, C> Extend<T> for NonEmpty<T, C>
where
    C: Extend<T>,
{
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.rest.extend(iter);
    }
}

impl<C> Index<usize> for NonEmpty<C::Output, C>
where
    C: Index<usize>,
    C::Output: Sized,
{
    type Output = C::Output;

    fn index(&self, index: usize) -> &Self::Output {
        match index.checked_sub(1) {
            Some(index) => self.rest.index(index),
            None => &self.first,
        }
    }
}

impl<C> IndexMut<usize> for NonEmpty<C::Output, C>
where
    C: IndexMut<usize>,
    C::Output: Sized,
{
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        match index.checked_sub(1) {
            Some(index) => self.rest.index_mut(index),
            None => &mut self.first,
        }
    }
}

impl<'arg, T, C> PositionalParameter<'arg> for NonEmpty<T, C>
where
    T: Value<'arg>,
    C: Extend<T> + Default,
{
    fn arg<E: parameter::Error<'arg>>(argument: &'arg Arg) -> Result<Self, E> {
        T::from_arg(argument).map(|first| Self {
            first,
            rest: C::default(),
        })
    }

    fn add_arg<E: parameter::Error<'arg>>(&mut self, argument: &'arg Arg) -> Result<(), E> {
        T::from_arg(argument).map(|item| self.rest.extend([item]))
    }
}

impl<C: IntoIterator> IntoIterator for NonEmpty<C::Item, C> {
    type Item = C::Item;
    type IntoIter = NonEmptyIter<C::IntoIter>;

    fn into_iter(self) -> Self::IntoIter {
        NonEmptyIter {
            first: Some(self.first),
            rest: self.rest.into_iter(),
        }
    }
}

impl<'a, T, C> IntoIterator for &'a NonEmpty<T, C>
where
    &'a C: IntoIterator<Item = &'a T>,
{
    type Item = &'a T;
    type IntoIter = NonEmptyIter<<&'a C as IntoIterator>::IntoIter>;

    fn into_iter(self) -> Self::IntoIter {
        NonEmptyIter {
            first: Some(&self.first),
            rest: self.rest.into_iter(),
        }
    }
}

impl<'a, T, C> IntoIterator for &'a mut NonEmpty<T, C>
where
    &'a C: IntoIterator<Item = &'a mut T>,
{
    type Item = &'a mut T;
    type IntoIter = NonEmptyIter<<&'a C as IntoIterator>::IntoIter>;

    fn into_iter(self) -> Self::IntoIter {
        NonEmptyIter {
            first: Some(&mut self.first),
            rest: self.rest.into_iter(),
        }
    }
}

/// Iterator type for [`NonEmpty`] collections
pub struct NonEmptyIter<I: Iterator> {
    first: Option<I::Item>,
    rest: I,
}

impl<I: Iterator> Iterator for NonEmptyIter<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        match self.first.take() {
            Some(item) => Some(item),
            None => self.rest.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self.first.as_ref() {
            None => self.rest.size_hint(),
            Some(_) => {
                let (min, max) = self.rest.size_hint();
                (
                    min.saturating_add(1),
                    max.and_then(|max| max.checked_add(1)),
                )
            }
        }
    }

    fn last(self) -> Option<Self::Item>
    where
        Self: Sized,
    {
        match self.rest.last() {
            Some(last) => Some(last),
            None => self.first,
        }
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        match self.first.take() {
            None => self.rest.nth(n),
            Some(first) => match n.checked_sub(1) {
                None => Some(first),
                Some(n) => self.rest.nth(n),
            },
        }
    }

    fn fold<B, F>(self, init: B, mut f: F) -> B
    where
        Self: Sized,
        F: FnMut(B, Self::Item) -> B,
    {
        self.rest.fold(
            match self.first {
                Some(item) => f(init, item),
                None => init,
            },
            f,
        )
    }
}

impl<I: DoubleEndedIterator> DoubleEndedIterator for NonEmptyIter<I> {
    fn next_back(&mut self) -> Option<Self::Item> {
        match self.rest.next_back() {
            Some(item) => Some(item),
            None => self.first.take(),
        }
    }

    fn rfold<B, F>(self, init: B, mut f: F) -> B
    where
        Self: Sized,
        F: FnMut(B, Self::Item) -> B,
    {
        let accum = self.rest.rfold(init, &mut f);

        match self.first {
            None => accum,
            Some(first) => f(accum, first),
        }
    }
}

impl<I: FusedIterator> FusedIterator for NonEmptyIter<I> {}

impl<I: ExactSizeIterator> ExactSizeIterator for NonEmptyIter<I> {
    fn len(&self) -> usize {
        self.rest.len() + (self.first.is_some() as usize)
    }
}
