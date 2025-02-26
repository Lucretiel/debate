#![no_std]

/*!
Low-level implementation of argument handling. Takes care of distinctions
between flags, options, and positionals, that sort of thing. No type handling
happens here. Usually this is too low level to use directly.
*/

mod populated_slice;

use ::core::fmt::{self, Debug, Write};

use populated_slice::PopulatedSlice;

/**
A single, raw argument passed in from the command line.

This type is used in two ways: to indicate long command line options, and to
indicate arguments themselves. For instance, given
`--target foo --path=bar input.txt`, `target`, `foo`, `path`, `bar`, and
`input.txt` would all be passed as [`Arg`] values to the relevant functions.

An [`Arg`] internally is just a byte slice, since that's what the OS gives us.
Callers can manually turn it into a [`str`] with [`from_utf8`][core::str::from_utf8],
and from there parse it however they need.
*/
#[derive(Clone, Copy)]
pub struct Arg<'arg>(&'arg [u8]);

impl<'arg> Arg<'arg> {
    pub fn bytes(&self) -> &'arg [u8] {
        self.0
    }
}

/**
Debug-print an arg. This implementation does its best to treat the arg as a
string, but includes non-utf-8 bytes in their hex representation as needed.
 */
impl Debug for Arg<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn write_bytes(f: &mut fmt::Formatter<'_>, bytes: &[u8]) -> fmt::Result {
            f.write_char('[')?;

            let mut bytes = bytes.iter().copied();

            if let Some(b) = bytes.next() {
                write!(f, "{b:#x}")?;
                bytes.try_for_each(|b| write!(f, ",{b:#x}"))?;
            }

            f.write_char(']')
        }

        self.0.utf8_chunks().enumerate().try_for_each(|(i, chunk)| {
            if i > 0 {
                write!(f, "..")?
            }

            let s = chunk.valid();
            let b = chunk.invalid();

            match (s, b) {
                (s, b"") => write!(f, "{s:?}"),
                ("", b) => write_bytes(f, b),
                (s, b) => {
                    write!(f, "{s:?}..")?;
                    write_bytes(f, b)
                }
            }
        })
    }
}

/**
The [`ArgumentsParser`] type operates by passing arguments it finds into a
[`Visitor`], to be handled.
 */
pub trait Visitor<'arg> {
    type Value;

    /// A positional parameter.
    fn visit_positional(self, argument: Arg<'arg>) -> Self::Value;

    /// A long option that definitely has an argument, because it was given
    /// as `--option=argument`
    fn visit_long_option(self, option: Arg<'arg>, argument: Arg<'arg>) -> Self::Value;

    /// A long option or flag, such as `--option`
    fn visit_long(self, option: Arg<'arg>, arg: impl ArgAccess<'arg>) -> Self::Value;

    /// A long option or flag, such as `-o`
    fn visit_short(self, option: u8, arg: impl ArgAccess<'arg>) -> Self::Value;
}

/**
[`ArgAccess`] allows a visitor to decide if a given parameter needs an argument,
based on the identity of the flag or option.

Consider `--foo bar`. Is this a pair of parameters (the flag `--foo` and the
positional parameter `bar`) or a single option `--foo bar` that takes an
argument? Similarly, `-ab foo` could be `-a b`, `foo`; or `-a`, `-b foo`; or
`-a`, `-b`, `foo`. The [`ArgumentsParser`] can't independently classify a given
argument, so instead, a visitor can request an argument via this trait only for
options that need them and the `ArgumentParser` takes care of the parsing logic
of actually determining where that argument comes from.
*/
pub trait ArgAccess<'arg>: Sized {
    /**
    Get an argument from the parser. This should only be called by options that
    need it; flags should simply ignore it, to ensure that the next command
    line argument can correctly be parsed independently.

    This returns [`None`] if all of the CLI arguments have been exhausted, or
    if there are known to only be positional parameters remaining (because
    a raw `--` was parsed at some point).
    */
    fn take(self) -> Option<Arg<'arg>>;
}

#[derive(Debug, Clone)]
enum State<'arg> {
    Ready,
    PositionalOnly,
    // TODO: reuse the better byte slice formatter here
    ShortInProgress(&'arg PopulatedSlice<u8>),
}

/**
An `ArgumentsParser` is the main entry point into `debate_parser`. It parses
arguments in each call to `next_arg`, sending those arguments to the given
[`Visitor`]. It handles distinguishing flags, options, and positionals; logic
related to how flags get their argument values, and the `--`

[debate-parser][crate] operates entirely on borrowed data, because we assume
that command-line arguments can be loaded early on in `main` and then handled
in a borrowed form for the rest of the program. The ubiquitous  `'arg` lifetime
refers to this borrowed command line data.
*/
#[derive(Debug, Clone)]
pub struct ArgumentsParser<'arg, I> {
    state: State<'arg>,
    args: I,
}

impl<'arg, I> ArgumentsParser<'arg, I>
where
    I: Iterator<Item = &'arg [u8]>,
{
    /**
    Create a new [`ArgumentsParser`] from an iterator of byte slices, where
    each byte slice is a single argument received from the command line. This
    list should *exclude* the name of the program, which is commonly passed as
    the first argument in the list.
     */
    #[inline]
    #[must_use]
    pub fn new(args: impl IntoIterator<IntoIter = I>) -> Self {
        Self {
            state: State::Ready,
            args: args.into_iter(),
        }
    }

    /// Put `self` into a `PositionalOnly` state, then process a positional
    /// argument
    #[inline]
    fn positional_only_arg<V>(&mut self, visitor: V) -> Option<V::Value>
    where
        V: Visitor<'arg>,
    {
        debug_assert!(!matches!(self.state, State::ShortInProgress(_)));

        self.state = State::PositionalOnly;
        self.args
            .next()
            .map(Arg)
            .map(|arg| visitor.visit_positional(arg))
    }

    /// Put `self` into a `Ready` state, then return a ShortArgAccess
    #[inline]
    fn standard_arg(&mut self) -> StandardArgAccess<'_, 'arg, I> {
        debug_assert!(!matches!(self.state, State::PositionalOnly));

        self.state = State::Ready;
        StandardArgAccess { parent: self }
    }

    /// Put `self` into a `ShortInProgress` state, then return a ShortArgAccess.
    /// `short` must be non-empty.
    #[inline]
    fn short_arg(&mut self, short: &'arg PopulatedSlice<u8>) -> ShortArgAccess<'_, 'arg> {
        debug_assert!(!matches!(self.state, State::PositionalOnly));

        self.state = State::ShortInProgress(short);
        ShortArgAccess {
            short: short.get(),
            state: &mut self.state,
        }
    }

    /// Handle getting the argument for a `-s` short option. If there is
    /// remaining content in the short, it's a candidate for the argument;
    /// otherwise, the next argument in the input args is the candidate.
    #[inline]
    fn handle_short_argument<V>(&mut self, short: &'arg PopulatedSlice<u8>, visitor: V) -> V::Value
    where
        V: Visitor<'arg>,
    {
        let (&option, short) = short.split_first();

        match PopulatedSlice::new(short) {
            None => visitor.visit_short(option, self.standard_arg()),
            Some(short) => visitor.visit_short(option, self.short_arg(short)),
        }
    }

    pub fn next_arg<V>(&mut self, visitor: V) -> Option<V::Value>
    where
        V: Visitor<'arg>,
    {
        match self.state {
            State::Ready => match self.args.next()? {
                b"--" => self.positional_only_arg(visitor),
                argument => Some(match argument {
                    [b'-', b'-', option @ ..] => match split_once(option, b'=') {
                        Some((option, argument)) => {
                            visitor.visit_long_option(Arg(option), Arg(argument))
                        }
                        None => visitor.visit_long(Arg(option), self.standard_arg()),
                    },
                    [b'-', short @ ..] => match PopulatedSlice::new(short) {
                        None => visitor.visit_positional(Arg(b"-")),
                        Some(short) => self.handle_short_argument(short, visitor),
                    },
                    positional => visitor.visit_positional(Arg(positional)),
                }),
            },
            State::PositionalOnly => self.positional_only_arg(visitor),
            State::ShortInProgress(short) => Some(self.handle_short_argument(short, visitor)),
        }
    }
}

/// ArgAccess implementation that gets the next argument from the list.
/// Handles logic around `--` PositionalOnly parameters.
struct StandardArgAccess<'a, 'arg, I> {
    parent: &'a mut ArgumentsParser<'arg, I>,
}

impl<'arg, I> ArgAccess<'arg> for StandardArgAccess<'_, 'arg, I>
where
    I: Iterator<Item = &'arg [u8]>,
{
    fn take(self) -> Option<Arg<'arg>> {
        match self.parent.args.next()? {
            b"--" if !matches!(self.parent.state, State::PositionalOnly) => {
                self.parent.state = State::PositionalOnly;
                None
            }
            arg => Some(Arg(arg)),
        }
    }
}

/// ArgAccess implementation that gets the remainder of a short argument.
/// Handles things like `-ovalue`, which is equivelent to `-o value`.
struct ShortArgAccess<'a, 'arg> {
    short: &'arg [u8],
    state: &'a mut State<'arg>,
}

impl<'arg> ArgAccess<'arg> for ShortArgAccess<'_, 'arg> {
    fn take(self) -> Option<Arg<'arg>> {
        debug_assert!(
            matches!(*self.state, State::ShortInProgress(short) if short.get() == self.short)
        );

        *self.state = State::Ready;
        Some(Arg(self.short))
    }
}

fn split_once(input: &[u8], delimiter: u8) -> Option<(&[u8], &[u8])> {
    memchr::memchr(delimiter, input).map(|i| (&input[..i], &input[i + 1..]))
}
