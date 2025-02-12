/*!
Low-level implementation of argument handling. Takes care of distinctions
between flags, options, and positionals, that sort of thing. No type handling
happens here. Usually this is too low level to use directly.
*/

use core::fmt::Debug;
use std::fmt::{self, Write};

/**
The primitive type of arguments in `debate` is `&[u8]`, since that's what the
OS provides. Conversion to &str, and from there to parsed types, is handled
separately.
 */
#[derive(Clone, Copy)]
pub struct Arg<'a>(&'a [u8]);

impl<'a> Arg<'a> {
    pub fn bytes(&self) -> &'a [u8] {
        self.0
    }
}

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

pub trait ArgAccess<'arg>: Sized {
    fn take(self) -> Option<Arg<'arg>>;
}

#[derive(Debug, Clone)]
enum State<'arg> {
    Ready,
    PositionalOnly,
    ShortInProgress(&'arg [u8]),
}

#[derive(Debug, Clone)]
pub struct ArgumentsParser<'arg, I> {
    state: State<'arg>,
    args: I,
}

impl<'arg, I> ArgumentsParser<'arg, I>
where
    I: Iterator<Item = &'arg [u8]>,
{
    #[inline]
    #[must_use]
    pub fn new(args: I) -> Self {
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
    fn short_arg(&mut self, short: &'arg [u8]) -> ShortArgAccess<'_, 'arg> {
        debug_assert!(!matches!(self.state, State::PositionalOnly));

        self.state = State::ShortInProgress(short);
        ShortArgAccess {
            short,
            state: &mut self.state,
        }
    }

    /// Handle getting the argument for a `-s` short option. If there is
    /// remaining content in the short, it's a candidate for the argument;
    /// otherwise, the next argument in the input args is the candidate.
    #[inline]
    fn handle_short_argument<V>(&mut self, option: u8, short: &'arg [u8], visitor: V) -> V::Value
    where
        V: Visitor<'arg>,
    {
        match short.is_empty() {
            true => visitor.visit_short(option, self.standard_arg()),
            false => visitor.visit_short(option, self.short_arg(short)),
        }
    }

    pub fn next_arg<V>(&mut self, visitor: V) -> Option<V::Value>
    where
        V: Visitor<'arg>,
    {
        match self.state {
            State::Ready => match self.args.next()? {
                b"--" => self.positional_only_arg(visitor),
                [b'-', b'-', option @ ..] => Some(match split_once(option, b'=') {
                    Some((option, argument)) => {
                        visitor.visit_long_option(Arg(option), Arg(argument))
                    }
                    None => visitor.visit_long(Arg(option), self.standard_arg()),
                }),
                [b'-', short @ ..] => Some(match short.split_first() {
                    None => visitor.visit_positional(Arg(b"-")),
                    Some((&option, short)) => self.handle_short_argument(option, short, visitor),
                }),
                positional => Some(visitor.visit_positional(Arg(positional))),
            },
            State::PositionalOnly => self.positional_only_arg(visitor),
            State::ShortInProgress(short) => Some(match short.split_first() {
                None => panic!("short arg should always have at least one element"),
                Some((&option, short)) => self.handle_short_argument(option, short, visitor),
            }),
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
        *self.state = State::Ready;
        Some(Arg(self.short))
    }
}

fn split_once(input: &[u8], delimiter: u8) -> Option<(&[u8], &[u8])> {
    memchr::memchr(delimiter, input).map(|i| (&input[..i], &input[i + 1..]))
}
