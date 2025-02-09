/*!
Low-level implementation of argument handling. Takes care of distinctions
between flags, options, and positionals, that sort of thing. No type handling
happens here. Usually this is too low level to use directly.
*/

use core::fmt::Debug;

/**
The primitive type of arguments in `debate` is `&[u8]`, since that's what the
OS provides. Conversion to &str, and from there to parsed types, is handled
separately.
 */
#[derive(Debug, Clone, Copy)]
pub struct Arg<'a>(&'a [u8]);

impl<'a> Arg<'a> {
    pub fn bytes(&self) -> &'a [u8] {
        self.0
    }
}

// TODO: better Debug impl

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
    fn arg(self) -> Option<Arg<'arg>>;
}

enum State<'arg> {
    Ready,
    PositionalOnly,
    ShortInProgress(&'arg [u8]),
}

pub struct Arguments<'arg, I> {
    state: State<'arg>,
    args: I,
}

impl<'arg, I> Arguments<'arg, I>
where
    I: Iterator<Item = &'arg [u8]>,
{
    pub fn new(args: I) -> Self {
        Arguments {
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

    /// Put `self` into a `ShortInProgress` state, then return a ShortArgAccess
    #[inline]
    fn short_arg(&mut self, short: &'arg [u8]) -> ShortArgAccess<'_, 'arg> {
        debug_assert!(!matches!(self.state, State::PositionalOnly));

        self.state = State::ShortInProgress(short);
        ShortArgAccess {
            short,
            state: &mut self.state,
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
                    Some((&option, [])) => visitor.visit_short(option, self.standard_arg()),
                    Some((&option, short)) => visitor.visit_short(option, self.short_arg(short)),
                }),
                positional => Some(visitor.visit_positional(Arg(positional))),
            },
            State::PositionalOnly => self.positional_only_arg(visitor),
            State::ShortInProgress(short) => Some(match short.split_first() {
                None => panic!("short arg should always have at least one element"),
                Some((&option, [])) => visitor.visit_short(option, self.standard_arg()),
                Some((&option, short)) => visitor.visit_short(option, self.short_arg(short)),
            }),
        }
    }
}

struct StandardArgAccess<'a, 'arg, I> {
    parent: &'a mut Arguments<'arg, I>,
}

impl<'arg, I> ArgAccess<'arg> for StandardArgAccess<'_, 'arg, I>
where
    I: Iterator<Item = &'arg [u8]>,
{
    fn arg(self) -> Option<Arg<'arg>> {
        match self.parent.args.next()? {
            b"--" => {
                self.parent.state = State::PositionalOnly;
                None
            }
            arg => Some(Arg(arg)),
        }
    }
}

struct ShortArgAccess<'a, 'arg> {
    short: &'arg [u8],
    state: &'a mut State<'arg>,
}

impl<'arg> ArgAccess<'arg> for ShortArgAccess<'_, 'arg> {
    fn arg(self) -> Option<Arg<'arg>> {
        *self.state = State::Ready;
        Some(Arg(self.short))
    }
}

fn split_once(input: &[u8], delimiter: u8) -> Option<(&[u8], &[u8])> {
    memchr::memchr(delimiter, input).map(|i| (&input[..i], &input[i + 1..]))
}
