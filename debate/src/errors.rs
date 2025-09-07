use core::fmt::{self};

use crate::{build, from_args, help::HelpRequest, parameter, state};

/// A simple argument parsing error type that contains no data. Mostly used for
/// testing and code examples.
#[derive(Debug, Clone, Copy, Default)]
pub struct EmptyError;

impl fmt::Display for EmptyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "there was an error parsing command-line arguments")
    }
}

impl core::error::Error for EmptyError {}

impl<'arg> parameter::Error<'arg> for EmptyError {
    #[inline(always)]
    fn needs_arg() -> Self {
        Self
    }

    #[inline(always)]
    fn got_arg(_: &'arg debate_parser::Arg) -> Self {
        Self
    }

    #[inline(always)]
    fn got_additional_instance() -> Self {
        Self
    }

    #[inline(always)]
    fn invalid_utf8(_: &'arg debate_parser::Arg) -> Self {
        Self
    }

    #[inline(always)]
    fn parse_error(_: &str, _: impl core::fmt::Display) -> Self {
        Self
    }

    #[inline(always)]
    fn byte_parse_error(_: &'arg debate_parser::Arg, _: impl core::fmt::Display) -> Self {
        Self
    }

    #[inline(always)]
    fn should_be(_: &'arg debate_parser::Arg, _: &'static [&'static str]) -> Self {
        Self
    }

    #[inline(always)]
    fn custom(_: impl core::fmt::Display) -> Self {
        Self
    }
}

impl<'arg, A> state::Error<'arg, A> for EmptyError {
    type ParameterError = EmptyError;

    #[inline(always)]
    fn parameter(_: &'static str, _: Self::ParameterError) -> Self {
        Self
    }

    #[inline(always)]
    fn unrecognized(_: A) -> Self {
        Self
    }

    #[inline(always)]
    fn flattened(_: &'static str, _: Self) -> Self {
        Self
    }

    #[inline(always)]
    fn unknown_subcommand(_: &'static [&'static str]) -> Self {
        Self
    }

    #[inline(always)]
    fn wrong_subcommand_for_argument(_: &str, _: &[&'static str]) -> Self {
        Self
    }

    #[inline(always)]
    fn help_requested(_: HelpRequest) -> Self {
        Self
    }
}

impl build::Error for EmptyError {
    #[inline(always)]
    fn required_long(_field: &'static str, _long: &'static str, _short: Option<char>) -> Self {
        Self
    }

    #[inline(always)]
    fn required_short(_field: &'static str, _short: char) -> Self {
        Self
    }

    #[inline(always)]
    fn required_positional(_field: &'static str, _placeholder: &'static str) -> Self {
        Self
    }

    #[inline(always)]
    fn flattened(_: &'static str, _: Self) -> Self {
        Self
    }

    #[inline(always)]
    fn required_subcommand(_: &'static [&'static str]) -> Self {
        Self
    }

    #[inline(always)]
    fn custom(_: impl core::fmt::Display) -> Self {
        Self
    }
}

impl<'arg> from_args::Error<'arg> for EmptyError {
    type StateError<A> = EmptyError;

    #[inline(always)]
    fn positional(_: &'arg debate_parser::Arg, _: Self) -> Self {
        Self
    }

    #[inline(always)]
    fn long_with_argument(
        _: &'arg debate_parser::Arg,
        _: &'arg debate_parser::Arg,
        _: Self,
    ) -> Self {
        Self
    }

    #[inline(always)]
    fn long<A>(_: &'arg debate_parser::Arg, _: Self) -> Self {
        Self
    }

    #[inline(always)]
    fn short<A>(_: u8, _: Self) -> Self {
        Self
    }

    #[inline(always)]
    fn and(self, _: Self) -> Self {
        Self
    }
}

#[cfg(feature = "std")]
mod with_std {
    use std::{
        borrow::ToOwned,
        fmt::Display,
        string::{String, ToString},
    };

    use debate_parser::Arg;

    use crate::{build, from_args, help::HelpRequest, parameter, state};

    /// An error trying to parse a parameter from a `--flag` or positional
    /// argument. These error are produced from the primitive types, like
    /// bools and strings and vectors.
    #[derive(Debug, Clone)]
    pub enum ParameterError<'arg> {
        /// This parameter needs an argument and didn't get one
        NeedArgument,

        /// This parameter is flag; it got an argument and didn't want one
        FlagGotArgument(&'arg Arg),

        /// This parameter appeared more than once or too many times
        GotAdditionalInstance,

        /// Failed to parse the argument
        ParseError { arg: &'arg Arg, message: String },

        /// The argument should have been one of these (or, possibly, a case
        /// insensitive variation)
        ShouldBe {
            arg: &'arg Arg,
            expected: &'static [&'static str],
        },

        /// Something else went wrong
        Custom { message: String },
    }

    impl<'arg> parameter::Error<'arg> for ParameterError<'arg> {
        fn needs_arg() -> Self {
            Self::NeedArgument
        }

        fn got_arg(argument: &'arg Arg) -> Self {
            Self::FlagGotArgument(argument)
        }

        fn got_additional_instance() -> Self {
            Self::GotAdditionalInstance
        }

        fn invalid_utf8(arg: &'arg Arg) -> Self {
            Self::ParseError {
                arg,
                message: "argument wasn't valid utf-8".to_owned(),
            }
        }

        fn parse_error(arg: &'arg str, message: impl Display) -> Self {
            Self::ParseError {
                arg: Arg::new(arg.as_bytes()),
                message: message.to_string(),
            }
        }

        fn byte_parse_error(arg: &'arg Arg, message: impl Display) -> Self {
            Self::ParseError {
                arg,
                message: message.to_string(),
            }
        }

        fn should_be(argument: &'arg Arg, expected: &'static [&'static str]) -> Self {
            Self::ShouldBe {
                arg: argument,
                expected,
            }
        }

        fn custom(message: impl Display) -> Self {
            Self::Custom {
                message: message.to_string(),
            }
        }
    }

    /// Errors that occur when parsing a single item from the command-line
    /// arguments
    #[derive(Debug, Clone)]
    pub enum StateError<'arg> {
        /// The argument was matched with a typed parameter, but the type
        /// returned an error.
        Parameter {
            field: &'static str,
            error: ParameterError<'arg>,
        },

        /// The argument failed to match any known parameters
        Unrecognized,

        /// The argument was identified as a subcommand, but didn't match
        /// any known subcommands.
        UnknownSubcommand { expected: &'static [&'static str] },

        /// Similar to `Unrecognized`: The argument was unrecognized by
        /// *this* subcommand, but is possibly a valid argument for one of the
        /// other subcommands.
        WrongSubcommand {
            subcommand: &'static str,
            allowed: &'static [&'static str],
        },

        /// The argument was a request for a usage message
        HelpRequested(HelpRequest),
        // /// A nested error from a nested arguments struct
        // Flattened {
        //     field: &'static str,
        //     error: Box<Self>,
        // },
    }

    impl StateError<'_> {
        pub fn help_request(&self) -> Option<HelpRequest> {
            match *self {
                Self::HelpRequested(help) => Some(help),
                _ => None,
            }
        }
    }

    impl<'arg, A> state::Error<'arg, A> for StateError<'arg> {
        type ParameterError = ParameterError<'arg>;

        fn parameter(field: &'static str, error: ParameterError<'arg>) -> Self {
            Self::Parameter { field, error }
        }

        fn unrecognized(_: A) -> Self {
            Self::Unrecognized
        }

        fn flattened(_field: &'static str, error: Self) -> Self {
            error
        }

        fn unknown_subcommand(expected: &'static [&'static str]) -> Self {
            Self::UnknownSubcommand { expected }
        }

        fn wrong_subcommand_for_argument(
            subcommand: &'static str,
            allowed: &'static [&'static str],
        ) -> Self {
            Self::WrongSubcommand {
                subcommand,
                allowed,
            }
        }

        fn help_requested(request: HelpRequest) -> Self {
            Self::HelpRequested(request)
        }
    }

    /// A description of an argument that was found on the command line,
    /// exactly as we found it. Used for messages about unrecognized arguments,
    /// parse errors, and so on.
    #[derive(Debug, Clone)]
    pub enum ParameterSource<'arg> {
        Positional {
            arg: &'arg Arg,
        },
        Short {
            option: u8,
        },
        Long {
            option: &'arg Arg,
            argument: Option<&'arg Arg>,
        },
    }

    /// Filtered, user-printable identification for a particular parameter.
    #[derive(Debug, Clone)]
    pub enum FieldKind {
        Long(&'static str),
        Short(char),
        Positional { placeholder: &'static str },
    }

    /// Errors  that occur after all command line arguments have been parsed,
    /// when trying to assemble the final Args object. This is where things
    /// like absent required fields are detected.
    #[derive(Debug, Clone)]
    pub enum BuildError<'arg> {
        /// There was a state error when loading the an argument from the
        /// given source.
        Arg {
            source: ParameterSource<'arg>,
            error: StateError<'arg>,
        },

        /// This field was absent. The `kind` includes information a
        RequiredFieldAbsent {
            field: &'static str,
            kind: FieldKind,
        },

        /// One of these subcommands is required here
        RequiredSubcommand {
            expected: &'static [&'static str],
        },
        // Flattened {
        //     field: &'static str,
        //     error: Box<Self>,
        // },
        Custom(String),
    }

    impl<'arg> BuildError<'arg> {
        pub fn help_request(&self) -> Option<HelpRequest> {
            match *self {
                Self::Arg { ref error, .. } => error.help_request(),
                _ => None,
            }
        }

        fn rank(&self) -> u16 {
            // TODO: complete ranking of errors. Lower numbered ranks are
            // prioritized over higher ranking ones.
            0
        }
    }

    impl<'arg> build::Error for BuildError<'arg> {
        fn required_long(field: &'static str, long: &'static str, _short: Option<char>) -> Self {
            Self::RequiredFieldAbsent {
                field,
                kind: FieldKind::Long(long),
            }
        }

        fn required_short(field: &'static str, short: char) -> Self {
            Self::RequiredFieldAbsent {
                field,
                kind: FieldKind::Short(short),
            }
        }

        fn required_positional(field: &'static str, placeholder: &'static str) -> Self {
            Self::RequiredFieldAbsent {
                field,
                kind: FieldKind::Positional { placeholder },
            }
        }

        fn flattened(_field: &'static str, error: Self) -> Self {
            error
        }

        fn required_subcommand(expected: &'static [&'static str]) -> Self {
            Self::RequiredSubcommand { expected }
        }

        fn custom(message: impl Display) -> Self {
            Self::Custom(message.to_string())
        }
    }

    impl<'arg> from_args::Error<'arg> for BuildError<'arg> {
        type StateError<A> = StateError<'arg>;

        fn positional(arg: &'arg Arg, error: StateError<'arg>) -> Self {
            Self::Arg {
                source: ParameterSource::Positional { arg },
                error,
            }
        }

        fn long_with_argument(
            option: &'arg Arg,
            argument: &'arg Arg,
            error: Self::StateError<()>,
        ) -> Self {
            Self::Arg {
                source: ParameterSource::Long {
                    option,
                    argument: Some(argument),
                },
                error,
            }
        }

        fn long<A>(option: &'arg Arg, error: StateError<'arg>) -> Self {
            Self::Arg {
                source: ParameterSource::Long {
                    option,
                    argument: None,
                },
                error,
            }
        }

        fn short<A>(option: u8, error: StateError<'arg>) -> Self {
            Self::Arg {
                source: ParameterSource::Short { option },
                error,
            }
        }

        fn and(self, rhs: Self) -> Self {
            if self.rank() <= rhs.rank() { self } else { rhs }
        }
    }
}

#[cfg(feature = "std")]
pub use with_std::*;
