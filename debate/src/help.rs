use core::marker::PhantomData;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HelpRequest {
    /// There was a request for a succinct usage message, probably via `-h`
    Succinct,

    /// There was a request for a comprehensive usage message, probably
    /// via `--help`
    Full,
}

/// A description of something, like a parameter or subcommand
#[derive(Debug, Clone, Copy)]
pub struct Description<'a> {
    /// A succinct description of this thing, usually only a sentence long
    pub short: &'a str,

    /// A full description of this thing
    pub long: &'a str,
}

impl<'a> Description<'a> {
    /// Create a new description where the short and long text is the same
    #[inline]
    #[must_use]
    pub const fn new(description: &'a str) -> Self {
        Self {
            short: description,
            long: description,
        }
    }
}

/// Description of a single value accepted as an argument (positional or option)
#[derive(Debug, Clone, Copy)]
pub struct ValueParameter<'a> {
    /// The name of the argument itself. Typically capitalized.
    ///
    /// `[--option VALUE]`, the placeholder is `VALUE`
    pub placeholder: &'a str,

    /// If given, the set of possible values that this argument can be.
    pub values: Option<&'a [&'a str]>,
}

/// The set of tags that identify a particular option or tag
/// (`-short`, `--long`)
#[derive(Debug, Clone, Copy)]
pub enum Tags<'a> {
    Long { long: &'a str },
    Short { short: char },
    LongShort { long: &'a str, short: char },
}

impl<'a> Tags<'a> {
    #[inline]
    #[must_use]
    pub const fn long(&self) -> Option<&'a str> {
        match self {
            Tags::Long { long } | Tags::LongShort { long, .. } => Some(long),
            Tags::Short { .. } => None,
        }
    }

    #[inline]
    #[must_use]
    pub const fn short(&self) -> Option<char> {
        match self {
            Tags::Short { short } | Tags::LongShort { short, .. } => Some(*short),
            Tags::Long { .. } => None,
        }
    }
}

pub trait Usage {
    /// The name of the command.
    const NAME: &'static str;

    /// The top-level description of the whole command.
    const DESCRIPTION: Description<'static>;

    /// The items (parameters, subcommands, and so on) in this command.
    const ITEMS: UsageItems<'static>;
}

/// The set of items associated with a `Usage` implementation, or something
/// similar.
#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum UsageItems<'a> {
    /// This `Usage` describes an ordinary set of parameters (flags and
    /// positionals and so on)
    Parameters { parameters: &'a [Parameter<'a>] },

    /// This `Usage` describes a set of subcommands
    Subcommands {
        /// Indicates if a subcommand is required
        requirement: Requirement,

        /// The subcommands themselves
        commands: &'a [Subcommand<'a>],
    },

    /// This usage describes mutually sets of flags, where exactly one set of
    /// flags must be present
    Exclusive {
        /// The groups of flags. Each child slice here represents a mutually
        /// exclusive separate group of flags.
        groups: &'a [&'a [ParameterOption<'a>]],
    },
}

#[derive(Debug, Clone)]
pub enum Parameter<'a> {
    Option(ParameterOption<'a>),
    Positional {
        description: Description<'a>,
        requirement: Requirement,
        repetition: Repetition,
        argument: ValueParameter<'a>,
    },
    Group {
        description: Description<'a>,
        name: Option<&'a str>,
        contents: UsageItems<'a>,
    },
}

#[derive(Debug, Clone)]
pub struct ParameterOption<'a> {
    pub description: Description<'a>,
    pub requirement: Requirement,
    pub repetition: Repetition,
    pub argument: Option<ValueParameter<'a>>,
    pub tags: Tags<'a>,
}

#[derive(Debug, Clone)]
pub struct Subcommand<'a> {
    pub command: &'a str,
    pub description: Description<'a>,
    pub usage: &'a [Parameter<'a>],
}

// TODO: error implementation here
pub struct HelpRequestedError;

// TODO: consider a separate PositionalUsage trait?

/// Information for a specific parameter. This should be derived or implemented
/// on any type that implements [`Parameter`][crate::parameter::Parameter],
/// so that it can be described correctly in usage messages.
pub trait ParameterUsage {
    /// The nature of the value this takes as an argument, if any.
    const VALUE: ParameterValueKind;

    /// Whether or not this parameter must appear at least once.
    const REQUIREMENT: Requirement;

    /// Whether or not this parameter may appear more than once.
    const REPETITION: Repetition;
}

/// Description of the nature of the value(s) a parameter takes as arguments
/// on the command line, if any.
#[derive(Debug, Clone, Copy)]
pub enum ParameterValueKind {
    /// This parameter behaves like a flag; it doesn't take any arguments
    Flag,

    /// This parameter takes arguments
    Value,

    /// This parameter takes arguments, which should be one of these specific
    /// values
    OneOf(&'static [&'static str]),
}

impl ParameterValueKind {
    pub const fn as_value_parameter<'a>(&'a self, placeholder: &'a str) -> ValueParameter<'a> {
        ValueParameter {
            placeholder,
            values: match *self {
                Self::OneOf(values) => Some(values),
                _ => None,
            },
        }
    }

    pub const fn as_maybe_value_parameter<'a>(
        &'a self,
        placeholder: &'a str,
    ) -> Option<ValueParameter<'a>> {
        match *self {
            ParameterValueKind::Flag => None,
            ParameterValueKind::Value => Some(ValueParameter {
                placeholder,
                values: None,
            }),
            ParameterValueKind::OneOf(values) => Some(ValueParameter {
                placeholder,
                values: Some(values),
            }),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Requirement {
    Optional,
    Mandatory,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Repetition {
    Single,
    Multiple,
}
