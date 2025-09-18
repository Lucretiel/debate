/*!
Types and traits related to structured usage information about command line
arguments.

The primary entry point to these usage messages is the [`Usage`] trait, which
includes all the necessary information to print usage messages for the user.
[`debate`][crate] includes its own usage message & error printers, but feel
free to write your own if you want your own styles.
*/

use crate::Tags;

/**
Enum indicating whether the user requested a succinct help message, usually
via `-h`, or a comprehensive usage message, usually via `--help`
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HelpRequest {
    Succinct,
    Full,
}

/// A description of something, like a parameter or subcommand.
#[derive(Debug, Clone, Copy)]
pub struct Description<'a> {
    /**
    A succinct description of this thing, usually only a sentence or short
    paragraph long.
    */
    pub succinct: &'a str,

    /**
    A full description of this thing. Usually the succinct text is a prefix.
    */
    pub full: &'a str,
}

impl<'a> Description<'a> {
    /// Create a new description where the short and long text is the same
    #[inline]
    #[must_use]
    pub const fn new(description: &'a str) -> Self {
        Self {
            succinct: description,
            full: description,
        }
    }

    /// Get the relevant description string, based on the help request.
    #[inline]
    #[must_use]
    pub const fn get(&self, style: HelpRequest) -> &'a str {
        match style {
            HelpRequest::Succinct => self.succinct,
            HelpRequest::Full => self.full,
        }
    }
}

/**
For a parameter that takes a value, this describes some aspects of that
value.
*/
#[non_exhaustive]
#[derive(Debug, Clone, Copy)]
pub struct ValueParameter<'a> {
    /**
    The display name of the value itself. Typically capitalized.

    `[--option VALUE]`, the placeholder is `VALUE`
    */
    pub placeholder: &'a str,

    /**
    If given, the set of possible values that this value can be.

    For instance, `--direction (up|down|left|right)`
    */
    pub values: Option<&'a [&'a str]>,
}

/**
A comprehensive description of the CLI parameters for a given
[`FromArgs`][crate::FromArgs] type. A type's [`Usage`] is used to create a
`--help` usage message. This trait is usually derived.
*/
pub trait Usage {
    /// The name of the command. Can be replaced with `argv[0]`.
    const NAME: &'static str;

    /// The top-level text description of the whole command.
    const DESCRIPTION: Description<'static>;

    /// The items (parameters, subcommands, and so on) in this command.
    const ITEMS: UsageItems<'static>;
}

/**
The set of items associated with a [`Usage`] implementation, or something
similar.
*/
#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum UsageItems<'a> {
    /**
    This `Usage` describes an ordinary set of parameters (flags and positionals
    and so on)
    */
    Parameters { parameters: &'a [Parameter<'a>] },

    /// This `Usage` describes a set of subcommands
    Subcommands {
        /// Indicates if a subcommand is required
        requirement: Requirement,

        /// The subcommands themselves
        commands: &'a [Subcommand<'a>],
    },

    /**
    This usage describes mutually sets of flags, where exactly one set of flags
    must be present.
    */
    Exclusive {
        /**
        Indicates if at least one group is required. Note that individual flags
        within groups might have their own requirements.
        */
        requirement: Requirement,

        /**
        The groups of flags. Each child slice here represents a mutually
        exclusive separate group of flags.
        */
        groups: &'a [&'a [ParameterFlag<'a>]],

        /**
        The complete set of flags. Flags may appear in more than one group (so
        long as they're identical); so this list deduplicates them.
        */
        all_flags: &'a [ParameterFlag<'a>],
    },
}

/// Usage information for a single parameter
#[derive(Debug, Clone)]
pub enum Parameter<'a> {
    /// This parameter is a `--option`
    Flag(ParameterFlag<'a>),

    /// This is a positional parameter
    Positional(ParameterPositional<'a>),

    /// This is a group containing its own parameters or subcommands
    Group(ParameterSubgroup<'a>),
}

impl<'a> Parameter<'a> {
    /// If this is a [`Parameter::Flag`], get the [`ParameterFlag`]
    pub const fn as_option(&self) -> Option<&ParameterFlag<'a>> {
        match self {
            Parameter::Flag(opt) => Some(opt),
            _ => None,
        }
    }

    /// If this is a [`Parameter::Positional`], get the [`ParameterPositional`]
    pub const fn as_positional(&self) -> Option<&ParameterPositional<'a>> {
        match self {
            Parameter::Positional(pos) => Some(pos),
            _ => None,
        }
    }

    /// If this is a [`Parameter::Group`], get the [`ParameterSubgroup`]
    pub const fn as_subgroup(&self) -> Option<&ParameterSubgroup<'a>> {
        match self {
            Parameter::Group(group) => Some(group),
            _ => None,
        }
    }
}

/// Details about a specific flag parameter.
#[derive(Debug, Clone)]
pub struct ParameterFlag<'a> {
    /// The description text for this flag.
    pub description: Description<'a>,

    /// Whether this flag is required or optional.
    pub requirement: Requirement,

    /// Whether this flag can appear more than once.
    pub repetition: Repetition,

    /**
    If this flag takes an argument, information about the argument it takes,
    such as the placeholder string.
    */
    pub argument: Option<ValueParameter<'a>>,

    /// The `-short` or `--long` tag associated with this flag.
    pub tags: Tags<'a>,
}

/// Details about a specific positional parameter
#[derive(Debug, Clone)]
pub struct ParameterPositional<'a> {
    /// The description text for this parameter.
    pub description: Description<'a>,

    /// Whether this parameter is required or optional.
    pub requirement: Requirement,

    /// Whether this parameter can appear more than once.
    pub repetition: Repetition,

    /**
    Information about the value this parameter takes, such as its placeholder.
    */
    pub argument: ValueParameter<'a>,
}

/**
Details about a subgroup of additional parameters. Subgroups allow parameters
to be nested, and additionally allow different kinds of parameter sets to
coexist. For instance, a struct of global flags will use a subgroup to express
any subcommands it wants.
*/
#[derive(Debug, Clone)]
pub struct ParameterSubgroup<'a> {
    /// The description text for this group. Not usually printed.
    pub description: Description<'a>,

    /**
    The identifier for this group. Not usually shown to the user in a
    usage message. I'd prefer to omit it entirely, but we use it to help
    look up the usage messages of individual subcommands (by traversing
    nested subgroups).
    */
    pub id: &'a str,

    /**
    When used as a group of items, the name of the group we show to the user.
    Usually Title Cased.
    */
    pub title: &'a str,

    /**
    When used as a placeholder, the name of the group we show to the user.
    Usually SHOUTY_SNAKE_CASED. Generally only used to refer to a subcommand
    in a synopsis.
    */
    pub placeholder: &'a str,

    /// The contents of this group.
    pub contents: UsageItems<'a>,
}

/// Details about an individual subcommand, such as `build` or `test`.
#[derive(Debug, Clone)]
pub struct Subcommand<'a> {
    /// The command itself
    pub command: &'a str,

    /// Description text for this command
    pub description: Description<'a>,

    /*
    Arguments and flags and so on that are specific to this command. Generally
    these can only appear after the command itself on the command-line.
    */
    pub usage: UsageItems<'a>,
}

/**
Information for a specific parameter. This should be derived or implemented
on any type that implements [`Parameter`][crate::parameter::Parameter], so that
it can be described correctly in usage messages.
*/
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
    /// This parameter behaves like a flag; it doesn't take any arguments.
    Flag,

    /// This parameter takes argument(s).
    Value,

    /// This parameter takes arguments, which should be one of these specific
    /// values.
    OneOf(&'static [&'static str]),
}

impl ParameterValueKind {
    /**
    Combine this [`ParameterValueKind`], which is associated with a particular
    parameter type (such as [`u32`]), with a placeholder string, to form
    a [`ValueParameter`]. We assume that this [`ParameterValueKind`] is not
    [`Flag`][ParameterValueKind::Flag].
    */
    pub const fn as_value_parameter<'a>(&'a self, placeholder: &'a str) -> ValueParameter<'a> {
        ValueParameter {
            placeholder,
            values: match *self {
                Self::OneOf(values) => Some(values),
                _ => None,
            },
        }
    }

    /**
    Combine this [`ParameterValueKind`], which is associated with a particular
    parameter type (such as [`bool`]), with a placeholder string, to form
    a [`ValueParameter`]. Returns [`None`] if this is a
    [`Flag`][ParameterValueKind::Flag].
    */
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

/// Information about whether a parameter must appear on the command line.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Requirement {
    /// The parameter is optional; it may be omitted
    Optional,

    /// The parameter must appear on the command line
    Mandatory,
}

/**
Information about whether a parameter may appear more than once on the command
line. Note that a mandatory parameter must always appear at *least* once.
Additionally note that some parameters allow subsequent instances to override
earlier instances; these parameters as still considered non-repeating.
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Repetition {
    /// This parameter should only appear at most once on the command line
    Single,

    /// This parameter may appear more than once on the command line
    Multiple,
}
