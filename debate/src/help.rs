use core::marker::PhantomData;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HelpRequest {
    /// There was a request for a succinct usage message, probably via `-h`
    Succinct,

    /// There was a request for a comprehensive usage message, probably
    /// via `--help`
    Full,
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
/// (`-short`, `--long0`)
#[derive(Debug, Clone, Copy)]
pub enum Tags<'a> {
    Long { long: &'a str },
    Short { short: char },
    LongShort { long: &'a str, short: char },
}

impl<'a> Tags<'a> {
    pub fn long(&self) -> Option<&'a str> {
        match self {
            Tags::Long { long } | Tags::LongShort { long, .. } => Some(long),
            Tags::Short { .. } => None,
        }
    }

    pub fn short(&self) -> Option<char> {
        match self {
            Tags::Short { short } | Tags::LongShort { short, .. } => Some(*short),
            Tags::Long { .. } => None,
        }
    }
}

pub trait Receiver {
    type Err;

    #[expect(unused_variables)]
    #[inline(always)]
    fn option(
        &mut self,
        tags: Tags<'_>,
        argument: Option<ValueParameter<'_>>,
        requirement: Requirement,
        repetition: Repetition,
        description: &str,
    ) -> Result<(), Self::Err> {
        Ok(())
    }

    #[expect(unused_variables)]
    #[inline(always)]
    fn positional(
        &mut self,
        argument: ValueParameter<'_>,
        requirement: Requirement,
        repetition: Repetition,
        description: &str,
    ) -> Result<(), Self::Err> {
        Ok(())
    }

    #[expect(unused_variables)]
    #[inline(always)]
    fn subcommand(
        &mut self,
        command: &str,
        description: Option<&str>,
        usage: UsageHelper<impl Usage>,
    ) -> Result<(), Self::Err> {
        Ok(())
    }

    #[expect(unused_variables)]
    #[inline(always)]
    fn group(&mut self, name: &str, usage: UsageHelper<impl Usage>) -> Result<(), Self::Err> {
        Ok(())
    }

    #[expect(unused_variables)]
    #[inline(always)]
    fn exclusive_group(
        &mut self,
        requirement: Requirement,
        usage: UsageHelper<impl Usage>,
    ) -> Result<(), Self::Err> {
        Ok(())
    }

    #[expect(unused_variables)]
    #[inline(always)]
    fn anonymous_group(&mut self, usage: UsageHelper<impl Usage>) -> Result<(), Self::Err> {
        Ok(())
    }
}

/// `trait` for argument parsers that can describe themselves for the purpose
/// of printing a usage message.
#[diagnostic::on_unimplemented(message = "TEST MESSAGE", label = "LABEL")]
pub trait Usage {
    fn describe<R>(receiver: &mut R) -> Result<(), R::Err>
    where
        R: Receiver;

    fn describe_deduplicated<R>(receiver: &mut R) -> Result<(), R::Err>
    where
        R: Receiver,
    {
        Self::describe(receiver)
    }
}

/// Type-wrapper for a type that implements `Usage`. `Usage`'s functions are
/// all static, rather than instance methods, so the `UsageHelper` allows
/// a `Usage` type to be passed as a "value" to the methods in [`Receiver`],
/// simplifying call sites.
///
/// I might get rid of this, we'll see.
pub struct UsageHelper<T> {
    phantom: PhantomData<T>,
}

impl<T: Usage> UsageHelper<T> {
    #[inline(always)]
    #[must_use]
    pub const fn new() -> Self {
        Self {
            phantom: PhantomData,
        }
    }
}

impl<T> Copy for UsageHelper<T> {}

impl<T> Clone for UsageHelper<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Usage> Default for UsageHelper<T> {
    #[inline(always)]
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Usage> UsageHelper<T> {
    #[inline(always)]
    pub fn describe<R>(self, receiver: &mut R) -> Result<(), R::Err>
    where
        R: Receiver,
    {
        T::describe(receiver)
    }

    #[inline(always)]
    pub fn describe_deduplicated<R>(self, receiver: &mut R) -> Result<(), R::Err>
    where
        R: Receiver,
    {
        T::describe_deduplicated(receiver)
    }
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
    /// This parameter behaves like a flake; it doesn't take any arguments
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
