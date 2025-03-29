use core::{convert::Infallible, marker::PhantomData};

use crate::state::SubcommandChain;

#[derive(Debug, Clone, Copy)]
pub enum Requirement {
    Optional,
    Mandatory,
}

#[derive(Debug, Clone, Copy)]
pub enum Repetition {
    Single,
    Multiple,
}

#[derive(Debug, Clone, Copy)]
pub struct ValueParameter<'a> {
    /// The name of the argument itself. Typically capitalized.
    ///
    /// `[--option VALUE]`, the placeholder is `VALUE`
    pub placeholder: &'a str,

    /// If given, the set of possible values that this argument can be.
    pub values: Option<&'a [&'a str]>,
}

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

/**
Entry point for the usage system. A `UsagePrinter` is passed in during argument
parsing and allows the.

The methods on this trait can only possibly return an error. This is a hint
that a typical `UsagePrinter` will actually print
*/
pub trait UsagePrinter {
    fn print_long_usage(
        self,
        description: &str,
        command: &SubcommandChain<'_>,
        usage: UsageHelper<impl Usage>,
    );

    fn print_short_usage(
        self,
        description: &str,
        command: &SubcommandChain<'_>,
        usage: UsageHelper<impl Usage>,
    );
}

// TODO: consider a separate PositionalUsage trait?

pub trait ParameterUsage {
    const VALUE: ParameterValueKind;
    const REQUIREMENT: Requirement;
    const REPETITION: Repetition;
}

#[derive(Debug, Clone, Copy)]
pub enum ParameterValueKind {
    Flag,
    Value,
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
