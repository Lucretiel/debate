use core::{
    convert::Infallible,
    fmt::{self, Write, write},
    marker::PhantomData,
};

use indent_write::{fmt::IndentWriter, indentable::Indentable};
use joinery::{Joinable, JoinableIterator};
use lazy_format::{lazy_format, make_lazy_format};

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

#[derive(Debug)]
pub struct ValueParameter<'a> {
    /// The name of the argument itself. Typically capitalized.
    pub metavariable: &'a str,

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

pub struct Descriptions<'a> {
    pub short: &'a str,
    pub long: &'a str,
}

///
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
        description: Descriptions,
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
        description: Descriptions,
    ) -> Result<(), Self::Err> {
        Ok(())
    }

    #[expect(unused_variables)]
    #[inline(always)]
    fn subcommand(
        &mut self,
        command: &str,
        description: Descriptions<'_>,
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
    pub fn new() -> Self {
        Self {
            phantom: PhantomData,
        }
    }

    pub fn typed<U: Usage>() -> UsageHelper<U> {
        UsageHelper::new()
    }
}

impl<T> Copy for UsageHelper<T> {}

impl<T> Clone for UsageHelper<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Usage> Default for UsageHelper<T> {
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

pub struct HelpRequestedError;

/// Entry point for the usage system. A `UsagePrinter` is passed in during
/// argument parsing and allows the
pub trait UsagePrinter {
    fn print_long_usage(
        self,
        description: &str,
        command: &SubcommandChain<'_>,
        usage: UsageHelper<impl Usage>,
    ) -> Result<Infallible, HelpRequestedError>;

    fn print_short_usage(
        self,
        description: &str,
        command: &SubcommandChain<'_>,
        usage: UsageHelper<impl Usage>,
    ) -> Result<Infallible, HelpRequestedError>;
}
