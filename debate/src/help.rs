use core::{
    convert::Infallible,
    fmt::{self, Write, write},
    marker::PhantomData,
};

use indent_write::{fmt::IndentWriter, indentable::Indentable};
use joinery::{Joinable, JoinableIterator};
use lazy_format::{lazy_format, make_lazy_format};

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
pub struct Argument<'a> {
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

pub trait Receiver {
    type Err;

    #[expect(unused_variables)]
    #[inline(always)]
    fn option(
        &mut self,
        tags: Tags<'_>,
        argument: Option<Argument<'_>>,
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
        argument: Argument<'_>,
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
    fn describe<R>(self, receiver: &mut R) -> Result<(), R::Err>
    where
        R: Receiver,
    {
        T::describe(receiver)
    }

    #[inline(always)]
    fn describe_deduplicated<R>(self, receiver: &mut R) -> Result<(), R::Err>
    where
        R: Receiver,
    {
        T::describe_deduplicated(receiver)
    }
}

pub fn print_long_description(
    dest: &mut impl fmt::Write,
    usage: UsageHelper<impl Usage>,
) -> fmt::Result {
    let mut presence = Presence::default();
    match usage.describe_deduplicated(&mut presence) {
        Ok(()) => {}
    };
    let presence = presence;

    if presence.subcommands {
        writeln!(dest, "Commands:")?;
        usage.describe_deduplicated(&mut CommandPrinter {
            dest: IndentWriter::new("  ", &mut *dest),
        })?;
    }

    if presence.options {
        writeln!(dest, "Options:")?;
        usage.describe_deduplicated(&mut OptionPrinter {
            dest: IndentWriter::new("  ", &mut *dest),
        })?;
    }

    if presence.positionals {
        writeln!(dest, "Arguments:")?;
        usage.describe_deduplicated(&mut PositionalPrinter {
            dest: IndentWriter::new("  ", &mut *dest),
        })?;
    }

    if presence.groups {
        usage.describe_deduplicated(&mut GroupPrinter { dest: &mut *dest })?;
    }

    Ok(())
}

fn print_long_option(
    dest: &mut impl fmt::Write,
    tags: &Tags<'_>,
    argument: Option<&Argument<'_>>,
    _requirement: Requirement,
    _repetition: Repetition,
    description: Descriptions<'_>,
) -> fmt::Result {
    let tags = lazy_format!(match (tags) {
        Tags::LongShort { short, long } => "-{short}, --{long}",
        Tags::Short { short } => "-{short}",
        Tags::Long { long } => "    --{long}",
    });

    let metavariable = argument.map(|arg| arg.metavariable);

    let header = lazy_format!(match (metavariable) {
        Some(name) => ("{tags} <{name}>"),
        None => "{tags}",
    });

    let description = description.long.indented("        ");

    write!(dest, "{header}\n{description}\n\n")
}

fn print_long_positional(
    dest: &mut impl fmt::Write,
    argument: &Argument<'_>,
    requirement: Requirement,
    repetition: Repetition,
    description: Descriptions<'_>,
) -> fmt::Result {
    let name = argument.metavariable;

    let header = lazy_format!(match (requirement) {
        Requirement::Mandatory => "<{name}>",
        Requirement::Optional => "[{name}]",
    });

    let header = lazy_format!(match (repetition) {
        Repetition::Single => "{header}",
        Repetition::Multiple => "{header}...",
    });

    let description = description.long.indented("        ");

    write!(dest, "{header}\n{description}\n\n")
}

fn print_command_description(
    dest: &mut impl fmt::Write,
    command: &str,
    description: Descriptions<'_>,
) -> fmt::Result {
    let description = description.long.indented("  ");
    write!(dest, "{command}\n{description}\n\n")
}

fn print_group(
    dest: &mut impl fmt::Write,
    name: &str,
    usage: UsageHelper<impl Usage>,
) -> fmt::Result {
    writeln!(dest, "{name}:")?;

    usage.describe_deduplicated(&mut GroupContentsPrinter {
        dest: IndentWriter::new("  ", dest),
    })
}

#[derive(Default)]
struct Presence {
    subcommands: bool,
    options: bool,
    positionals: bool,
    groups: bool,
}

impl Receiver for Presence {
    type Err = Infallible;

    #[inline(always)]
    fn option(
        &mut self,
        _tags: Tags<'_>,
        _argument: Option<Argument<'_>>,
        _requirement: Requirement,
        _repetition: Repetition,
        _description: Descriptions,
    ) -> Result<(), Self::Err> {
        self.options = true;
        Ok(())
    }

    #[inline(always)]
    fn positional(
        &mut self,
        _argument: Argument<'_>,
        _requirement: Requirement,
        _repetition: Repetition,
        _description: Descriptions,
    ) -> Result<(), Self::Err> {
        self.positionals = true;
        Ok(())
    }

    #[inline(always)]
    fn subcommand(
        &mut self,
        _command: &str,
        _description: Descriptions<'_>,
        _usage: UsageHelper<impl Usage>,
    ) -> Result<(), Self::Err> {
        self.subcommands = true;
        Ok(())
    }

    #[inline(always)]
    fn group(&mut self, _name: &str, _usage: UsageHelper<impl Usage>) -> Result<(), Self::Err> {
        self.groups = true;
        Ok(())
    }

    #[inline]
    fn exclusive_group(
        &mut self,
        _requirement: Requirement,
        usage: UsageHelper<impl Usage>,
    ) -> Result<(), Self::Err> {
        usage.describe_deduplicated(self)
    }

    #[inline]
    fn anonymous_group(&mut self, usage: UsageHelper<impl Usage>) -> Result<(), Self::Err> {
        usage.describe_deduplicated(self)
    }
}

struct OptionPrinter<T> {
    dest: T,
}

impl<T: fmt::Write> Receiver for OptionPrinter<T> {
    type Err = fmt::Error;

    fn option(
        &mut self,
        tags: Tags<'_>,
        argument: Option<Argument<'_>>,
        requirement: Requirement,
        repetition: Repetition,
        description: Descriptions,
    ) -> Result<(), Self::Err> {
        print_long_option(
            &mut self.dest,
            &tags,
            argument.as_ref(),
            requirement,
            repetition,
            description,
        )
    }

    fn exclusive_group(
        &mut self,
        _requirement: Requirement,
        usage: UsageHelper<impl Usage>,
    ) -> Result<(), Self::Err> {
        usage.describe_deduplicated(self)
    }

    fn anonymous_group(&mut self, usage: UsageHelper<impl Usage>) -> Result<(), Self::Err> {
        usage.describe_deduplicated(self)
    }
}

struct PositionalPrinter<T> {
    dest: T,
}

impl<T: fmt::Write> Receiver for PositionalPrinter<T> {
    type Err = fmt::Error;

    fn positional(
        &mut self,
        argument: Argument<'_>,
        requirement: Requirement,
        repetition: Repetition,
        description: Descriptions,
    ) -> Result<(), Self::Err> {
        print_long_positional(
            &mut self.dest,
            &argument,
            requirement,
            repetition,
            description,
        )
    }

    fn anonymous_group(&mut self, usage: UsageHelper<impl Usage>) -> Result<(), Self::Err> {
        usage.describe_deduplicated(self)
    }

    // Skip exclusive group, since an exclusive group that includes
    // positionals isn't really sensible (and isn't supported by the
    // derive macros). Subcommands can contain positionals, but
    // subcommand help is printed separately.
}

struct CommandPrinter<T> {
    dest: T,
}

impl<T: fmt::Write> Receiver for CommandPrinter<T> {
    type Err = fmt::Error;

    fn subcommand(
        &mut self,
        command: &str,
        description: Descriptions<'_>,
        _usage: UsageHelper<impl Usage>,
    ) -> Result<(), Self::Err> {
        print_command_description(&mut self.dest, command, description)
    }

    fn anonymous_group(&mut self, usage: UsageHelper<impl Usage>) -> Result<(), Self::Err> {
        usage.describe_deduplicated(self)
    }

    fn exclusive_group(
        &mut self,
        _requirement: Requirement,
        usage: UsageHelper<impl Usage>,
    ) -> Result<(), Self::Err> {
        usage.describe_deduplicated(self)
    }
}

struct GroupPrinter<T> {
    dest: T,
}

impl<T: fmt::Write> Receiver for GroupPrinter<T> {
    type Err = fmt::Error;

    fn group(&mut self, name: &str, usage: UsageHelper<impl Usage>) -> Result<(), Self::Err> {
        print_group(&mut self.dest, name, usage)
    }
}

struct GroupContentsPrinter<T> {
    dest: T,
}

impl<T: fmt::Write> Receiver for GroupContentsPrinter<T> {
    type Err = fmt::Error;

    fn option(
        &mut self,
        tags: Tags<'_>,
        argument: Option<Argument<'_>>,
        requirement: Requirement,
        repetition: Repetition,
        description: Descriptions,
    ) -> Result<(), Self::Err> {
        print_long_option(
            &mut self.dest,
            &tags,
            argument.as_ref(),
            requirement,
            repetition,
            description,
        )
    }

    fn positional(
        &mut self,
        argument: Argument<'_>,
        requirement: Requirement,
        repetition: Repetition,
        description: Descriptions,
    ) -> Result<(), Self::Err> {
        print_long_positional(
            &mut self.dest,
            &argument,
            requirement,
            repetition,
            description,
        )
    }

    fn subcommand(
        &mut self,
        command: &str,
        description: Descriptions<'_>,
        _usage: UsageHelper<impl Usage>,
    ) -> Result<(), Self::Err> {
        print_command_description(&mut self.dest, command, description)
    }

    fn anonymous_group(&mut self, usage: UsageHelper<impl Usage>) -> Result<(), Self::Err> {
        usage.describe_deduplicated(self)
    }

    fn exclusive_group(
        &mut self,
        _requirement: Requirement,
        usage: UsageHelper<impl Usage>,
    ) -> Result<(), Self::Err> {
        usage.describe_deduplicated(self)
    }

    fn group(&mut self, name: &str, usage: UsageHelper<impl Usage>) -> Result<(), Self::Err> {
        print_group(&mut self.dest, name, usage)
    }
}
