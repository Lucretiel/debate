// use core::{
//     convert::Infallible,
//     fmt::{self, Write, write},
//     marker::PhantomData,
// };

// use indent_write::{fmt::IndentWriter, indentable::Indentable};
// use joinery::{Joinable, JoinableIterator};
// use lazy_format::{lazy_format, make_lazy_format};

// use crate::{
//     help::{Receiver, Repetition, Requirement, Tags, Usage, UsageHelper, ValueParameter},
//     state::SubcommandChain,
// };

// pub fn print_complete_help_message(
//     dest: &mut impl fmt::Write,
//     description: &str,
//     command: &SubcommandChain<'_>,
//     usage: UsageHelper<impl Usage>,
// ) -> fmt::Result {
//     Ok(())
// }

// pub fn print_long_description(
//     dest: &mut impl fmt::Write,
//     usage: UsageHelper<impl Usage>,
// ) -> fmt::Result {
//     let mut presence = Presence::default();

//     match usage.describe_deduplicated(&mut presence) {
//         Ok(()) => {}
//     };
//     let presence = presence;

//     if presence.subcommands {
//         writeln!(dest, "Commands:")?;
//         usage.describe_deduplicated(&mut CommandPrinter {
//             dest: IndentWriter::new("  ", &mut *dest),
//         })?;
//     }

//     if presence.options {
//         writeln!(dest, "Options:")?;
//         usage.describe_deduplicated(&mut OptionPrinter {
//             dest: IndentWriter::new("  ", &mut *dest),
//         })?;
//     }

//     if presence.positionals {
//         writeln!(dest, "Arguments:")?;
//         usage.describe_deduplicated(&mut PositionalPrinter {
//             dest: IndentWriter::new("  ", &mut *dest),
//         })?;
//     }

//     if presence.groups {
//         usage.describe_deduplicated(&mut GroupPrinter { dest: &mut *dest })?;
//     }

//     Ok(())
// }

// fn print_long_option(
//     dest: &mut impl fmt::Write,
//     tags: &Tags<'_>,
//     argument: Option<&ValueParameter<'_>>,
//     _requirement: Requirement,
//     _repetition: Repetition,
//     description: &str,
// ) -> fmt::Result {
//     let tags = lazy_format!(match (tags) {
//         Tags::LongShort { short, long } => "-{short}, --{long}",
//         Tags::Short { short } => "-{short}",
//         Tags::Long { long } => "    --{long}",
//     });

//     let metavariable = argument.map(|arg| arg.placeholder);

//     let header = lazy_format!(match (metavariable) {
//         Some(name) => ("{tags} <{name}>"),
//         None => "{tags}",
//     });

//     let description = description.indented("        ");

//     write!(dest, "{header}\n{description}\n\n")
// }

// fn print_long_positional(
//     dest: &mut impl fmt::Write,
//     argument: &ValueParameter<'_>,
//     requirement: Requirement,
//     repetition: Repetition,
//     description: &str,
// ) -> fmt::Result {
//     let name = argument.placeholder;

//     let header = lazy_format!(match (requirement) {
//         Requirement::Mandatory => "<{name}>",
//         Requirement::Optional => "[{name}]",
//     });

//     let header = lazy_format!(match (repetition) {
//         Repetition::Single => "{header}",
//         Repetition::Multiple => "{header}...",
//     });

//     let description = description.indented("        ");

//     write!(dest, "{header}\n{description}\n\n")
// }

// fn print_command_description(
//     dest: &mut impl fmt::Write,
//     command: &str,
//     description: &str,
// ) -> fmt::Result {
//     let description = description.indented("  ");
//     write!(dest, "{command}\n{description}\n\n")
// }

// fn print_group(
//     dest: &mut impl fmt::Write,
//     name: &str,
//     usage: UsageHelper<impl Usage>,
// ) -> fmt::Result {
//     writeln!(dest, "{name}:")?;

//     usage.describe_deduplicated(&mut GroupContentsPrinter {
//         dest: IndentWriter::new("  ", dest),
//     })
// }

// #[derive(Default)]
// struct Presence {
//     subcommands: bool,
//     options: bool,
//     positionals: bool,
//     groups: bool,
// }

// impl Receiver for Presence {
//     type Err = Infallible;

//     #[inline(always)]
//     fn option(
//         &mut self,
//         _tags: Tags<'_>,
//         _argument: Option<ValueParameter<'_>>,
//         _requirement: Requirement,
//         _repetition: Repetition,
//         _description: Descriptions,
//     ) -> Result<(), Self::Err> {
//         self.options = true;
//         Ok(())
//     }

//     #[inline(always)]
//     fn positional(
//         &mut self,
//         _argument: ValueParameter<'_>,
//         _requirement: Requirement,
//         _repetition: Repetition,
//         _description: Descriptions,
//     ) -> Result<(), Self::Err> {
//         self.positionals = true;
//         Ok(())
//     }

//     #[inline(always)]
//     fn subcommand(
//         &mut self,
//         _command: &str,
//         _description: Descriptions<'_>,
//         _usage: UsageHelper<impl Usage>,
//     ) -> Result<(), Self::Err> {
//         self.subcommands = true;
//         Ok(())
//     }

//     #[inline(always)]
//     fn group(&mut self, _name: &str, _usage: UsageHelper<impl Usage>) -> Result<(), Self::Err> {
//         self.groups = true;
//         Ok(())
//     }

//     #[inline]
//     fn exclusive_group(
//         &mut self,
//         _requirement: Requirement,
//         usage: UsageHelper<impl Usage>,
//     ) -> Result<(), Self::Err> {
//         usage.describe_deduplicated(self)
//     }

//     #[inline]
//     fn anonymous_group(&mut self, usage: UsageHelper<impl Usage>) -> Result<(), Self::Err> {
//         usage.describe_deduplicated(self)
//     }
// }

// struct OptionPrinter<T> {
//     dest: T,
// }

// impl<T: fmt::Write> Receiver for OptionPrinter<T> {
//     type Err = fmt::Error;

//     fn option(
//         &mut self,
//         tags: Tags<'_>,
//         argument: Option<ValueParameter<'_>>,
//         requirement: Requirement,
//         repetition: Repetition,
//         description: Descriptions,
//     ) -> Result<(), Self::Err> {
//         print_long_option(
//             &mut self.dest,
//             &tags,
//             argument.as_ref(),
//             requirement,
//             repetition,
//             description,
//         )
//     }

//     fn exclusive_group(
//         &mut self,
//         _requirement: Requirement,
//         usage: UsageHelper<impl Usage>,
//     ) -> Result<(), Self::Err> {
//         usage.describe_deduplicated(self)
//     }

//     fn anonymous_group(&mut self, usage: UsageHelper<impl Usage>) -> Result<(), Self::Err> {
//         usage.describe_deduplicated(self)
//     }
// }

// struct PositionalPrinter<T> {
//     dest: T,
// }

// impl<T: fmt::Write> Receiver for PositionalPrinter<T> {
//     type Err = fmt::Error;

//     fn positional(
//         &mut self,
//         argument: ValueParameter<'_>,
//         requirement: Requirement,
//         repetition: Repetition,
//         description: Descriptions,
//     ) -> Result<(), Self::Err> {
//         print_long_positional(
//             &mut self.dest,
//             &argument,
//             requirement,
//             repetition,
//             description,
//         )
//     }

//     fn anonymous_group(&mut self, usage: UsageHelper<impl Usage>) -> Result<(), Self::Err> {
//         usage.describe_deduplicated(self)
//     }

//     // Skip exclusive group, since an exclusive group that includes
//     // positionals isn't really sensible (and isn't supported by the
//     // derive macros). Subcommands can contain positionals, but
//     // subcommand help is printed separately.
// }

// struct CommandPrinter<T> {
//     dest: T,
// }

// impl<T: fmt::Write> Receiver for CommandPrinter<T> {
//     type Err = fmt::Error;

//     fn subcommand(
//         &mut self,
//         command: &str,
//         description: Descriptions<'_>,
//         _usage: UsageHelper<impl Usage>,
//     ) -> Result<(), Self::Err> {
//         print_command_description(&mut self.dest, command, description)
//     }

//     fn anonymous_group(&mut self, usage: UsageHelper<impl Usage>) -> Result<(), Self::Err> {
//         usage.describe_deduplicated(self)
//     }

//     fn exclusive_group(
//         &mut self,
//         _requirement: Requirement,
//         usage: UsageHelper<impl Usage>,
//     ) -> Result<(), Self::Err> {
//         usage.describe_deduplicated(self)
//     }
// }

// struct GroupPrinter<T> {
//     dest: T,
// }

// impl<T: fmt::Write> Receiver for GroupPrinter<T> {
//     type Err = fmt::Error;

//     fn group(&mut self, name: &str, usage: UsageHelper<impl Usage>) -> Result<(), Self::Err> {
//         print_group(&mut self.dest, name, usage)
//     }
// }

// struct GroupContentsPrinter<T> {
//     dest: T,
// }

// impl<T: fmt::Write> Receiver for GroupContentsPrinter<T> {
//     type Err = fmt::Error;

//     fn option(
//         &mut self,
//         tags: Tags<'_>,
//         argument: Option<ValueParameter<'_>>,
//         requirement: Requirement,
//         repetition: Repetition,
//         description: Descriptions,
//     ) -> Result<(), Self::Err> {
//         print_long_option(
//             &mut self.dest,
//             &tags,
//             argument.as_ref(),
//             requirement,
//             repetition,
//             description,
//         )
//     }

//     fn positional(
//         &mut self,
//         argument: ValueParameter<'_>,
//         requirement: Requirement,
//         repetition: Repetition,
//         description: Descriptions,
//     ) -> Result<(), Self::Err> {
//         print_long_positional(
//             &mut self.dest,
//             &argument,
//             requirement,
//             repetition,
//             description,
//         )
//     }

//     fn subcommand(
//         &mut self,
//         command: &str,
//         description: Descriptions<'_>,
//         _usage: UsageHelper<impl Usage>,
//     ) -> Result<(), Self::Err> {
//         print_command_description(&mut self.dest, command, description)
//     }

//     fn anonymous_group(&mut self, usage: UsageHelper<impl Usage>) -> Result<(), Self::Err> {
//         usage.describe_deduplicated(self)
//     }

//     fn exclusive_group(
//         &mut self,
//         _requirement: Requirement,
//         usage: UsageHelper<impl Usage>,
//     ) -> Result<(), Self::Err> {
//         usage.describe_deduplicated(self)
//     }

//     fn group(&mut self, name: &str, usage: UsageHelper<impl Usage>) -> Result<(), Self::Err> {
//         print_group(&mut self.dest, name, usage)
//     }
// }
