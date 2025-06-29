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
//     let mut presence = Pres ence::default();

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

#[cfg(feature = "std")]
mod with_std {
    use std::{
        fmt::Display,
        io::{self, Write as _},
    };

    use indent_write::io::IndentWriter;
    use lazy_format::lazy_format;

    use crate::{
        errors::{BuildError, FieldKind, ParameterError, ParameterSource, StateError},
        help::{Parameter, Subcommand, UsageItems},
    };

    pub fn printable_source(source: &ParameterSource) -> impl Display {
        // TODO: the `long` case is current quoted and shouldn't be
        lazy_format! {
            match (*source) {
                ParameterSource::Positional { arg } => "argument {arg:?}",
                ParameterSource::Short { option } => "option -{option}",
                ParameterSource::Long { option, .. } => "option --{option:?}",
            }
        }
    }

    pub fn write_error(out: &mut impl io::Write, error: &BuildError) -> io::Result<()> {
        match error {
            BuildError::Arg { source, error } => match error {
                StateError::Parameter { error, .. } => match error {
                    ParameterError::NeedArgument => write!(
                        out,
                        "{source} requires an argument",
                        source = printable_source(source)
                    ),
                    ParameterError::FlagGotArgument(arg) => write!(
                        out,
                        "{source} is doesn't take an argument (got {arg:?})",
                        source = printable_source(source),
                    ),
                    // TODO: this message assumes that, if this error occurs,
                    // the argument is supposed to appear at most once.``
                    ParameterError::GotAdditionalInstance => write!(
                        out,
                        "{source} appeared more than once",
                        source = printable_source(source)
                    ),
                    ParameterError::ParseError { message, .. } => write!(
                        out,
                        "{source}: parse error: {message}",
                        source = printable_source(source)
                    ),
                    ParameterError::Custom { message } => write!(
                        out,
                        "{source}: {message}",
                        source = printable_source(source)
                    ),
                },
                StateError::Unrecognized => write!(
                    out,
                    "unrecognized {source}",
                    source = printable_source(source)
                ),
                StateError::UnknownSubcommand { .. } => {
                    write!(out, "unrecognized subcommand <TODO: SOURCE>")
                }
                StateError::WrongSubcommand { subcommand, .. } => write!(
                    out,
                    "{source} is not valid for subcommand {subcommand:?}",
                    source = printable_source(source),
                ),
                StateError::HelpRequested(..) => write!(out, "usage message was requested"),
            },
            BuildError::RequiredSubcommand { .. } => write!(out, "no subcommand given"),
            BuildError::RequiredFieldAbsent { kind, .. } => match kind {
                FieldKind::Long(long) => write! { out, "required option --{long} was omitted" },
                FieldKind::Short(short) => write! {out, "required option -{short} was omitted" },
                FieldKind::Positional(placeholder) => {
                    write! { out, "required argument <{placeholder}> was omitted"}
                }
            },
            BuildError::Custom(message) => write!(out, "{message}"),
        }
    }

    fn discover_subcommand_usage<'a>(
        command: &str,
        items: &'a UsageItems<'a>,
    ) -> Option<&'a Subcommand<'a>> {
        match items {
            UsageItems::Parameters { parameters } => parameters
                .iter()
                .filter_map(|parameter| match parameter {
                    Parameter::Group { contents, .. } => Some(contents),
                    _ => None,
                })
                .find_map(|group| discover_subcommand_usage(command, group)),
            UsageItems::Subcommands { commands, .. } => {
                commands.iter().find(|usage| usage.command == command)
            }
            UsageItems::Exclusive { .. } => None,
        }
    }

    /*
    Overall structure:

    DESCRIPTION

    USAGE:
      command [OPTIONS] <ARG>

    ARGUMENTS:
      <ARG>

    OPTIONS:
      -f, --foo <ARG>  asd
          --help  asd
      -g          asd

    GROUP:
      etc

     */
    pub fn print_help(
        out: &mut impl io::Write,
        command: &str,
        description: &str,
        items: &UsageItems<'_>,
    ) -> io::Result<()> {
        let description = description.trim_end();
        write!(out, "{description}\n\n")?;

        section(out, "Usage", |mut out| {
            writeln!(out, "{command} [OPTIONS] [ARGUMENTS]")
        })?;
        section(out, "Arguments", |_out| Ok(()))?;
        section(out, "Options", |_out| Ok(()))?;

        Ok(())
    }

    fn section<O: io::Write, T>(
        out: &mut O,
        header: &str,
        body: impl FnOnce(IndentWriter<&mut O>) -> io::Result<T>,
    ) -> io::Result<T> {
        writeln!(out, "{header}:")?;
        let value = body(IndentWriter::new("  ", out))?;
        writeln!(out)?;

        Ok(value)
    }
}

pub use with_std::*;
