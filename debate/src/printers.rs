#[cfg(feature = "std")]
mod with_std {
    use core::ops::ControlFlow;
    use std::{
        fmt::Display,
        io::{self, Write as _},
    };

    use indent_write::io::IndentWriter;
    use lazy_format::lazy_format;

    use crate::{
        errors::{BuildError, FieldKind, ParameterError, ParameterSource, StateError},
        help::{Parameter, Repetition, Requirement, Subcommand, UsageItems},
        printers::visitors,
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

    pub fn write_parameter_error(
        out: &mut impl io::Write,
        source: &ParameterSource,
        error: &ParameterError,
    ) -> io::Result<()> {
        match error {
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
            // the argument is supposed to appear at most once. Might be
            // worth including information here about the maximum permitted
            // instances.
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
        }
    }

    pub fn write_state_error(
        out: &mut impl io::Write,
        source: &ParameterSource,
        error: &StateError<'_>,
    ) -> io::Result<()> {
        match error {
            StateError::Parameter { error, .. } => write_parameter_error(out, source, error),
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
            StateError::Flattened { error, .. } => write_state_error(out, source, &**error),
            StateError::HelpRequested(..) => write!(out, "usage message was requested"),
        }
    }

    pub fn write_build_error(out: &mut impl io::Write, error: &BuildError) -> io::Result<()> {
        match error {
            BuildError::Arg { source, error } => write_state_error(out, source, error),
            BuildError::RequiredSubcommand { .. } => write!(out, "no subcommand given"),
            BuildError::RequiredFieldAbsent { kind, .. } => match kind {
                FieldKind::Long(long) => write! { out, "required option --{long} was omitted" },
                FieldKind::Short(short) => write! {out, "required option -{short} was omitted" },
                FieldKind::Positional(placeholder) => {
                    write! { out, "required argument <{placeholder}> was omitted"}
                }
            },
            BuildError::Flattened { error, .. } => write_build_error(out, &**error),
            BuildError::Custom(message) => write!(out, "{message}"),
        }
    }

    // fn discover_subcommand_usage<'a>(
    //     command: &str,
    //     items: &'a UsageItems<'a>,
    // ) -> Option<&'a Subcommand<'a>> {
    //     match items {
    //         UsageItems::Parameters { parameters } => parameters
    //             .iter()
    //             .filter_map(|parameter| match parameter {
    //                 Parameter::Group { contents, .. } => Some(contents),
    //                 _ => None,
    //             })
    //             .find_map(|group| discover_subcommand_usage(command, group)),
    //         UsageItems::Subcommands { commands, .. } => {
    //             commands.iter().find(|usage| usage.command == command)
    //         }
    //         UsageItems::Exclusive { .. } => None,
    //     }
    // }

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

        match items {
            UsageItems::Parameters { parameters } => todo!(),
            UsageItems::Subcommands { requirement, commands } => todo!(),
            UsageItems::Exclusive { groups, options } => todo!(),
        }

        section(out, "Usage", |mut out| {
            writeln!(out, "{command} [OPTIONS] [ARGUMENTS]")
        })?;

        let has_positionals = visitors::visit_positionals(items, &mut |_arg| Err(())).is_err();

        if has_positionals {
            section(out, "Arguments", |mut out| {
                visitors::visit_positionals(items, &mut |positional| {
                    let name = positional.argument.placeholder;

                    let name = lazy_format! {match ((positional.requirement, positional.repetition)) {
                        (Requirement::Optional, Repetition::Single) => "[{name}]",
                        (Requirement::Mandatory, Repetition::Single) => "<{name}>",
                        (Requirement::Optional, Repetition::Multiple) => "[{name}...]",
                        (Requirement::Mandatory, Repetition::Multiple) => "<{name}>...",
                    }};

                    writeln!(out, "{name}")?;

                    if !positional.description.long.is_empty() {
                        let mut out = IndentWriter::new("  ", &mut out);
                        writeln!(out, "{}", positional.description.long)?;
                    }

                    Ok(())
                })
            })?;
        }

        let has_options = visitors::visit_options(items, &mut |_opt| Err(())).is_err();

        if has_options {
            section(out, "Options", |mut out| {
                visitors::visit_options(items, &mut |option| {
                    match option.tags {
                        crate::help::Tags::Long { long } => write!(out, "    --{long}"),
                        crate::help::Tags::Short { short } => write!(out, "-{short}"),
                        crate::help::Tags::LongShort { long, short } => {
                            write!(out, "-{short}, --{long}")
                        }
                    }?;

                    if let Some(arg) = option.argument {
                        write!(out, " <{placeholder}>", placeholder = arg.placeholder)?
                    }

                    writeln!(out)?;

                    if !option.description.long.is_empty() {
                        let mut out = IndentWriter::new("  ", &mut out);
                        writeln!(out, "{}", option.description.long)?;
                    }

                    Ok(())
                })
            })?;
        }

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

mod visitors {
    use core::ops::ControlFlow;

    use crate::help::{
        Parameter, ParameterOption, ParameterPositional, ParameterSubgroup, UsageItems,
    };

    pub fn visit_positionals<'a, 'i, E>(
        items: &'a UsageItems<'i>,
        visitor: &mut impl FnMut(&'a ParameterPositional<'i>) -> Result<(), E>,
    ) -> Result<(), E> {
        match items {
            UsageItems::Parameters { parameters } => {
                parameters.iter().try_for_each(|parameter| match parameter {
                    Parameter::Positional(arg) => visitor(arg),
                    Parameter::Group(group) if group.name.is_none() => {
                        visit_positionals(&group.contents, visitor)
                    }
                    Parameter::Option(_) | Parameter::Group(_) => Ok(()),
                })
            }
            UsageItems::Exclusive { .. } | UsageItems::Subcommands { .. } => Ok(()),
        }
    }

    pub fn visit_options<'a, 'i, E>(
        items: &'a UsageItems<'i>,
        visitor: &mut impl FnMut(&'a ParameterOption<'i>) -> Result<(), E>,
    ) -> Result<(), E> {
        match items {
            UsageItems::Parameters { parameters } => {
                parameters.iter().try_for_each(|parameter| match parameter {
                    Parameter::Option(opt) => visitor(opt),
                    Parameter::Group(group) if group.name.is_none() => {
                        visit_options(&group.contents, visitor)
                    }
                    Parameter::Positional(_) | Parameter::Group(_) => Ok(()),
                })
            }
            UsageItems::Subcommands { .. } => Ok(()),
            UsageItems::Exclusive { options, .. } => {
                options.iter().try_for_each(|opt| visitor(opt))
            }
        }
    }
}

pub use with_std::*;
