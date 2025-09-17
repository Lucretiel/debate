use core::slice;
use std::{
    fmt::Display,
    io::{self, Write as _},
};

use indent_write::io::IndentWriter;
use lazy_format::lazy_format;

use crate::{
    errors::{BuildError, FieldKind, ParameterError, ParameterSource, StateError},
    help::{
        Description, HelpRequest, Parameter, ParameterOption, ParameterPositional,
        ParameterSubgroup, Repetition, Requirement, Tags, UsageItems, ValueParameter,
    },
};

fn printable_byte(byte: &u8) -> &str {
    let bytes = slice::from_ref(byte);
    match str::from_utf8(bytes) {
        Ok(s) => s,
        Err(_) => "??",
    }
}

/// Turn a parameter source into a "noun" that can be used in the error
/// messages
pub fn printable_source(source: &ParameterSource) -> impl Display {
    lazy_format! {
        match (*source) {
            ParameterSource::Positional { arg } => ("positional argument {arg}", arg = arg.as_str()),
            ParameterSource::Short { option } => ("option -{option}", option = printable_byte(&option)),
            ParameterSource::Long { option, .. } => ("option --{option}", option = option.as_str()),
        }
    }
}

/// Get the "word" contained within the source. Usually `printable_source` is
/// preferable, but this has its uses.
pub fn source_word<'a>(source: &'a ParameterSource<'_>) -> &'a str {
    match *source {
        ParameterSource::Positional { arg } => arg.as_str(),
        ParameterSource::Short { ref option } => printable_byte(option),
        ParameterSource::Long { option, .. } => option.as_str(),
    }
}

pub fn parameter_error(source: &ParameterSource, error: &ParameterError) -> impl Display {
    lazy_format! {
        match (error) {
            ParameterError::NeedArgument => (
                "{source} requires an argument",
                source = printable_source(source)
            ),
            ParameterError::FlagGotArgument(arg) => (
                "{source} doesn't take an argument (got {arg:?})",
                source = printable_source(source),
            ),
            // TODO: typically this message means that it appeared more than once,
            // which is common enough that we should find a way to detect it and
            // print a specific message for it.
            ParameterError::GotAdditionalInstance => (
                "{source} appeared too many times",
                source = printable_source(source)
            ),
            // TODO: these error messages are not ideal with positional
            // arguments (argument is duplicated)
            ParameterError::ParseError { message, arg } => (
                "{source}: failed to parse {arg:?}: {message}",
                source = printable_source(source)
            ),
            // TODO: show the possibility set (potentially only if it's small)
            ParameterError::ShouldBe { arg, .. } => (
                "{source}: {arg:?} wasn't an expected possible value",
                source = printable_source(source)
            ),
            ParameterError::Custom { message } => (
                "{source}: {message}",
                source = printable_source(source)
            ),
        }
    }
}

pub fn state_error(source: &ParameterSource, error: &StateError<'_>) -> impl Display {
    lazy_format! {
        match (error) {
            StateError::Parameter { error, .. } => ("{}", parameter_error(source, error)),
            StateError::Unrecognized => (
                "unrecognized {source}",
                source = printable_source(source)
            ),
            StateError::UnknownSubcommand { .. } => (
                "unrecognized subcommand {source:?}",
                source = source_word(source)
            ),
            StateError::WrongSubcommand { subcommand, .. } => (
                "{source} is not valid for subcommand {subcommand:?}",
                source = printable_source(source),
            ),
            StateError::HelpRequested(..) => "usage message was requested",
        }
    }
}

pub fn build_error(error: &BuildError) -> impl Display {
    lazy_format! {
        match (error) {
            BuildError::Arg { source, error } => ("{}", state_error(source, error)),
            BuildError::RequiredSubcommand { .. } => "no subcommand given",
            BuildError::RequiredFieldAbsent { kind: FieldKind::Long(long), .. } =>
                "required option --{long} was omitted",
            BuildError::RequiredFieldAbsent { kind: FieldKind::Short(short), .. } =>
                "required option -{short} was omitted",
            BuildError::RequiredFieldAbsent { kind:  FieldKind::Positional { placeholder }, .. } =>"
                required argument <{placeholder}> was omitted",
            BuildError::Custom(message) => "{message}",
        }
    }
}

fn any_options(items: &UsageItems<'_>) -> bool {
    match *items {
        UsageItems::Parameters { parameters } => {
            parameters.iter().any(|parameter| match parameter {
                Parameter::Option(option) => option.requirement == Requirement::Optional,
                Parameter::Positional(_) => false,
                Parameter::Group(group) => any_options(&group.contents),
            })
        }
        // Subcommands might have options, but their usage messages are printed
        // separately.
        UsageItems::Subcommands { .. } => todo!(),
        // For now we'll say yes, but definitely will revisit later when we
        // decide how to print synopses for exclusive flag sets
        UsageItems::Exclusive {
            all_flags: all_options,
            ..
        } => !all_options.is_empty(),
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
  -f, --foo <ARG>
      --help
  -g

GROUP:
  etc

 */
pub fn print_help<'a>(
    out: &mut impl io::Write,
    command: &str,
    subcommand: impl IntoIterator<Item = &'a str>,
    description: &Description<'a>,
    items: &UsageItems<'a>,
    style: HelpRequest,
) -> io::Result<()> {
    let description = description.get(style);
    writeln!(out, "{description}")?;

    section(out, "Synopsis", |mut out| {
        // TODO: [OPTIONS] is a lie, we should only print it if there's
        // at least one optional flag.
        // In particular, we know that there are definitely no options
        // if items is a subcommand set.
        write!(out, "{command}")?;

        subcommand
            .into_iter()
            .try_for_each(|subcommand| write!(out, " {subcommand}"))?;

        if any_options(items) {
            write!(out, " [OPTIONS]")?;
        }

        print_synopsis(&mut out, "COMMAND", items)?;
        writeln!(out)
    })?;

    match *items {
        UsageItems::Parameters { parameters } => {
            let positionals = parameters
                .iter()
                .filter_map(|parameter| parameter.as_positional());
            let options = parameters
                .iter()
                .filter_map(|parameter| parameter.as_option());
            let groups = parameters
                .iter()
                .filter_map(|parameter| parameter.as_subgroup());

            // subcommands, then arguments, then options, then groups
            groups
                .clone()
                .filter(|group| matches!(group.contents, UsageItems::Subcommands { .. }))
                .try_for_each(|group| print_parameter_subgroup(out, style, group))?;

            maybe_section(out, "Arguments", positionals, |out, positional| {
                print_parameter_positional(out, style, positional)
            })?;

            maybe_section(out, "Options", options, |out, option| {
                print_parameter_option(out, style, option)
            })?;

            groups
                .filter(|group| !matches!(group.contents, UsageItems::Subcommands { .. }))
                // TODO: various edge cases here related to anonymous groups.
                // Hopefully that isn't an issue for a while.
                .try_for_each(|group| print_parameter_subgroup(out, style, group))
        }
        UsageItems::Subcommands { commands, .. } => {
            maybe_section(out, "Commands", commands, |out, command| {
                describe(out, command.command, &command.description, style)
            })
        }
        UsageItems::Exclusive {
            all_flags: all_options,
            ..
        } => maybe_section(out, "Options", all_options, |out, option| {
            print_parameter_option(out, style, option)
        }),
    }
}

fn print_usage_items(
    out: &mut (impl io::Write + ?Sized),
    style: HelpRequest,
    items: &UsageItems<'_>,
) -> io::Result<()> {
    match *items {
        UsageItems::Parameters { parameters } => {
            parameters.iter().try_for_each(|parameter| match parameter {
                Parameter::Option(option) => print_parameter_option(out, style, option),
                Parameter::Positional(positional) => {
                    print_parameter_positional(out, style, positional)
                }
                Parameter::Group(group) => print_parameter_subgroup(out, style, group),
            })
        }
        UsageItems::Subcommands { commands, .. } => commands
            .iter()
            .try_for_each(|command| describe(out, command.command, &command.description, style)),
        UsageItems::Exclusive { all_flags, .. } => all_flags
            .iter()
            .try_for_each(|flag| print_parameter_option(out, style, flag)),
    }
}

fn print_parameter_option(
    out: &mut (impl io::Write + ?Sized),
    style: HelpRequest,
    option: &ParameterOption<'_>,
) -> io::Result<()> {
    // TODO: find a way to express the repetition and
    // requirement for an option
    let tags = lazy_format!(match (option.tags) {
        Tags::Short { short } => "-{short}",
        Tags::Long { long } => "    --{long}",
        Tags::LongShort { short, long } => "-{short}, --{long}",
    });

    let tags = lazy_format!(match (option.argument) {
        None => "{tags}",
        Some(ValueParameter { placeholder, .. }) => "{tags} <{placeholder}>",
    });

    describe(out, tags, &option.description, style)
}

fn print_parameter_positional(
    out: &mut (impl io::Write + ?Sized),
    style: HelpRequest,
    positional: &ParameterPositional<'_>,
) -> io::Result<()> {
    let placeholder = positional.argument.placeholder;

    let name = lazy_format!(match ((positional.requirement, positional.repetition)) {
        (Requirement::Mandatory, Repetition::Single) => "<{placeholder}>",
        (Requirement::Mandatory, Repetition::Multiple) => "<{placeholder}>...",
        (Requirement::Optional, Repetition::Single) => "[{placeholder}]",
        (Requirement::Optional, Repetition::Multiple) => "[{placeholder}...]",
    });

    describe(out, name, &positional.description, style)
}

fn print_parameter_subgroup(
    out: &mut (impl io::Write + ?Sized),
    style: HelpRequest,
    subgroup: &ParameterSubgroup<'_>,
) -> io::Result<()> {
    // TODO: include a description for the section? That would be a neat
    // trick that other libraries don't really do.
    section(out, subgroup.title, |mut out| {
        // This is necessary to avoid an unbounded recursion of
        // nested types
        let out: &mut dyn io::Write = &mut out;
        print_usage_items(out, style, &subgroup.contents)
    })
}

fn printable_option_synopsis(option: &ParameterOption<'_>) -> impl Display {
    let tag = lazy_format!(match (option.tags) {
        Tags::LongShort { long, .. } | Tags::Long { long } => "--{long}",
        Tags::Short { short } => "-{short}",
    });

    let tag = lazy_format!(match (option.argument) {
        None => "{tag}",
        Some(ValueParameter { placeholder, .. }) => "{tag} <{placeholder}>",
    });

    lazy_format!(match (option.repetition) {
        Repetition::Single => "{tag}",
        Repetition::Multiple => "<{tag}>...",
    })
}

fn print_synopsis(
    out: &mut (impl io::Write + ?Sized),
    subcommand_placeholder: &str,
    items: &UsageItems<'_>,
) -> io::Result<()> {
    match *items {
        UsageItems::Parameters { parameters } => {
            parameters.iter().try_for_each(|parameter| match parameter {
                Parameter::Option(option) => {
                    if option.requirement == Requirement::Optional {
                        return Ok(());
                    }

                    write!(out, "{}", printable_option_synopsis(option))
                }
                Parameter::Positional(positional) => {
                    let placeholder = positional.argument.placeholder;

                    let name =
                        lazy_format!(match ((positional.requirement, positional.repetition)) {
                            (Requirement::Mandatory, Repetition::Single) => "<{placeholder}>",
                            (Requirement::Mandatory, Repetition::Multiple) => "<{placeholder}>...",
                            (Requirement::Optional, Repetition::Single) => "[{placeholder}]",
                            (Requirement::Optional, Repetition::Multiple) => "[{placeholder}...]",
                        });

                    write!(out, " {name}")
                }
                Parameter::Group(group) => print_synopsis(out, group.placeholder, &group.contents),
            })
        }
        UsageItems::Subcommands { requirement, .. } => match requirement {
            Requirement::Optional => write!(out, " [{subcommand_placeholder}]"),
            Requirement::Mandatory => write!(out, " <{subcommand_placeholder}>"),
        },
        // Write out groups, but include only options that are mandatory in the
        // sets.
        UsageItems::Exclusive {
            groups,
            requirement,
            ..
        } => match requirement {
            Requirement::Optional => Ok(()),
            Requirement::Mandatory => {
                let mut groups = groups.iter().filter_map(|group| {
                    let mut flags = group
                        .iter()
                        .filter(|flag| matches!(flag.requirement, Requirement::Mandatory))
                        .map(|flag| printable_option_synopsis(flag));

                    let first = flags.next()?;
                    let tail = lazy_format!(" {flag}" for flag in flags.clone());
                    Some(lazy_format!("{first}{tail}"))
                });

                match groups.next() {
                    None => Ok(()),
                    Some(group) => {
                        let tail = lazy_format!(" | {group}" for group in groups.clone());
                        write!(out, " {{{group}{tail}}}")
                    }
                }
            }
        },
    }
}

/// Write a section by writing a newline, then the `header`, then an
/// indented `body`.
///
/// TODO: Only top-level sections should get the bonus leading newline.
/// Consider removing it from `section` and doing it only in `print_help`.
fn section<O: io::Write + ?Sized, T>(
    out: &mut O,
    header: &str,
    body: impl FnOnce(IndentWriter<&mut O>) -> io::Result<T>,
) -> io::Result<T> {
    writeln!(out, "\n{header}:")?;
    let value = body(IndentWriter::new("  ", out))?;

    Ok(value)
}

/// Write an optional section, only if the iterator is not empty.
/// Otherwise identical to `section`.
fn maybe_section<O: io::Write + ?Sized, I: IntoIterator>(
    out: &mut O,
    header: &str,
    items: I,
    body: impl Fn(&mut IndentWriter<&mut O>, I::Item) -> io::Result<()>,
) -> io::Result<()> {
    let mut items = items.into_iter();

    match items.next() {
        None => Ok(()),
        Some(first) => section(out, header, |mut out| {
            body(&mut out, first)?;
            items.try_for_each(|item| body(&mut out, item))
        }),
    }
}

/// Describe an item by printing the item, followed by the indented
/// description. Doesn't yet support short help; need to find a solution
/// for aligned indentation in that case.
///
/// If the description is and item are both short enough, the entire
/// thing is instead printed on a single line.
fn describe(
    out: &mut (impl io::Write + ?Sized),
    item: impl Display,
    description: &Description<'_>,
    style: HelpRequest,
) -> io::Result<()> {
    /// Helper type that counts the bytes that flow through it. We use this
    /// to detect if we need any newlines here in `describe`
    struct IoByteCounter<'a, T: io::Write + ?Sized> {
        inner: &'a mut T,
        count: &'a mut usize,
    }

    impl<'a, T: io::Write + ?Sized> IoByteCounter<'a, T> {
        pub fn new(inner: &'a mut T, count: &'a mut usize) -> Self {
            Self { inner, count }
        }
    }

    impl<'a, T: io::Write + ?Sized> io::Write for IoByteCounter<'a, T> {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            let n = self.inner.write(buf)?;
            *self.count += n;
            Ok(n)
        }

        fn flush(&mut self) -> io::Result<()> {
            self.inner.flush()
        }
    }

    let mut count = 0;

    {
        let mut out = IoByteCounter::new(&mut *out, &mut count);
        write!(out, "{item}")?;
    }

    let description = description.get(style);
    let indent = "        ";

    // For the record, this kind of significant conditional logic is
    // exactly the sort of thing I really *don't* like doing in libraries
    // like this, where we could instead do so much of this analysis during
    // codegen and emit unconditional prints.
    //
    // The dream, of course, is that `#[derive(Usage)]` ends up almost
    // producing a single string literal (or, like, a single format string
    // that just can be fed formatting parameters). I may yet rewrite a
    // lot of this in a future debate v2.

    if description.is_empty() {
        writeln!(out)
    } else if let Some(space) = indent.len().checked_sub(count)
        && space >= 2
        && !description.contains('\n')
    {
        (0..space).try_for_each(|_| out.write_all(b" "))?;
        writeln!(out, "{description}")
    } else {
        let mut out = IndentWriter::new("        ", out);

        writeln!(out, "\n{description}")?;
        if description.contains('\n') || matches!(style, HelpRequest::Full) {
            writeln!(out)?;
        }

        Ok(())
    }
}
